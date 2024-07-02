use crate::kernel_err::TermMeta;
use crate::*;

use once_cell::sync::Lazy;
use rooster_kernel::Environment;
use rooster_kernel::Term;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use tokio::sync::Mutex;

// A counter to assign indices to top-level definitions
static INDEX_COUNTER: AtomicUsize = AtomicUsize::new(0);

static GLOBAL_ENV: Lazy<Mutex<Environment<TermMeta>>> =
    Lazy::new(|| Environment::from_vec(vec![]).into());

pub(crate) async fn update_environment(
    env: Environment<TermMeta>,
    dependency: &str,
) -> Result<(Environment<TermMeta>, usize), ()> {
    let (def, def_type, index) = &*KERNEL_CACHE.get(dependency).await?;
    let mut env_vec = GLOBAL_ENV.lock().await.clone().into_vec();
    let l = env_vec.len();
    if *index < l {
        let r = Environment::from_vec(env_vec);
        return Ok((r, *index));
    }
    debug_assert!(*index == l);
    env_vec.push((Some(def.clone()), def_type.clone()));
    let r = Environment::from_vec(env_vec);
    *GLOBAL_ENV.lock().await = r.clone(); // TODO: this will only work in single-threaded mode
    Ok((r, *index))
}

pub type Definition = (Arc<Term<TermMeta>>, Arc<Term<TermMeta>>, usize);

fn kernel_loader(logical_path: &str) -> rooster_cache::LoaderFuture<'_, Definition> {
    Box::pin(async move {
        // First we verify the current definition
        eprintln!("Verifying {}", logical_path);
        // Start by creating an environment for the kernel
        let mut env = GLOBAL_ENV.lock().await.clone();
        // Then load the definition term and compute its type
        let (ast, filename) = get_ast(logical_path).await.unwrap();
        let definition = Arc::new(semantics::convert_to_term(ast, &filename, &mut env).await?);
        let tt_opt = match get_type_ast(logical_path).await.unwrap().0 {
            Some(ast) => Some(semantics::convert_to_term(ast, &filename, &mut env).await?),
            None => None,
        };
        // And finally call into the kernel
        let type_term = Arc::new(match tt_opt {
            Some(t) => t,
            None => definition
                .get_type(&env)
                .map_err(|e| kernel_err::report(e, &filename))?,
        });
        env.add_definition(Some(definition.clone()), type_term.clone())
            .map_err(|e| kernel_err::report(e, &filename))?;
        eprintln!("Verified {}", logical_path);
        let index = INDEX_COUNTER.fetch_add(1, Ordering::Relaxed);
        Ok((definition, type_term, index))
    })
}

// The global cache storing definition, type and index for each verified definition
static KERNEL_CACHE: rooster_cache::Cache<Definition> =
    rooster_cache::Cache::new(kernel_loader as _);

/// Parse and verify the CoC expression corresponding to a particular logical path.
pub async fn verify(logical_path: &str) -> Result<(), ()> {
    KERNEL_CACHE.get(logical_path).await?;
    Ok(())
}
