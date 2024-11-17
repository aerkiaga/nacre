use crate::kernel_err::TermMeta;
use crate::*;

use nacre_kernel::Environment;
use nacre_kernel::Term;
use nacre_kernel::TermInner;
use once_cell::sync::Lazy;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use std::sync::Arc;
use tokio::sync::Mutex;

// A counter to assign indices to top-level definitions
static INDEX_COUNTER: AtomicUsize = AtomicUsize::new(0);

static GLOBAL_ENV: Lazy<Mutex<Environment<TermMeta>>> =
    Lazy::new(|| Environment::from_vec(vec![]).into());

/// Returns a copy of the global [Environment].
pub async fn get_global_environment() -> Environment<TermMeta> {
    GLOBAL_ENV.lock().await.clone()
}

pub(crate) async fn update_environment(
    _env: Environment<TermMeta>,
    dependency: &str,
) -> Result<(Environment<TermMeta>, usize), ()> {
    let (def, def_type, index) = &*KERNEL_CACHE.get(dependency).await?;
    let mut env_vec = GLOBAL_ENV.lock().await.clone().into_vec();
    let l = env_vec.len();
    if *index < l {
        env_vec[*index] = (Some(def.clone()), def_type.clone());
    } else {
        for _ in 0..*index - l {
            env_vec.push((
                None,
                Arc::new((TermInner::Prop, &Arc::new(TermMeta::default())).into()),
            ));
        }
        env_vec.push((Some(def.clone()), def_type.clone()));
    }
    let r = {
        let mut ge = GLOBAL_ENV.lock().await;
        let mut env_vec2 = ge.clone().into_vec();
        for (n, element) in env_vec.into_iter().enumerate() {
            if n >= env_vec2.len() {
                env_vec2.push(element);
            } else if env_vec2[n].0.is_none() {
                env_vec2[n] = element;
            }
        }
        let r = Environment::from_vec(env_vec2);
        *ge = r.clone();
        r
    };
    Ok((r, *index))
}

pub type Definition = (Arc<Term<TermMeta>>, Arc<Term<TermMeta>>, usize);

fn kernel_loader(logical_path: &str) -> nacre_cache::LoaderFuture<'_, Definition> {
    Box::pin(async move {
        // First we verify the current definition
        eprintln!("Verifying {}", logical_path);
        // Start by creating an environment for the kernel
        let mut env = GLOBAL_ENV.lock().await.clone();
        // Then load the definition term and compute its type
        let (ast, filename) = get_ast(logical_path).await?;
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
static KERNEL_CACHE: nacre_cache::Cache<Definition> = nacre_cache::Cache::new(kernel_loader as _);

/// Parse and verify the CoC expression corresponding to a particular logical path.
pub async fn verify(logical_path: &str) -> Result<(), ()> {
    KERNEL_CACHE.get(logical_path).await?;
    Ok(())
}

/// Returns the index of the expression corresponding to a logical path.
///
/// Will parse and verify the expression, much like [verify].
pub async fn get_definition_index(logical_path: &str) -> Result<usize, ()> {
    let def = KERNEL_CACHE.get(logical_path).await?;
    let index = def.2;
    let mut env_lock = GLOBAL_ENV.lock().await;
    let mut env = env_lock.clone().into_vec();
    for i in env.len()..std::cmp::max(env.len(), index + 1) {
        env.push((
            None,
            Arc::new((TermInner::Prop, &Arc::new(TermMeta::default())).into()),
        ));
    }
    let def2 = (&*def).clone();
    env[index] = (Some(def2.0), def2.1);
    *env_lock = Environment::from_vec(env);
    Ok(index)
}
