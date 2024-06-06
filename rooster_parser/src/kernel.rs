use crate::parser2::AbstractSyntaxTree;
use crate::*;

use rooster_kernel::Environment;
use rooster_kernel::Term;
use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use tokio::task::yield_now;
use tokio::task::JoinSet;

// A counter to assign indices to top-level definitions
static INDEX_COUNTER: AtomicUsize = AtomicUsize::new(0);

async fn create_environment(dependencies: Arc<HashSet<String>>) -> Result<Environment, ()> {
    let mut map_index_to_def = HashMap::new();
    let mut max_index = 0;
    for dep in dependencies.iter() {
        let (def, def_type, index) = &*KERNEL__CACHE.get(dep).await?;
        map_index_to_def.insert(*index, (Some(def.clone()), def_type.clone()));
        max_index = max_index.max(*index);
    }
    let mut env_vec = vec![];
    let null = (None, Arc::new(Term::Prop));
    for index in 0..max_index + 1 {
        let def = match map_index_to_def.remove(&index) {
            Some(prev) => prev,
            None => null.clone(),
        };
        env_vec.push(def);
    }
    Ok(Environment::from_vec(env_vec))
}

fn kernel_loader(
    logical_path: &str,
) -> Pin<Box<dyn Future<Output = Result<(Arc<Term>, Arc<Term>, usize), ()>> + Send + '_>> {
    let logical_path_string = logical_path.to_string();
    Box::pin(async move {
        println!("Computing dependencies for {}", logical_path);
        // First we compute all dependencies, so the operation can be parallelized
        // TODO: move into new function that can be used by API users
        let deps = semantics::get_direct_dependencies(logical_path).await?;
        let mut join_set = JoinSet::new();
        for dep in deps.iter() {
            let dep_string = dep.to_string();
            join_set.spawn(async move { verify(&dep_string).await });
        }
        while let Some(result) = join_set.join_next().await {
            result.or(Err(()))?;
        }
        // Then we verify the current definition
        println!("Verifying {}", logical_path);
        // Start by loading the definition term
        let (ast, filename) = get_ast(logical_path).await.unwrap();
        let definition =
            Arc::new(semantics::convert_to_term(ast, &HashMap::new(), 0, &filename).await?);
        // Then create an environment for the kernel
        // TODO: add indirect dependencies
        let all_deps = semantics::get_all_dependencies(logical_path).await?;
        let mut env = create_environment(all_deps).await?;
        // And finally call into the kernel
        let type_term = Arc::new(match get_type_ast(logical_path).await.unwrap().0 {
            Some(ast) => semantics::convert_to_term(ast, &HashMap::new(), 0, &filename).await?,
            None => definition
                .get_type(&env)
                .map_err(|e| kernel_err::report(e, &filename))?,
        });
        env.add_definition(Some(definition.clone()), type_term.clone())
            .map_err(|e| kernel_err::report(e, &filename))?;
        println!("Verified {}", logical_path);
        let index = INDEX_COUNTER.fetch_add(1, Ordering::Relaxed);
        Ok((definition, type_term, index))
    })
}

// The global cache storing definition, type and index for each verified definition
static KERNEL__CACHE: cache::Cache<(Arc<Term>, Arc<Term>, usize)> =
    cache::Cache::new(kernel_loader as _);

/// Parse and verify the CoC expression corresponding to a particular logical path.
pub async fn verify(logical_path: &str) -> Result<(), ()> {
    KERNEL__CACHE.get(logical_path).await?;
    Ok(())
}

// Get the index associated to a top-level definition
pub(crate) async fn get_global_index(logical_path: &str) -> Result<usize, ()> {
    Ok(KERNEL__CACHE.get(logical_path).await?.2)
}
