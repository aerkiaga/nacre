use crate::parser2::AbstractSyntaxTree;
use crate::*;

use rooster_kernel::Term;
use std::collections::HashMap;
use std::sync::atomic::AtomicUsize;
use std::sync::atomic::Ordering;
use tokio::task::JoinSet;

// A counter to assign indices to top-level definitions
static INDEX_COUNTER: AtomicUsize = AtomicUsize::new(0);

fn kernel_loader(
    logical_path: &str,
) -> Pin<Box<dyn Future<Output = Result<(Arc<Term>, Arc<Term>, usize), ()>> + Send + '_>> {
    let logical_path_string = logical_path.to_string();
    Box::pin(async move {
        println!("Verifying {}", logical_path);
        // First we compute all dependencies, so the operation can be parallelized
        let deps = semantics::get_direct_dependencies(logical_path).await?;
        let mut join_set = JoinSet::new();
        for dep in deps.iter() {
            let dep_string = dep.to_string();
            join_set.spawn(async move { verify(&dep_string).await });
        }
        while let Some(result) = join_set.join_next().await {
            result.or(Err(()))?;
        }
        let (ast, filename) = get_ast(logical_path).await.unwrap();
        let definition =
            Arc::new(semantics::convert_to_term(ast, &HashMap::new(), 0, &filename).await?);
        //let type_term = definition.get_type()?; // need environment
        let index = INDEX_COUNTER.fetch_add(1, Ordering::Relaxed);
        println!("Verified {}", logical_path);
        Ok((definition, Arc::new(Term::Prop), index))
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
