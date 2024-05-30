use crate::parser2::AbstractSyntaxTree;
use crate::*;

use async_recursion::async_recursion;
use tokio::task::JoinSet;

/// Parse and verify the CoC expression corresponding to a particular logical path.
#[async_recursion]
pub async fn verify(logical_path: &str) -> Result<(), ()> {
    println!("Verifying {}", logical_path);
    let deps = semantics::get_direct_dependencies(logical_path).await?;
    let mut join_set = JoinSet::new();
    for dep in deps.iter() {
        let dep_string = dep.to_string();
        join_set.spawn(async move { verify(&dep_string).await });
    }
    while let Some(result) = join_set.join_next().await {
        result.or(Err(()))?;
    }
    // Perform the verification here!
    let (ast, filename) = get_ast(logical_path).await.unwrap();
    println!("Verified {}", logical_path);
    Ok(())
}
