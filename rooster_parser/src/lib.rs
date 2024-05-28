#![feature(map_try_insert)]

use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use tokio::fs::File;
use tokio::io::AsyncReadExt;

mod cache;
mod lexer;
mod operators;
mod parser;
mod parser2;
mod preprocess;
mod report;

pub use report::Report;
pub use report::REPORTS;

fn error_cannot_find(filename: &str) -> ! {
    println!("Error: cannot find file '{}'.", filename);
    panic!();
}

fn error_cannot_read() -> ! {
    println!("Error: file has invalid encoding.");
    panic!();
}

// TODO: move into new module
async fn read_file(file: &mut File) -> String {
    let mut r = String::new();
    match file.read_to_string(&mut r).await {
        Ok(_) => (),
        Err(_) => error_cannot_read(),
    }
    r
}

fn parser_loader(
    logical_path: &str,
) -> Pin<Box<dyn Future<Output = Result<parser2::AbstractSyntaxTree, ()>> + Send + '_>> {
    Box::pin(async move {
        // TODO: load file only if necessary
        // TODO: convert logical to physical paths
        let mut file = match File::open(logical_path).await {
            Ok(x) => x,
            Err(_) => error_cannot_find(logical_path),
        };
        let ast = preprocess::preprocess_file(&read_file(&mut file).await).await?;
        println!("{:?}", &ast); //D
        Ok(ast)
    })
}

// The global cache storing all processed definitions
static PARSER_CACHE: cache::Cache<parser2::AbstractSyntaxTree> =
    cache::Cache::new(parser_loader as _);

async fn get_ast(key: &str) -> Result<Arc<parser2::AbstractSyntaxTree>, ()> {
    PARSER_CACHE.get(key).await
}

/// Get the annotated CoC expression corresponding to a particular logical path.
pub async fn get_expression(logical_path: &str) {
    get_ast(logical_path).await.unwrap();
}
