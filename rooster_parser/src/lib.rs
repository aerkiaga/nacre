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

pub use parser2::AbstractSyntaxTree;
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

fn file_loader(filename: &str) -> Pin<Box<dyn Future<Output = Result<String, ()>> + Send + '_>> {
    Box::pin(async move {
        // TODO: load file only if necessary
        // TODO: convert logical to physical paths
        let mut file = match File::open(filename).await {
            Ok(x) => x,
            Err(_) => error_cannot_find(filename),
        };
        let mut r = String::new();
        match file.read_to_string(&mut r).await {
            Ok(_) => (),
            Err(_) => error_cannot_read(),
        }
        Ok(r)
    })
}

// The global cache storing all raw files
static FILE__CACHE: cache::Cache<String> = cache::Cache::new(file_loader as _);

/// Get the raw contents of a particular file.
pub async fn get_contents(filename: &str) -> Result<Arc<String>, ()> {
    // TODO: normalize filename
    FILE__CACHE.get(filename).await
}

fn parser_loader(
    filename: &str,
) -> Pin<Box<dyn Future<Output = Result<AbstractSyntaxTree, ()>> + Send + '_>> {
    let filename_string = filename.to_string();
    Box::pin(async move {
        let file = get_contents(filename).await?;
        let ast = preprocess::preprocess_file(&file, filename_string).await?;
        println!("{:?}", &ast); //D
        Ok(ast)
    })
}

// The global cache storing all parsed files
static PARSER_CACHE: cache::Cache<AbstractSyntaxTree> = cache::Cache::new(parser_loader as _);

/// Get the [AbstractSyntaxTree] for a particular file.
pub async fn get_ast(filename: &str) -> Result<Arc<AbstractSyntaxTree>, ()> {
    // TODO: normalize filename
    PARSER_CACHE.get(filename).await
}

/// Get the annotated CoC expression corresponding to a particular logical path.
pub async fn get_expression(logical_path: &str) {
    get_ast(logical_path).await.unwrap();
}
