#![feature(map_try_insert)]

use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use tokio::fs::File;
use tokio::io::AsyncReadExt;

mod cache;
mod kernel;
mod kernel_err;
mod lexer;
mod operators;
mod parser;
mod parser2;
mod path;
mod preprocess;
mod report;
mod semantics;

pub use kernel::verify;
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
        Ok(ast)
    })
}

// The global cache storing all parsed files
static PARSER_CACHE: cache::Cache<AbstractSyntaxTree> = cache::Cache::new(parser_loader as _);

/// Get the [AbstractSyntaxTree] for a particular file.
pub async fn get_file_ast(filename: &str) -> Result<Arc<AbstractSyntaxTree>, ()> {
    // TODO: normalize filename
    PARSER_CACHE.get(filename).await
}

fn definition_loader(
    logical_path: &str,
) -> Pin<
    Box<
        dyn Future<
                Output = Result<
                    (
                        (
                            &'static AbstractSyntaxTree,
                            Option<&'static AbstractSyntaxTree>,
                        ),
                        String,
                    ),
                    (),
                >,
            > + Send
            + '_,
    >,
> {
    let logical_path_string = logical_path.to_string();
    Box::pin(async move {
        let (filename, identifier) = path::get_physical_path(&logical_path_string).await;
        let file_ast = get_file_ast(&filename).await?;
        let r = file_ast.get_definition(&identifier)?;
        // Since PARSER_CACHE is static, the Arc will never get dropped, so
        // r effectively has a 'static lifetime even though Rust won't see it.
        // Let's manually give it that lifetime.
        // REMEMBER TO DO SOMETHING ABOUT THIS if Cache ever implements eviction
        Ok((
            unsafe {
                (
                    &*(r.0 as *const AbstractSyntaxTree),
                    r.1.map(|x| &*(x as *const AbstractSyntaxTree)),
                )
            },
            filename,
        ))
    })
}

// The global cache storing all parsed top-level definitions
static DEFINITION_CACHE: cache::Cache<(
    (&AbstractSyntaxTree, Option<&AbstractSyntaxTree>),
    String,
)> = cache::Cache::new(definition_loader as _);

// Get the [AbstractSyntaxTree] for a particular top-level definition.
pub(crate) async fn get_ast(
    logical_path: &str,
) -> Result<(&'static AbstractSyntaxTree, String), ()> {
    // TODO: normalize filename
    DEFINITION_CACHE
        .get(&logical_path)
        .await
        .map(|x| (x.0 .0, x.1.clone()))
}

// Get the [AbstractSyntaxTree] for a particular top-level definition.
pub(crate) async fn get_type_ast(
    logical_path: &str,
) -> Result<(Option<&'static AbstractSyntaxTree>, String), ()> {
    // TODO: normalize filename
    DEFINITION_CACHE
        .get(&logical_path)
        .await
        .map(|x| (x.0 .1, x.1.clone()))
}
