use std::sync::Arc;
use tokio::fs::File;
use tokio::io::AsyncReadExt;

mod kernel;
mod kernel_err;
mod path;
mod semantics;

pub use kernel::verify;
pub use nacre_ast::{preprocess_file, AbstractSyntaxTree};
pub use nacre_types::report;
pub use nacre_types::report::Report;
pub use report::REPORTS;

fn error_cannot_find(filename: &str) -> ! {
    println!("Error: cannot find file '{}'.", filename);
    panic!();
}

fn file_loader(filename: &str) -> nacre_cache::LoaderFuture<'_, String> {
    Box::pin(async move {
        let mut file = match File::open(filename).await {
            Ok(x) => x,
            Err(_) => error_cannot_find(filename),
        };
        let mut r = String::new();
        match file.read_to_string(&mut r).await {
            Ok(_) => (),
            Err(_) => {
                report::send(Report {
                    is_error: true,
                    filename: filename.to_string(),
                    offset: 0,
                    message: format!("file {} cannot be read", filename),
                    note: None,
                    help: None,
                    labels: vec![],
                });
                return Err(());
            }
        }
        Ok(r)
    })
}

// The global cache storing all raw files
static FILE_CACHE: nacre_cache::Cache<String> = nacre_cache::Cache::new(file_loader as _);

/// Get the raw contents of a particular file.
pub async fn get_contents(filename: &str) -> Result<Arc<String>, ()> {
    FILE_CACHE.get(filename).await
}

fn parser_loader(filename: &str) -> nacre_cache::LoaderFuture<'_, AbstractSyntaxTree> {
    let filename_string = filename.to_string();
    Box::pin(async move {
        let file = get_contents(filename).await?;
        let ast = preprocess_file(&file, filename_string).await?;
        Ok(ast)
    })
}

// The global cache storing all parsed files
static PARSER_CACHE: nacre_cache::Cache<AbstractSyntaxTree> =
    nacre_cache::Cache::new(parser_loader as _);

/// Get the [AbstractSyntaxTree] for a particular file.
pub async fn get_file_ast(filename: &str) -> Result<Arc<AbstractSyntaxTree>, ()> {
    PARSER_CACHE.get(filename).await
}

fn definition_loader(
    logical_path: &str,
) -> nacre_cache::LoaderFuture<
    '_,
    (
        (
            &'static AbstractSyntaxTree,
            Option<&'static AbstractSyntaxTree>,
        ),
        String,
    ),
> {
    let logical_path_string = logical_path.to_string();
    Box::pin(async move {
        let (filename, identifier) = path::get_physical_path(&logical_path_string).await;
        let file_ast = get_file_ast(&filename).await?;
        let r = file_ast.get_definition(&identifier).map_err(|_| ())?;
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
static DEFINITION_CACHE: nacre_cache::Cache<(
    (&AbstractSyntaxTree, Option<&AbstractSyntaxTree>),
    String,
)> = nacre_cache::Cache::new(definition_loader as _);

// Get the [AbstractSyntaxTree] for a particular top-level definition.
pub(crate) async fn get_ast(
    logical_path: &str,
) -> Result<(&'static AbstractSyntaxTree, String), ()> {
    DEFINITION_CACHE
        .get(logical_path)
        .await
        .map(|x| (x.0 .0, x.1.clone()))
}

// Get the [AbstractSyntaxTree] for a particular top-level definition.
pub(crate) async fn get_type_ast(
    logical_path: &str,
) -> Result<(Option<&'static AbstractSyntaxTree>, String), ()> {
    DEFINITION_CACHE
        .get(logical_path)
        .await
        .map(|x| (x.0 .1, x.1.clone()))
}
