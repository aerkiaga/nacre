#![feature(map_try_insert)]

mod cache;
mod lexer;
mod operators;
mod parser;
mod parser2;
mod preprocess;
mod report;

pub use report::Report;
pub use report::REPORTS;

/// Get the annotated CoC expression corresponding to a particular logical path.
pub async fn get_expression(logical_path: &str) {
    cache::get_ast(logical_path).await;
}
