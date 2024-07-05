use crate::*;

use async_recursion::async_recursion;
use tokio::sync::mpsc;
use tokio::sync::oneshot;

#[async_recursion]
pub(crate) async fn preprocess_chunk(
    chunk: String,
    filename: String,
    offset: usize,
) -> Result<AbstractSyntaxTree, ()> {
    // Start a concurrent task for tokenization
    let (tokenizer_sender, tokenizer_receiver) = mpsc::unbounded_channel();
    let filename_clone = filename.clone();
    let tokenizer_handle = tokio::spawn(async move {
        let _ = lexer::tokenize_chunk(&chunk, filename_clone, offset, tokenizer_sender).await;
    });
    // Start a concurrent task for operator-precedence parsing
    let (parser_sender, parser_receiver) = mpsc::unbounded_channel();
    let parser_handle = tokio::spawn(async move {
        parser::parse_stream(tokenizer_receiver, parser_sender).await;
    });
    // Start a concurrent task for operator-precedence parsing
    let (ast_sender, ast_receiver) = oneshot::channel();
    let ast_handle = tokio::spawn(async move {
        let _ = ast_sender.send(ast::build_tree(parser_receiver, filename).await);
    });
    tokenizer_handle.await.unwrap();
    parser_handle.await.unwrap();
    ast_handle.await.unwrap();
    ast_receiver.await.unwrap()
}

/// Returns the [AbstractSyntaxTree] for a file.
///
/// Must be given its source code and filename.
pub async fn preprocess_file(src: &str, filename: String) -> Result<AbstractSyntaxTree, ()> {
    preprocess_chunk(src.to_string(), filename, 0).await
}
