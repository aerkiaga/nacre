use crate::lexer::Token;
use crate::*;

use async_recursion::async_recursion;
use tokio::sync::mpsc;
use tokio::sync::oneshot;

#[async_recursion]
pub(crate) async fn preprocess_chunk(chunk: String, offset: usize) -> parser2::AbstractSyntaxTree {
    // Start a concurrent task for tokenization
    let (tokenizer_sender, mut tokenizer_receiver) = mpsc::unbounded_channel();
    let tokenizer_handle = tokio::spawn(async move {
        lexer::tokenize_chunk(&chunk, offset, tokenizer_sender).await;
    });
    // Start a concurrent task for operator-precedence parsing
    let (parser_sender, mut parser_receiver) = mpsc::unbounded_channel();
    let parser_handle = tokio::spawn(async move {
        parser::parse_stream(tokenizer_receiver, parser_sender).await;
    });
    // Start a concurrent task for operator-precedence parsing
    let (parser2_sender, mut parser2_receiver) = oneshot::channel();
    let parser2_handle =
        tokio::spawn(
            async move { parser2_sender.send(parser2::build_tree(parser_receiver).await) },
        );
    tokenizer_handle.await.unwrap();
    parser_handle.await.unwrap();
    parser2_handle.await.unwrap();
    parser2_receiver.await.unwrap()
}

pub(crate) async fn preprocess_file(src: &String) -> parser2::AbstractSyntaxTree {
    preprocess_chunk(src.clone(), 0).await
}
