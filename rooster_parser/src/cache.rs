use crate::*;

use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::fs::File;
use tokio::io::AsyncReadExt;
use tokio::sync::Notify;
use tokio::sync::RwLock;

// An entry in the parser cache
#[derive(Clone)]
enum CacheEntry {
    // Notifies tasks when the AST they requested becomes available
    Pending(Arc<Notify>),
    // The (fully processed) AST
    Available(Arc<parser2::AbstractSyntaxTree>),
}

// The global cache storing all processed definitions
static PARSER_CACHE: Lazy<RwLock<HashMap<String, CacheEntry>>> =
    Lazy::new(|| HashMap::new().into());

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

/// Make sure the AST associated with a logical path is available.
async fn load_ast(logical_path: &str) -> Result<(), ()> {
    // TODO: load file only if necessary
    // TODO: convert logical to physical paths
    let mut file = match File::open(logical_path).await {
        Ok(x) => x,
        Err(_) => error_cannot_find(logical_path),
    };
    // TODO: call preprocessor giving it an async stream
    let ast = preprocess::preprocess_file(&read_file(&mut file).await).await?;
    println!("{:?}", &ast);
    PARSER_CACHE
        .write()
        .await
        .insert(logical_path.to_string(), CacheEntry::Available(ast.into()));
    Ok(())
}

async fn try_get_ast(logical_path: &str) -> Option<Arc<parser2::AbstractSyntaxTree>> {
    match PARSER_CACHE.read().await.get(logical_path)? {
        CacheEntry::Pending(_) => None,
        CacheEntry::Available(ast) => Some(ast.clone()),
    }
}

pub(crate) async fn get_ast(logical_path: &str) -> Result<Arc<parser2::AbstractSyntaxTree>, ()> {
    // First, atomically check if entry exists and insert pending if not
    let option = {
        let notify = Arc::new(Notify::new());
        match PARSER_CACHE.write().await.try_insert(
            logical_path.to_string(),
            CacheEntry::Pending(notify.clone()),
        ) {
            Ok(_) => Some(notify),
            Err(_) => None,
        }
    };

    // If entry didn't exist (now pending), spawn task to compute it
    if let Some(notify) = option {
        let s = logical_path.to_string();
        let notify_sender = notify.clone();
        tokio::spawn(async move {
            load_ast(&s).await;
            notify_sender.notify_waiters();
        })
        .await
        .unwrap();
        try_get_ast(logical_path).await.ok_or(())?;
    }

    // Wait if pending, return AST if/when available
    let entry = PARSER_CACHE.read().await.get(logical_path).unwrap().clone();
    match entry {
        CacheEntry::Pending(notify) => {
            notify.notified().await;
            Ok(try_get_ast(logical_path).await.ok_or(())?)
        }
        CacheEntry::Available(ast) => Ok(ast.clone()),
    }
}
