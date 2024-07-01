#![feature(map_try_insert)]

use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;
use tokio::sync::Notify;
use tokio::sync::RwLock;

// An entry in the parser cache
enum CacheEntry<T> {
    // Notifies tasks when the value they requested becomes available
    Pending(Arc<Notify>),
    // The (fully processed) value
    Available(Arc<T>),
}

impl<T> Clone for CacheEntry<T> {
    fn clone(&self) -> Self {
        match self {
            CacheEntry::Pending(notify) => CacheEntry::Pending(notify.clone()),
            CacheEntry::Available(value) => CacheEntry::Available(value.clone()),
        }
    }
}

pub type LoaderFuture<'a, T> = Pin<Box<dyn Future<Output = Result<T, ()>> + Send + 'a>>;

pub struct Cache<T> {
    storage: Lazy<RwLock<HashMap<String, CacheEntry<T>>>>,
    loader: fn(&str) -> LoaderFuture<'_, T>,
}

impl<T: Send + Sync> Cache<T> {
    pub const fn new(loader: fn(&str) -> LoaderFuture<'_, T>) -> Cache<T> {
        Cache {
            storage: Lazy::new(|| HashMap::new().into()),
            loader,
        }
    }

    // Make sure the value associated with a key is available.
    async fn load(&self, key: &str) -> Result<(), ()> {
        let r = (self.loader)(key).await?;
        self.storage
            .write()
            .await
            .insert(key.to_string(), CacheEntry::Available(r.into()));
        Ok(())
    }

    async fn try_get(&self, key: &str) -> Option<Arc<T>> {
        match self.storage.read().await.get(key)? {
            CacheEntry::Pending(_) => None,
            CacheEntry::Available(val) => Some(val.clone()),
        }
    }

    pub async fn get(&'static self, key: &str) -> Result<Arc<T>, ()> {
        // First, atomically check if entry exists and insert pending if not
        let option = {
            let notify = Arc::new(Notify::new());
            match self
                .storage
                .write()
                .await
                .try_insert(key.to_string(), CacheEntry::Pending(notify.clone()))
            {
                Ok(_) => Some(notify),
                Err(_) => None,
            }
        };

        // If entry didn't exist (now pending), spawn task to compute it
        if let Some(notify) = option {
            let s = key.to_string();
            let notify_sender = notify.clone();
            let result = tokio::spawn(async move {
                self.load(&s).await?;
                notify_sender.notify_waiters();
                Ok::<(), ()>(())
            })
            .await
            .unwrap();
            if result.is_err() {
                self.storage.write().await.remove(key);
                return Err(());
            }
            if self.try_get(key).await.is_none() {
                self.storage.write().await.remove(key);
                return Err(());
            }
        }

        // Wait if pending, return value if/when available
        let entry_clone = self.storage.read().await.get(key).unwrap().clone();
        match entry_clone {
            CacheEntry::Pending(notify) => {
                notify.notified().await;
                Ok(self.try_get(key).await.ok_or(())?)
            }
            CacheEntry::Available(val) => Ok(val.clone()),
        }
    }
}
