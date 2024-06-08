use crate::*;

use std::sync::Arc;

pub trait Meta: Send + Sync {}

pub enum Error<T: Meta> {
    MismatchedType {
        expected: Arc<Term<T>>,
        found: Arc<Term<T>>,
        env: Environment<T>,
    },
    AppMismatchedType {
        lhs: Arc<Term<T>>,
        expected: Arc<Term<T>>,
        found: Arc<Term<T>>,
        env: Environment<T>,
        ctx: Context<T>,
    },
    NonSort {
        expr: Arc<T>,
        offending: Arc<Term<T>>,
    },
    Other,
}
