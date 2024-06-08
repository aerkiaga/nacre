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
        rhs: Arc<Term<T>>,
        found: Arc<Term<T>>,
        env: Environment<T>,
        ctx: Context<T>,
    },
    AppInvalid {
        lhs: Arc<Term<T>>,
        ltype: Arc<Term<T>>,
        rhs: Arc<Term<T>>,
    },
    NonSort {
        expr: Arc<T>,
        offending: Arc<Term<T>>,
    },
    Other,
}
