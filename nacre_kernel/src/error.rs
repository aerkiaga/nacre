use crate::*;

use std::sync::Arc;

/// Metadata trait for a [Term].
pub trait Meta: Send + Sync {}

/// Kernel error type.
#[derive(Debug)] // TODO: more specific implementation
pub enum Error<T: Meta> {
    /// Mismatched type in definition.
    MismatchedType {
        expected: Arc<Term<T>>,
        found: Arc<Term<T>>,
        env: Environment<T>,
    },
    /// Mismatched parameter type in application.
    AppMismatchedType {
        lhs: Arc<Term<T>>,
        expected: Arc<Term<T>>,
        rhs: Arc<Term<T>>,
        found: Arc<Term<T>>,
        env: Environment<T>,
        ctx: Context<T>,
    },
    /// Invalid left operand in application.
    AppInvalid {
        lhs: Arc<Term<T>>,
        ltype: Arc<Term<T>>,
        rhs: Arc<Term<T>>,
    },
    /// Term's type is not a sort.
    NonSort {
        expr: Arc<T>,
        offending: Arc<Term<T>>,
    },
    /// Other kernel error.
    Other,
}
