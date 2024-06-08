use crate::*;

use std::sync::Arc;

type GlobalRef = usize;

type VariableRef = usize;

pub enum TermInner<T: Meta> {
    /// 𝐏.
    Prop,
    /// 𝐓ₙ.
    Type(usize),
    /// A global.
    Global(GlobalRef),
    /// A variable.
    Variable(VariableRef),
    /// ∀x:A.B.
    Forall(Box<Term<T>>, Box<Term<T>>),
    /// λx:A.B.
    Lambda(Box<Term<T>>, Box<Term<T>>),
    /// A B.
    Apply(Box<Term<T>>, Box<Term<T>>),
    /// A let-statement.
    Let(Box<Term<T>>, Box<Term<T>>),
}

impl<T: Meta> Clone for TermInner<T> {
    fn clone(&self) -> TermInner<T> {
        match self {
            TermInner::Prop => TermInner::Prop,
            TermInner::Type(n1) => TermInner::Type(*n1),
            TermInner::Global(g1) => TermInner::Global(*g1),
            TermInner::Variable(v1) => TermInner::Variable(*v1),
            TermInner::Forall(l, r) => {
                TermInner::Forall(Box::new(*l.clone()), Box::new(*r.clone()))
            }
            TermInner::Lambda(l, r) => {
                TermInner::Lambda(Box::new(*l.clone()), Box::new(*r.clone()))
            }
            TermInner::Apply(l, r) => TermInner::Apply(Box::new(*l.clone()), Box::new(*r.clone())),
            TermInner::Let(l, r) => TermInner::Let(Box::new(*l.clone()), Box::new(*r.clone())),
        }
    }
}

impl<T: Meta> PartialEq<TermInner<T>> for TermInner<T> {
    fn eq(&self, other: &TermInner<T>) -> bool {
        match self {
            TermInner::Prop => {
                if let TermInner::Prop = other {
                    true
                } else {
                    false
                }
            }
            TermInner::Type(n1) => {
                if let TermInner::Type(n2) = other {
                    n1 == n2
                } else {
                    false
                }
            }
            TermInner::Global(g1) => {
                if let TermInner::Global(g2) = other {
                    g1 == g2
                } else {
                    false
                }
            }
            TermInner::Variable(v1) => {
                if let TermInner::Variable(v2) = other {
                    v1 == v2
                } else {
                    false
                }
            }
            TermInner::Forall(l1, r1) => {
                if let TermInner::Forall(l2, r2) = other {
                    l1 == l2 && r1 == r2
                } else {
                    false
                }
            }
            TermInner::Lambda(l1, r1) => {
                if let TermInner::Lambda(l2, r2) = other {
                    l1 == l2 && r1 == r2
                } else {
                    false
                }
            }
            TermInner::Apply(l1, r1) => {
                if let TermInner::Apply(l2, r2) = other {
                    l1 == l2 && r1 == r2
                } else {
                    false
                }
            }
            TermInner::Let(l1, r1) => {
                if let TermInner::Let(l2, r2) = other {
                    l1 == l2 && r1 == r2
                } else {
                    false
                }
            }
        }
    }
}

/// A term in the Calculus of Constructions.
pub struct Term<T: Meta> {
    pub inner: TermInner<T>,
    pub meta: Arc<T>,
}

impl<T: Meta> Clone for Term<T> {
    fn clone(&self) -> Term<T> {
        Term {
            inner: self.inner.clone(),
            meta: self.meta.clone(),
        }
    }
}

impl<T: Meta> PartialEq<Term<T>> for Term<T> {
    fn eq(&self, other: &Term<T>) -> bool {
        self.inner == other.inner
    }
}

impl<T: Meta> Eq for Term<T> {}

impl<T: Meta> From<(TermInner<T>, &Arc<T>)> for Term<T> {
    fn from(value: (TermInner<T>, &Arc<T>)) -> Term<T> {
        Term {
            inner: value.0,
            meta: value.1.clone(),
        }
    }
}

impl<T: Meta> Term<T> {
    // Whether a term is in {𝐏, 𝐓ₙ | n in ℕ}.
    pub(crate) fn is_sort(&self) -> bool {
        match self.inner {
            TermInner::Prop | TermInner::Type(_) => true,
            _ => false,
        }
    }

    // Adds n levels of abstraction around the term.
    // Returns Err(()) if an integer overflow occurred.
    pub(crate) fn make_inner_by_n_rec(
        &self,
        n: usize,
        at_least: usize,
    ) -> Result<Term<T>, Error<T>> {
        match &self.inner {
            TermInner::Prop => Ok((TermInner::Prop, &self.meta).into()),
            TermInner::Type(n) => Ok((TermInner::Type(*n), &self.meta).into()),
            TermInner::Global(g) => Ok((TermInner::Global(*g), &self.meta).into()),
            TermInner::Variable(v) => {
                if *v >= at_least {
                    match v.checked_add(n) {
                        Some(r) => Ok((TermInner::Variable(r), &self.meta).into()),
                        None => Err(Error::Other),
                    }
                } else {
                    Ok((TermInner::Variable(*v), &self.meta).into())
                }
            }
            TermInner::Forall(a, b) => Ok((
                TermInner::Forall(
                    Box::new(a.make_inner_by_n_rec(n, at_least)?),
                    Box::new(
                        b.make_inner_by_n_rec(n, at_least.checked_add(1).ok_or(Error::Other)?)?,
                    ),
                ),
                &self.meta,
            )
                .into()),
            TermInner::Lambda(a, b) => Ok((
                TermInner::Lambda(
                    Box::new(a.make_inner_by_n_rec(n, at_least)?),
                    Box::new(
                        b.make_inner_by_n_rec(n, at_least.checked_add(1).ok_or(Error::Other)?)?,
                    ),
                ),
                &self.meta,
            )
                .into()),
            TermInner::Apply(a, b) => Ok((
                TermInner::Apply(
                    Box::new(a.make_inner_by_n_rec(n, at_least)?),
                    Box::new(b.make_inner_by_n_rec(n, at_least)?),
                ),
                &self.meta,
            )
                .into()),
            TermInner::Let(a, b) => Ok((
                TermInner::Apply(
                    Box::new(a.make_inner_by_n_rec(n, at_least)?),
                    Box::new(
                        b.make_inner_by_n_rec(n, at_least.checked_add(1).ok_or(Error::Other)?)?,
                    ),
                ),
                &self.meta,
            )
                .into()),
        }
    }

    // Adds n levels of abstraction around the term.
    // Returns Err(...) if an integer overflow occurred.
    pub(crate) fn make_inner_by_n(&self, n: usize) -> Result<Term<T>, Error<T>> {
        self.make_inner_by_n_rec(n, 0)
    }

    // Replaces all occurrences of the v-th variable with t in a,
    // and deletes the variable, returning a new Term.
    // Returns Err(...) if an integer over-/underflow occurred.
    pub(crate) fn replace_variable(
        &self,
        v: VariableRef,
        t: &Term<T>,
    ) -> Result<Term<T>, Error<T>> {
        match &self.inner {
            TermInner::Prop => Ok((TermInner::Prop, &self.meta).into()),
            TermInner::Type(n) => Ok((TermInner::Type(*n), &self.meta).into()),
            TermInner::Global(g) => Ok((TermInner::Global(*g), &self.meta).into()),
            TermInner::Variable(v2) => {
                if *v2 == v {
                    t.make_inner_by_n(v)
                } else if *v2 < v {
                    Ok((TermInner::Variable(*v2), &self.meta).into())
                } else {
                    match v2.checked_sub(1) {
                        Some(r) => Ok((TermInner::Variable(r), &self.meta).into()),
                        None => Err(Error::Other),
                    }
                }
            }
            TermInner::Forall(a, b) => match v.checked_add(1) {
                Some(r) => Ok((
                    TermInner::Forall(
                        Box::new(a.replace_variable(v, t)?),
                        Box::new(b.replace_variable(r, t)?),
                    ),
                    &self.meta,
                )
                    .into()),
                None => Err(Error::Other),
            },
            TermInner::Lambda(a, b) => match v.checked_add(1) {
                Some(r) => Ok((
                    TermInner::Lambda(
                        Box::new(a.replace_variable(v, t)?),
                        Box::new(b.replace_variable(r, t)?),
                    ),
                    &self.meta,
                )
                    .into()),
                None => Err(Error::Other),
            },
            TermInner::Apply(a, b) => Ok((
                TermInner::Apply(
                    Box::new(a.replace_variable(v, t)?),
                    Box::new(b.replace_variable(v, t)?),
                ),
                &self.meta,
            )
                .into()),
            TermInner::Let(a, b) => match v.checked_add(1) {
                Some(r) => Ok((
                    TermInner::Let(
                        Box::new(a.replace_variable(v, t)?),
                        Box::new(b.replace_variable(r, t)?),
                    ),
                    &self.meta,
                )
                    .into()),
                None => Err(Error::Other),
            },
        }
    }

    // Replaces all occurrences of the g-th global with t in self,
    // returning a new Term.
    // Returns Err(...) if an integer overflow occurred.
    pub(crate) fn replace_global(&self, g: GlobalRef, t: &Term<T>) -> Result<Term<T>, Error<T>> {
        match &self.inner {
            TermInner::Prop => Ok((TermInner::Prop, &self.meta).into()),
            TermInner::Type(n) => Ok((TermInner::Type(*n), &self.meta).into()),
            TermInner::Global(g2) => {
                if *g2 == g {
                    Ok(t.clone())
                } else {
                    Ok((TermInner::Global(*g2), &self.meta).into())
                }
            }
            TermInner::Variable(v) => Ok((TermInner::Variable(*v), &self.meta).into()),
            TermInner::Forall(a, b) => match g.checked_add(1) {
                Some(r) => Ok((
                    TermInner::Forall(
                        Box::new(a.replace_global(g, t)?),
                        Box::new(b.replace_global(r, t)?),
                    ),
                    &self.meta,
                )
                    .into()),
                None => Err(Error::Other),
            },
            TermInner::Lambda(a, b) => match g.checked_add(1) {
                Some(r) => Ok((
                    TermInner::Lambda(
                        Box::new(a.replace_global(g, t)?),
                        Box::new(b.replace_global(r, t)?),
                    ),
                    &self.meta,
                )
                    .into()),
                None => Err(Error::Other),
            },
            TermInner::Apply(a, b) => Ok((
                TermInner::Apply(
                    Box::new(a.replace_global(g, t)?),
                    Box::new(b.replace_global(g, t)?),
                ),
                &self.meta,
            )
                .into()),
            TermInner::Let(a, b) => match g.checked_add(1) {
                Some(r) => Ok((
                    TermInner::Let(
                        Box::new(a.replace_global(g, t)?),
                        Box::new(b.replace_global(r, t)?),
                    ),
                    &self.meta,
                )
                    .into()),
                None => Err(Error::Other),
            },
        }
    }
}

impl<T: Meta> std::fmt::Debug for Term<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.inner {
            TermInner::Prop => write!(f, "𝐏"),
            TermInner::Type(n) => write!(f, "𝐓{}", n),
            TermInner::Global(g) => write!(f, "G{}", g),
            TermInner::Variable(v) => write!(f, "x{}", v),
            TermInner::Forall(a, b) => write!(f, "∀:{:?}.{:?}", a, b),
            TermInner::Lambda(a, b) => write!(f, "λ:{:?}.{:?}", a, b),
            TermInner::Apply(a, b) => write!(f, "({:?})({:?})", a, b),
            TermInner::Let(a, b) => write!(f, "={:?};{:?}", a, b),
        }
    }
}

// The local context of a subterm, including all local variables in order of definition, from the inside out.
pub struct Context<T: Meta> {
    inner: Vec<(Option<Term<T>>, Term<T>)>,
}

impl<T: Meta> Context<T> {
    pub fn new() -> Context<T> {
        Context { inner: vec![] }
    }

    pub(crate) fn contains_variable(&self, v: VariableRef) -> bool {
        v < self.inner.len()
    }

    pub(crate) fn variable_type(&self, v: VariableRef) -> Option<&Term<T>> {
        if !self.contains_variable(v) {
            return None;
        }
        self.inner.get(self.inner.len() - v - 1).map(|x| &x.1)
    }

    pub(crate) fn variable_value(&self, v: VariableRef) -> Option<&Term<T>> {
        if !self.contains_variable(v) {
            return None;
        }
        self.inner
            .get(self.inner.len() - v - 1)
            .map(|x| x.0.as_ref())?
    }

    // Add a new variable with a definition to a context, at the innermost position.
    pub(crate) fn add_inner(&mut self, var_def: Option<Term<T>>, var_type: Term<T>) {
        self.inner.push((var_def, var_type));
    }

    // Removes the innermost variable from a context.
    pub(crate) fn remove_inner(&mut self) {
        self.inner.pop();
    }

    pub fn delta_replacement(&self, v: VariableRef) -> Result<Term<T>, Error<T>> {
        match self.variable_value(v) {
            Some(t) => Ok(t.make_inner_by_n(v + 1)?),
            None => Err(Error::Other),
        }
    }
}

impl<T: Meta> std::fmt::Debug for Context<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        for def in &self.inner {
            if let Some(term) = &def.0 {
                write!(f, "{:?}", term)?;
            }
            write!(f, " : {:?}, ", def.1)?;
        }
        write!(f, "]")
    }
}

/// The global environment that a term lives in, containing all global variables.
pub struct Environment<T: Meta> {
    inner: Vec<(Option<Arc<Term<T>>>, Arc<Term<T>>)>,
}

impl<T: Meta> Clone for Environment<T> {
    fn clone(&self) -> Environment<T> {
        Environment {
            inner: (&self.inner)
                .into_iter()
                .map(|x| (x.0.as_ref().map(|y| y.clone()), x.1.clone()))
                .collect(),
        }
    }
}

impl<T: Meta> Environment<T> {
    /// Construct an [Environment] from a list of definitions.
    ///
    /// Each definition consists of an optional value and a mandatory type.
    /// The definitions are not checked for well-foundedness or validity.
    pub fn from_vec(list: Vec<(Option<Arc<Term<T>>>, Arc<Term<T>>)>) -> Environment<T> {
        Environment { inner: list }
    }

    pub(crate) fn contains_global(&self, g: GlobalRef) -> bool {
        g < self.inner.len()
    }

    pub(crate) fn global_type(&self, g: GlobalRef) -> Option<&Term<T>> {
        self.inner.get(g).map(|x| &*x.1)
    }

    pub(crate) fn global_value(&self, g: GlobalRef) -> Option<&Term<T>> {
        self.inner.get(g).map(|x| x.0.as_deref())?
    }

    pub fn delta_replacement(&self, g: GlobalRef) -> Result<Term<T>, Error<T>> {
        match self.global_value(g) {
            Some(t) => Ok(t.clone()),
            None => Err(Error::Other),
        }
    }

    /// Add a new global definition to an environment,
    /// returning `Err(())` if it could not be added.
    // TODO: make the type also optional (can't be both None)
    pub fn add_definition(
        &mut self,
        global_def: Option<Arc<Term<T>>>,
        global_type: Arc<Term<T>>,
    ) -> Result<(), Error<T>> {
        if self.global_is_legal(global_def.as_deref(), &global_type) {
            if let Some(def) = &global_def {
                let tdef = def.get_type(self)?;
                let ctdef = tdef.normalize(self)?;
                let cglobal_type = global_type.normalize(self)?;
                if ctdef != cglobal_type {
                    return Err(Error::MismatchedType {
                        expected: global_type.clone(),
                        found: Arc::new(tdef),
                        env: self.clone(),
                    });
                }
            }
            self.inner.insert(0, (global_def, global_type));
            Ok(())
        } else {
            Err(Error::Other)
        }
    }
}

impl<T: Meta> std::fmt::Debug for Environment<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for def in &self.inner {
            write!(f, "| ")?;
            if let Some(term) = &def.0 {
                write!(f, "{:?}", term)?;
            }
            write!(f, " : {:?}\n", def.1)?;
        }
        write!(f, "\n")
    }
}
