type GlobalRef = usize;

type VariableRef = usize;

/// A term in the Calculus of Constructions.
#[derive(Clone, PartialEq, Eq)]
pub enum Term {
    /// 𝐏.
    Prop,
    /// 𝐓ₙ.
    Type(usize),
    /// A global.
    Global(GlobalRef),
    /// A variable.
    Variable(VariableRef),
    /// ∀x:A.B.
    Forall(Box<Term>, Box<Term>),
    /// λx:A.B.
    Lambda(Box<Term>, Box<Term>),
    /// A B.
    Apply(Box<Term>, Box<Term>),
    /// A let-statement.
    Let(Box<Term>, Box<Term>),
}

impl Term {
    // Whether a term is in {𝐏, 𝐓ₙ | n in ℕ}.
    pub(crate) fn is_sort(&self) -> bool {
        match self {
            Term::Prop | Term::Type(_) => true,
            _ => false,
        }
    }

    // Adds n levels of abstraction around the term.
    // Returns Err(()) if an integer overflow occurred.
    pub(crate) fn make_inner_by_n(&self, n: usize) -> Result<Term, ()> {
        match self {
            Term::Prop => Ok(Term::Prop),
            Term::Type(n) => Ok(Term::Type(*n)),
            Term::Global(g) => Ok(Term::Global(*g)),
            Term::Variable(v) => match v.checked_add(n) {
                Some(r) => Ok(Term::Variable(r)),
                None => Err(()),
            },
            Term::Forall(a, b) => Ok(Term::Forall(
                Box::new(a.make_inner_by_n(n)?),
                Box::new(b.make_inner_by_n(n)?),
            )),
            Term::Lambda(a, b) => Ok(Term::Lambda(
                Box::new(a.make_inner_by_n(n)?),
                Box::new(b.make_inner_by_n(n)?),
            )),
            Term::Apply(a, b) => Ok(Term::Apply(
                Box::new(a.make_inner_by_n(n)?),
                Box::new(b.make_inner_by_n(n)?),
            )),
            Term::Let(a, b) => Ok(Term::Apply(
                Box::new(a.make_inner_by_n(n)?),
                Box::new(b.make_inner_by_n(n)?),
            )),
        }
    }

    // Replaces all occurrences of the v-th variable with t in a,
    // and deletes the variable, returning a new Term.
    // Returns Err(()) if an integer over-/underflow occurred.
    pub(crate) fn replace_variable(&self, v: VariableRef, t: &Term) -> Result<Term, ()> {
        match self {
            Term::Prop => Ok(Term::Prop),
            Term::Type(n) => Ok(Term::Type(*n)),
            Term::Global(g) => Ok(Term::Global(*g)),
            Term::Variable(v2) => {
                if *v2 == v {
                    t.make_inner_by_n(v)
                } else if *v2 < v {
                    Ok(Term::Variable(*v2))
                } else {
                    match v2.checked_sub(1) {
                        Some(r) => Ok(Term::Variable(r)),
                        None => Err(()),
                    }
                }
            }
            Term::Forall(a, b) => match v.checked_add(1) {
                Some(r) => Ok(Term::Forall(
                    Box::new(a.replace_variable(v, t)?),
                    Box::new(b.replace_variable(r, t)?),
                )),
                None => Err(()),
            },
            Term::Lambda(a, b) => match v.checked_add(1) {
                Some(r) => Ok(Term::Lambda(
                    Box::new(a.replace_variable(v, t)?),
                    Box::new(b.replace_variable(r, t)?),
                )),
                None => Err(()),
            },
            Term::Apply(a, b) => Ok(Term::Apply(
                Box::new(a.replace_variable(v, t)?),
                Box::new(b.replace_variable(v, t)?),
            )),
            Term::Let(a, b) => match v.checked_add(1) {
                Some(r) => Ok(Term::Let(
                    Box::new(a.replace_variable(v, t)?),
                    Box::new(b.replace_variable(r, t)?),
                )),
                None => Err(()),
            },
        }
    }

    // Replaces all occurrences of the g-th global with t in self,
    // returning a new Term.
    // Returns Err(()) if an integer overflow occurred.
    pub(crate) fn replace_global(&self, g: GlobalRef, t: &Term) -> Result<Term, ()> {
        match self {
            Term::Prop => Ok(Term::Prop),
            Term::Type(n) => Ok(Term::Type(*n)),
            Term::Global(g2) => {
                if *g2 == g {
                    Ok(t.clone())
                } else {
                    Ok(Term::Global(*g2))
                }
            }
            Term::Variable(v) => Ok(Term::Variable(*v)),
            Term::Forall(a, b) => match g.checked_add(1) {
                Some(r) => Ok(Term::Forall(
                    Box::new(a.replace_global(g, t)?),
                    Box::new(b.replace_global(r, t)?),
                )),
                None => Err(()),
            },
            Term::Lambda(a, b) => match g.checked_add(1) {
                Some(r) => Ok(Term::Lambda(
                    Box::new(a.replace_global(g, t)?),
                    Box::new(b.replace_global(r, t)?),
                )),
                None => Err(()),
            },
            Term::Apply(a, b) => Ok(Term::Apply(
                Box::new(a.replace_global(g, t)?),
                Box::new(b.replace_global(g, t)?),
            )),
            Term::Let(a, b) => match g.checked_add(1) {
                Some(r) => Ok(Term::Let(
                    Box::new(a.replace_global(g, t)?),
                    Box::new(b.replace_global(r, t)?),
                )),
                None => Err(()),
            },
        }
    }
}

// The local context of a subterm, including all local variables in order of definition, from the inside out.
pub(crate) struct Context {
    inner: Vec<(Option<Term>, Term)>,
}

impl Context {
    pub(crate) fn new() -> Context {
        Context { inner: vec![] }
    }

    pub(crate) fn contains_variable(&self, v: VariableRef) -> bool {
        v < self.inner.len()
    }

    pub(crate) fn variable_type(&self, v: VariableRef) -> Option<&Term> {
        self.inner.get(v).map(|x| &x.1)
    }

    pub(crate) fn variable_value(&self, v: VariableRef) -> Option<&Term> {
        self.inner.get(v).map(|x| x.0.as_ref())?
    }

    // Add a new variable with a definition to a context, at the innermost position.
    pub(crate) fn add_inner(&mut self, var_def: Option<Term>, var_type: Term) {
        self.inner.insert(0, (var_def, var_type));
    }

    // Removes the innermost variable from a context.
    pub(crate) fn remove_inner(&mut self) {
        self.inner.pop();
    }
}

/// The global environment that a term lives in, containing all global variables.
// TODO: make it Sync, use RwLock
pub struct Environment {
    inner: Vec<(Option<Term>, Term)>,
}

impl Environment {
    pub(crate) fn contains_global(&self, g: GlobalRef) -> bool {
        g < self.inner.len()
    }

    pub(crate) fn global_type(&self, g: GlobalRef) -> Option<&Term> {
        self.inner.get(g).map(|x| &x.1)
    }

    pub(crate) fn global_value(&self, g: GlobalRef) -> Option<&Term> {
        self.inner.get(g).map(|x| x.0.as_ref())?
    }

    /// Add a new global definition to an environment,
    /// returning `Err(())` if it could not be added.
    pub fn add_definition(
        &mut self,
        global_def: Option<Term>,
        global_type: Term,
    ) -> Result<(), ()> {
        if self.global_is_legal(global_def.as_ref(), &global_type) {
            if let Some(def) = &global_def {
                let ctdef = def.get_type(self)?.normalize(self)?;
                let cglobal_type = global_type.normalize(self)?;
                if ctdef != cglobal_type {
                    return Err(());
                }
            }
            self.inner.insert(0, (global_def, global_type));
            Ok(())
        } else {
            Err(())
        }
    }
}
