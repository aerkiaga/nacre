use crate::*;

// Returns the type of a forall x:A, B expression, where ta and tb are the types of the inner terms.
// Returns Err(()) if the term does not have a type or an error occurred.
fn combine_forall_types(
    ta: &Term,
    tb: &Term,
    env: &Environment,
    ctx: &mut Context,
) -> Result<Term, Error> {
    let cta = ta.normalize_in_ctx(env, ctx)?;
    match tb.normalize_in_ctx(env, ctx)? /* x:A is not necessary */ {
        Term::Prop => {
            if cta.is_sort() {
                Ok(Term::Prop)
            } else {
                Err(Error::Other)
            }
        }
        Term::Type(j) => match cta {
            Term::Prop => Ok(Term::Type(j)), /* ? */
            Term::Type(i) => Ok(Term::Type(i.max(j))),
            _ => Err(Error::Other),
        },
        _ => Err(Error::Other),
    }
}

impl Term {
    // Returns the type of a term in an environment and context, if it exists.
    // Assumes that both the environment and context are well-formed.
    // Returns Err(()) if the term does not have a type or an error occurred.
    pub(crate) fn compute_type(&self, env: &Environment, ctx: &mut Context) -> Result<Term, Error> {
        debug_assert!(self.is_well_formed(env, ctx));
        match self {
            Term::Prop => Ok(Term::Type(0)),
            Term::Type(n) => {
                let r = n.checked_add(1).ok_or(Error::Other)?;
                Ok(Term::Type(r))
            }
            Term::Global(g) => env.global_type(*g).map(|x| x.clone()).ok_or(Error::Other),
            Term::Variable(v) => ctx
                .variable_type(*v)
                .map(|x| x.make_inner_by_n(*v + 1))
                .ok_or(Error::Other)?,
            Term::Forall(a, b) => {
                let ta = a.compute_type(env, ctx)?;
                ctx.add_inner(None, *a.clone());
                let tb = b.compute_type(env, ctx)?;
                ctx.remove_inner();
                combine_forall_types(&ta, &tb, env, ctx)
            }
            Term::Lambda(a, b) => {
                let ta = a.compute_type(env, ctx)?;
                ctx.add_inner(None, *a.clone());
                let tb = b.compute_type(env, ctx)?;
                let ttb = tb.compute_type(env, ctx)?;
                ctx.remove_inner();
                combine_forall_types(&ta, &ttb, env, ctx)?;
                Ok(Term::Forall(Box::new(*a.clone()), Box::new(tb)))
            }
            Term::Apply(a, b) => {
                let ta = a.compute_type(env, ctx)?;
                match ta.normalize_in_ctx(env, ctx)? {
                    Term::Forall(c, d) => {
                        let tb = b.compute_type(env, ctx)?;
                        let ctb = tb.normalize_in_ctx(env, ctx)?;
                        let cc = c.normalize_in_ctx(env, ctx)?;
                        if ctb == cc {
                            d.replace_variable(0, b)
                        } else {
                            Err(Error::Other)
                        }
                    }
                    _ => Err(Error::Other),
                }
            }
            Term::Let(a, b) => {
                let ta = a.compute_type(env, ctx)?;
                ctx.add_inner(Some(*a.clone()), ta);
                let tb = b.compute_type(env, ctx)?;
                ctx.remove_inner();
                tb.replace_variable(0, a)
            }
        }
    }

    /// Returns the type of a top-level term.
    ///
    /// Returns `Err(())` if the term does not have a type or an error occurred.
    pub fn get_type(&self, env: &Environment) -> Result<Term, Error> {
        self.compute_type(env, &mut Context::new())
    }
}
