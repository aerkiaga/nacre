use crate::*;

// Returns the type of a forall x:A, B expression, where ta and tb are the types of the inner terms.
// Returns Err(()) if the term does not have a type or an error occurred.
fn combine_forall_types(
    ta: &Term,
    tb: &Term,
    env: &Environment,
    ctx: &Context,
) -> Result<Term, ()> {
    let cta = ta.convert(env, ctx)?;
    match tb.convert(env, ctx)? /* x:A is not necessary */ {
        Term::Prop => {
            if cta.is_sort() {
                Ok(Term::Prop)
            } else {
                Err(())
            }
        }
        Term::Type(j) => match cta {
            Term::Prop => Ok(Term::Type(j)), /* ? */
            Term::Type(i) => Ok(Term::Type(i.max(j))),
            _ => Err(()),
        },
        _ => Err(()),
    }
}

impl Term {
    // Returns the type of a term in an environment and context, if it exists.
    // Assumes that both the environment and context are well-formed.
    // Returns Err(()) if the term does not have a type or an error occurred.
    pub(crate) fn compute_type(&self, env: &Environment, ctx: &mut Context) -> Result<Term, ()> {
        debug_assert!(self.is_well_formed(env, ctx));
        match self {
            Term::Prop => Ok(Term::Type(0)),
            Term::Type(n) => {
                let r = n.checked_add(1).ok_or(())?;
                Ok(Term::Type(r))
            }
            Term::Global(g) => env.global_type(*g).map(|x| x.clone()).ok_or(()),
            Term::Variable(v) => ctx
                .variable_type(*v)
                .map(|x| x.make_inner_by_n(*v + 1))
                .ok_or(())?,
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
                match ta.convert(env, ctx)? {
                    Term::Forall(c, d) => {
                        let tb = b.compute_type(env, ctx)?;
                        let ctb = tb.convert(env, ctx)?;
                        let cc = c.convert(env, ctx)?;
                        if ctb == cc {
                            d.replace_variable(0, b)
                        } else {
                            Err(())
                        }
                    }
                    _ => Err(()),
                }
            }
            Term::Let(a, b) => {
                let ta = a.compute_type(env, ctx)?;
                ctx.add_inner(None, ta);
                let tb = b.compute_type(env, ctx)?;
                ctx.remove_inner();
                tb.replace_variable(0, a)
            }
        }
    }

    /// Returns the type of a top-level term.
    ///
    /// Returns `Err(())` if the term does not have a type or an error occurred.
    pub fn get_type(&self, env: &Environment) -> Result<Term, ()> {
        self.compute_type(env, &mut Context::new())
    }
}
