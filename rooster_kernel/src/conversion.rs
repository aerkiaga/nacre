use crate::*;

impl Term {
    // Applies beta and delta reduction to fully convert a term.
    // Returns Err(()) if an error occurred.
    fn convert(&self, env: &Environment, ctx: &mut Context) -> Result<(Term, bool), ()> {
        match self {
            Term::Prop => Ok((Term::Prop, false)),
            Term::Type(n) => Ok((Term::Type(*n), false)),
            Term::Global(g) => match env.global_value(*g) {
                Some(t) => Ok((t.clone(), true)), // Delta-Global
                None => Err(()),
            },
            Term::Variable(v) => match ctx.variable_value(*v) {
                Some(t) => Ok((t.make_inner_by_n(*v + 1)?, true)), // Delta-Local
                None => Ok((Term::Variable(*v), false)),
            },
            Term::Apply(a, b) => {
                let (ca, did_ca) = a.convert(env, ctx)?; // Reduce callee first
                let (cb, did_cb) = b.convert(env, ctx)?; // Innermost order
                                                         // TODO: explore outermost order
                match ca {
                    Term::Lambda(_, d) => Ok((d.replace_variable(0, &cb)?, true)), // Beta
                    _ => Ok((Term::Apply(Box::new(ca), Box::new(cb)), did_ca || did_cb)),
                }
            }
            Term::Let(a, b) => {
                let (ca, did_ca) = a.convert(env, ctx)?; // Reduce callee first
                ctx.add_inner(Some(ca.clone()), Term::Prop);
                let (cb, did_cb) = b.convert(env, ctx)?; // Innermost order
                                                         // TODO: explore outermost order
                ctx.remove_inner();
                Ok((cb.replace_variable(0, &ca)?, true)) // Zeta
            }
            Term::Forall(a, b) => {
                let (ca, did_ca) = a.convert(env, ctx)?; // Reduce left first
                ctx.add_inner(None, Term::Prop);
                let (cb, did_cb) = b.convert(env, ctx)?; // Then reduce right
                ctx.remove_inner();
                Ok((Term::Forall(Box::new(ca), Box::new(cb)), did_ca || did_cb))
            }
            Term::Lambda(a, b) => {
                let (ca, did_ca) = a.convert(env, ctx)?; // Reduce left first
                ctx.add_inner(None, Term::Prop);
                let (cb, did_cb) = b.convert(env, ctx)?; // Then reduce right
                ctx.remove_inner();
                Ok((Term::Lambda(Box::new(ca), Box::new(cb)), did_ca || did_cb))
            }
        }
    }

    // Applies successive reduction steps to fully normalize a term.
    //
    // Returns `Err(())` if an error occurred.
    pub(crate) fn normalize_in_ctx(
        &self,
        env: &Environment,
        ctx: &mut Context,
    ) -> Result<Term, ()> {
        let mut r = self.clone();
        let mut pending = true;
        while pending {
            (r, pending) = r.convert(env, ctx)?;
        }
        Ok(r)
    }

    /// Applies successive reduction steps to fully normalize a term.
    ///
    /// Returns `Err(())` if an error occurred.
    pub fn normalize(&self, env: &Environment) -> Result<Term, ()> {
        self.normalize_in_ctx(env, &mut Context::new())
    }
}
