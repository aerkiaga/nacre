use crate::*;

impl<T: Meta> Term<T> {
    // Applies beta and delta reduction to fully convert a term.
    // Returns Err(()) if an error occurred.
    fn convert(
        &self,
        env: &Environment<T>,
        ctx: &mut Context<T>,
    ) -> Result<(Term<T>, bool), Error<T>> {
        match &self.inner {
            TermInner::Prop => Ok(((TermInner::Prop, &self.meta).into(), false)),
            TermInner::Type(n) => Ok(((TermInner::Type(*n), &self.meta).into(), false)),
            TermInner::Global(g) => match env.global_value(*g) {
                Some(t) => Ok((t.clone(), true)), // Delta-Global
                None => Err(Error::Other),
            },
            TermInner::Variable(v) => match ctx.variable_value(*v) {
                Some(t) => Ok((t.make_inner_by_n(v + 1)?, true)), // Delta-Local
                None => Ok(((TermInner::Variable(*v), &self.meta).into(), false)),
            },
            TermInner::Apply(a, b) => {
                let (ca, did_ca) = a.convert(env, ctx)?; // Reduce callee first
                let (cb, did_cb) = b.convert(env, ctx)?; // Innermost order
                                                         // TODO: explore outermost order
                match ca.inner {
                    TermInner::Lambda(_, d) => Ok((d.replace_variable(0, &cb)?, true)), // Beta
                    _ => Ok((
                        (TermInner::Apply(Box::new(ca), Box::new(cb)), &self.meta).into(),
                        did_ca || did_cb,
                    )),
                }
            }
            TermInner::Let(a, b) => {
                let (ca, did_ca) = a.convert(env, ctx)?; // Reduce callee first
                ctx.add_inner(Some(ca.clone()), (TermInner::Prop, &self.meta).into());
                let (cb, did_cb) = b.convert(env, ctx)?; // Innermost order
                                                         // TODO: explore outermost order
                ctx.remove_inner();
                Ok((cb.replace_variable(0, &ca)?, true)) // Zeta
            }
            TermInner::Forall(a, b) => {
                let (ca, did_ca) = a.convert(env, ctx)?; // Reduce left first
                ctx.add_inner(None, (TermInner::Prop, &self.meta).into());
                let (cb, did_cb) = b.convert(env, ctx)?; // Then reduce right
                ctx.remove_inner();
                Ok((
                    (TermInner::Forall(Box::new(ca), Box::new(cb)), &self.meta).into(),
                    did_ca || did_cb,
                ))
            }
            TermInner::Lambda(a, b) => {
                let (ca, did_ca) = a.convert(env, ctx)?; // Reduce left first
                ctx.add_inner(None, (TermInner::Prop, &self.meta).into());
                let (cb, did_cb) = b.convert(env, ctx)?; // Then reduce right
                ctx.remove_inner();
                Ok((
                    (TermInner::Lambda(Box::new(ca), Box::new(cb)), &self.meta).into(),
                    did_ca || did_cb,
                ))
            }
        }
    }

    // Applies successive reduction steps to fully normalize a term.
    //
    // Returns `Err(...)` if an error occurred.
    pub fn normalize_in_ctx(
        &self,
        env: &Environment<T>,
        ctx: &mut Context<T>,
    ) -> Result<Term<T>, Error<T>> {
        let mut r = self.clone();
        let mut pending = true;
        while pending {
            (r, pending) = r.convert(env, ctx)?;
        }
        Ok(r)
    }

    /// Applies successive reduction steps to fully normalize a term.
    ///
    /// Returns `Err(...)` if an error occurred.
    pub fn normalize(&self, env: &Environment<T>) -> Result<Term<T>, Error<T>> {
        self.normalize_in_ctx(env, &mut Context::new())
    }
}
