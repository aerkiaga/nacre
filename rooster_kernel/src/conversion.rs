use crate::*;

impl<T: Meta> Term<T> {
    // Applies beta and delta reduction to fully convert a term.
    // Returns Err(()) if an error occurred.
    fn convert(&mut self, env: &Environment<T>, ctx: &mut Context<T>) -> Result<bool, Error<T>> {
        match &mut self.inner {
            TermInner::Prop => Ok(false),
            TermInner::Type(n) => Ok(false),
            TermInner::Global(g) => match env.global_value(*g) {
                Some(t) => {
                    // Delta-Global
                    *self = t.clone();
                    Ok(true)
                }
                None => Err(Error::Other),
            },
            TermInner::Variable(v) => match ctx.variable_value(*v) {
                Some(t) => {
                    // Delta-Local
                    *self = t.make_inner_by_n(*v + 1)?;
                    Ok(true)
                }
                None => Ok(false),
            },
            TermInner::Apply(a, b) => {
                match &a.inner {
                    TermInner::Lambda(_, d) => {
                        // Beta
                        *self = d.replace_variable(0, &b)?;
                        Ok(true)
                    }
                    _ => Ok(a.convert(env, ctx)? || b.convert(env, ctx)?),
                }
            }
            TermInner::Let(a, b) => {
                // Zeta
                *self = b.replace_variable(0, &a)?;
                Ok(true)
            }
            TermInner::Forall(a, b) | TermInner::Lambda(a, b) => Ok(a.convert(env, ctx)? || {
                ctx.add_inner(None, (TermInner::Prop, &self.meta).into());
                let tmp = b.convert(env, ctx)?;
                ctx.remove_inner();
                tmp
            }),
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
            pending = r.convert(env, ctx)?;
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
