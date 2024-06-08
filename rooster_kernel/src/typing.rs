use crate::*;

use std::sync::Arc;

// Returns the type of a forall x:A, B expression, where ta and tb are the types of the inner terms.
// Returns Err(()) if the term does not have a type or an error occurred.
fn combine_forall_types<T: Meta>(
    ta: &Term<T>,
    tb: &Term<T>,
    env: &Environment<T>,
    ctx: &mut Context<T>,
    meta_a: &Arc<T>,
    meta_b: &Arc<T>,
) -> Result<Term<T>, Error<T>> {
    let cta = ta.normalize_in_ctx(env, ctx)?;
    let ctb = tb.normalize_in_ctx(env, ctx)?; /* x:A is not necessary */
    // TODO: improve metadata handling
    match ctb.inner {
        TermInner::Prop => {
            if cta.is_sort() {
                Ok((TermInner::Prop, &ctb.meta).into())
            } else {
                Err(Error::NonSort {
                    expr: meta_a.clone(),
                    offending: Arc::new(ta.clone()),
                })
            }
        }
        TermInner::Type(j) => match cta.inner {
            TermInner::Prop => Ok((TermInner::Type(j), &ctb.meta).into()), /* ? */
            TermInner::Type(i) => Ok((TermInner::Type(i.max(j)), &ctb.meta).into()),
            _ => Err(Error::NonSort {
                expr: meta_a.clone(),
                offending: Arc::new(ta.clone()),
            }),
        },
        _ => Err(Error::NonSort {
            expr: meta_b.clone(),
            offending: Arc::new(tb.clone()),
        }),
    }
}

impl<T: Meta> Term<T> {
    // Returns the type of a term in an environment and context, if it exists.
    // Assumes that both the environment and context are well-formed.
    // Returns Err(()) if the term does not have a type or an error occurred.
    pub(crate) fn compute_type(
        &self,
        env: &Environment<T>,
        ctx: &mut Context<T>,
    ) -> Result<Term<T>, Error<T>> {
        debug_assert!(self.is_well_formed(env, ctx));
        match &self.inner {
            TermInner::Prop => Ok((TermInner::Type(0), &self.meta).into()),
            TermInner::Type(n) => {
                let r = n.checked_add(1).ok_or(Error::Other)?;
                Ok((TermInner::Type(r), &self.meta).into())
            }
            TermInner::Global(g) => env.global_type(*g).map(|x| x.clone()).ok_or(Error::Other),
            TermInner::Variable(v) => ctx
                .variable_type(*v)
                .map(|x| x.make_inner_by_n(v + 1))
                .ok_or(Error::Other)?,
            TermInner::Forall(a, b) => {
                let ta = a.compute_type(env, ctx)?;
                ctx.add_inner(None, *a.clone());
                let tb = b.compute_type(env, ctx)?;
                ctx.remove_inner();
                combine_forall_types(&ta, &tb, env, ctx, &a.meta, &b.meta)
            }
            TermInner::Lambda(a, b) => {
                let ta = a.compute_type(env, ctx)?;
                ctx.add_inner(None, *a.clone());
                let tb = b.compute_type(env, ctx)?;
                let ttb = tb.compute_type(env, ctx)?;
                ctx.remove_inner();
                combine_forall_types(&ta, &ttb, env, ctx, &a.meta, &b.meta)?;
                Ok((
                    TermInner::Forall(Box::new(*a.clone()), Box::new(tb)),
                    &self.meta,
                )
                    .into())
            }
            TermInner::Apply(a, b) => {
                let ta = a.compute_type(env, ctx)?;
                match ta.inner {
                    TermInner::Forall(c, d) => {
                        let tb = b.compute_type(env, ctx)?;
                        let ctb = tb.normalize_in_ctx(env, ctx)?;
                        let cc = c.normalize_in_ctx(env, ctx)?;
                        if ctb == cc {
                            d.replace_variable(0, &b)
                        } else {
                            Err(Error::AppMismatchedType {
                                lhs: Arc::new(*a.clone()),
                                expected: Arc::new(cc),
                                rhs: Arc::new(*b.clone()),
                                found: Arc::new(tb),
                                env: env.clone(),
                                ctx: ctx.clone(),
                            })
                        }
                    }
                    _ => {
                        let cta = ta.normalize_in_ctx(env, ctx)?;
                        match cta.inner {
                            TermInner::Forall(c, d) => {
                                let tb = b.compute_type(env, ctx)?;
                                let ctb = tb.normalize_in_ctx(env, ctx)?;
                                let cc = c.normalize_in_ctx(env, ctx)?;
                                if ctb == cc {
                                    d.replace_variable(0, &b)
                                } else {
                                    Err(Error::AppMismatchedType {
                                        lhs: Arc::new(*a.clone()),
                                        expected: Arc::new(cc),
                                        rhs: Arc::new(*b.clone()),
                                        found: Arc::new(tb),
                                        env: env.clone(),
                                        ctx: ctx.clone(),
                                    })
                                }
                            }
                            _ => Err(Error::AppInvalid {
                                lhs: Arc::new(*a.clone()),
                                ltype: Arc::new(ta),
                                rhs: Arc::new(*b.clone()),
                            }),
                        }
                    }
                }
            }
            TermInner::Let(a, b) => {
                let ta = a.compute_type(env, ctx)?;
                ctx.add_inner(Some(*a.clone()), ta);
                let tb = b.compute_type(env, ctx)?;
                ctx.remove_inner();
                tb.replace_variable(0, &a)
            }
        }
    }

    /// Returns the type of a top-level term.
    ///
    /// Returns `Err(())` if the term does not have a type or an error occurred.
    pub fn get_type(&self, env: &Environment<T>) -> Result<Term<T>, Error<T>> {
        self.compute_type(env, &mut Context::new())
    }
}
