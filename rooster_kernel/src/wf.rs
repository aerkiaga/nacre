use crate::*;

impl<T: Meta> Term<T> {
    // Checks well-formedness of a term in an environment and context.
    pub(crate) fn is_well_formed(&self, env: &Environment<T>, ctx: &mut Context<T>) -> bool {
        match &self.inner {
            TermInner::Prop | TermInner::Type(_) => true,
            TermInner::Global(g) => env.contains_global(*g),
            TermInner::Variable(v) => ctx.contains_variable(*v),
            TermInner::Forall(a, b) => {
                a.is_well_formed(env, ctx) && {
                    ctx.add_inner(None, *a.clone());
                    let r = b.is_well_formed(env, ctx);
                    ctx.remove_inner();
                    r
                }
            }
            TermInner::Lambda(a, b) => {
                a.is_well_formed(env, ctx) && {
                    ctx.add_inner(None, *a.clone());
                    let r = b.is_well_formed(env, ctx);
                    ctx.remove_inner();
                    r
                }
            }
            TermInner::Apply(a, b) => a.is_well_formed(env, ctx) && b.is_well_formed(env, ctx),
            TermInner::Let(a, b) => {
                a.is_well_formed(env, ctx) && {
                    ctx.add_inner(Some(*a.clone()), (TermInner::Prop, &self.meta).into());
                    let r = b.is_well_formed(env, ctx);
                    ctx.remove_inner();
                    r
                }
            }
        }
    }

    /// Checks well-formedness of a term in an environment.
    pub fn well_formed(&self, env: &Environment<T>) -> bool {
        self.is_well_formed(env, &mut Context::new())
    }
}

impl<T: Meta> Environment<T> {
    // Checks whether the term can be added to an environment while
    // keeping that environment well-formed.
    // Assumes empty context.
    pub(crate) fn global_is_legal(
        &self,
        global_def: Option<&Term<T>>,
        global_type: &Term<T>,
    ) -> bool {
        global_type.well_formed(self)
            && (match global_def {
                // all global values are well-formed
                Some(def) => def.well_formed(self),
                None => match global_type.get_type(self) {
                    Ok(tglobal_type) => match tglobal_type.normalize(self) {
                        // all global assumptions' types' types are sorts
                        Ok(ctglobal_type) => ctglobal_type.is_sort(),
                        Err(_) => false,
                    },
                    Err(_) => false, // TODO: handle specific errors
                },
            })
    }
}
