use crate::*;

impl Term {
    // Applies beta and delta reduction to fully convert a term.
    // Returns Err(()) if an error occurred.
    pub(crate) fn convert(&self, env: &Environment, ctx: &Context) -> Result<Term, ()> {
        match self {
            Term::Prop => Ok(Term::Prop),
            Term::Type(n) => Ok(Term::Type(*n)),
            Term::Global(g) => match env.global_value(*g) {
                Some(t) => Ok(self.replace_global(*g, t)?), // Delta-Global
                None => Err(()),
            },
            Term::Variable(v) => match ctx.variable_value(*v) {
                Some(t) => Ok(self.replace_variable(0, t)?), // Delta-Local
                None => Err(()),
            },
            Term::Apply(a, b) => {
                let ca = a.convert(env, ctx)?; // Reduce callee first
                let cb = b.convert(env, ctx)?; // Innermost order
                                               // TODO: explore outermost order
                match ca {
                    Term::Lambda(_, d) => Ok(d.replace_variable(0, &cb)?), // Beta
                    _ => Ok(Term::Apply(Box::new(ca), Box::new(cb))),
                }
            }
            Term::Let(a, b) => {
                let ca = a.convert(env, ctx)?; // Reduce callee first
                let cb = b.convert(env, ctx)?; // Innermost order
                                               // TODO: explore outermost order
                Ok(cb.replace_variable(0, &ca)?) // Zeta
            }
            Term::Forall(a, b) => {
                let ca = a.convert(env, ctx)?; // Reduce type first
                let cb = b.convert(env, ctx)?; // Then reduce inner term
                Ok(Term::Forall(Box::new(ca), Box::new(cb))) // Zeta
            }
            Term::Lambda(a, b) => {
                let ca = a.convert(env, ctx)?; // Reduce type first
                let cb = b.convert(env, ctx)?; // Then reduce inner term
                Ok(Term::Lambda(Box::new(ca), Box::new(cb))) // Zeta
            }
        }
    }

    /// Applies successive reduction steps to fully normalize a term.
    ///
    /// Returns `Err(())` if an error occurred.
    pub fn normalize(&self, env: &Environment) -> Result<Term, ()> {
        self.convert(env, &Context::new())
    }
}
