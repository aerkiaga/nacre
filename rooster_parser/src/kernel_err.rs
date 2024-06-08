use crate::*;

use rooster_kernel::Context;
use rooster_kernel::Environment;
use rooster_kernel::Error;
use rooster_kernel::Meta;
use rooster_kernel::Term;
use rooster_kernel::TermInner;
use std::ops::Range;

pub(crate) struct TermMeta {
    pub(crate) range: Range<usize>,
}

impl Default for TermMeta {
    fn default() -> TermMeta {
        TermMeta { range: 0..0 }
    }
}

impl Meta for TermMeta {}

fn adjust_strings(s1: String, s2: String) -> (String, String) {
    let l = s1.len().max(s2.len());
    (format!("{:<1$}", s1, l), format!("{:<1$}", s2, l))
}

fn produce_term(t: &Term<TermMeta>) -> String {
    match &t.inner {
        TermInner::Prop => "Type".to_string(),
        TermInner::Type(n) => format!("Type{}", n + 1),
        TermInner::Forall(l, r) => {
            format!("type(@: {}) -> {}", produce_term(&*l), produce_term(&*r))
        }
        _ => "@".to_string(),
    }
}

fn produce_comparison(
    t1: &Term<TermMeta>,
    t2: &Term<TermMeta>,
    env: &Environment<TermMeta>,
    ctx: &mut Context<TermMeta>,
) -> (String, String) {
    // same subterm
    if t1 == t2 {
        return adjust_strings(produce_term(t1), produce_term(t2));
    }
    // try to expand
    match t1.inner {
        TermInner::Global(g) => match env.delta_replacement(g) {
            Ok(ct1) => return produce_comparison(&ct1, t2, env, ctx),
            Err(_) => {}
        },
        TermInner::Variable(v) => match ctx.delta_replacement(v) {
            Ok(ct1) => return produce_comparison(&ct1, t2, env, ctx),
            Err(_) => {}
        },
        _ => {}
    }
    match t2.inner {
        TermInner::Global(g) => match env.delta_replacement(g) {
            Ok(ct2) => return produce_comparison(t1, &ct2, env, ctx),
            Err(_) => {}
        },
        TermInner::Variable(v) => match ctx.delta_replacement(v) {
            Ok(ct2) => return produce_comparison(t1, &ct2, env, ctx),
            Err(_) => {}
        },
        _ => {}
    }
    // different terms
    match &t1.inner {
        TermInner::Forall(l1, r1) => {
            if let TermInner::Forall(l2, r2) = &t2.inner {
                // TODO: introduce variable
                let l = produce_comparison(&*l1, &*l2, env, ctx);
                let r = produce_comparison(&*r1, &*r2, env, ctx);
                let s1 = format!("type(@: {}) -> {}", l.0, r.0);
                let s2 = format!("type(@: {}) -> {}", l.1, r.1);
                return (s1, s2);
            }
        }
        _ => {}
    }
    return adjust_strings(
        format!("`{}`", produce_term(t1)),
        format!("`{}`", produce_term(t2)),
    );
}

pub(crate) fn report(error: Error<TermMeta>, filename: &str) {
    match error {
        Error::MismatchedType {
            expected,
            found,
            env,
        } => {
            let mut ctx = Context::new();
            report::send(Report {
                is_error: true,
                filename: filename.to_string(),
                offset: 0,
                message: "mismatched type in definition".to_string(),
                note: {
                    let (exp, fnd) = produce_comparison(&expected, &found, &env, &mut ctx);
                    Some(format!(
                        "mismatched types\nexpected {}\nfound    {}",
                        exp, fnd
                    ))
                },
                help: None,
                labels: vec![
                    (
                        expected.meta.range.clone(),
                        "type specified here".to_string(),
                    ),
                    (found.meta.range.clone(), "has wrong type".to_string()),
                ],
            });
        }
        Error::Other => {
            report::send(Report {
                is_error: true,
                filename: filename.to_string(),
                offset: 0,
                message: "unknown kernel error".to_string(),
                note: None,
                help: None,
                labels: vec![],
            });
        }
    }
}
