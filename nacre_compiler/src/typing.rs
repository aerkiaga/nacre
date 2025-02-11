use nacre_kernel::Term;
use nacre_kernel::TermInner;
use nacre_kernel::{Context, Environment};
use nacre_parser::TermMeta;
use std::collections::HashSet;

/// The type of an IR value.
#[derive(Eq, PartialEq)]
pub enum IrType {
    /// An enum with some variants, each possibly containing some data.
    Enum(Vec<Option<usize>>),
    /// A struct with a number of fields.
    Struct(Vec<Option<usize>>),
    /// A closure that takes some parameters and returns a value.
    Closure(Vec<Option<usize>>, Option<usize>),
    /// A function that takes some parameters and returns a value.
    Function(Vec<Option<usize>>, Option<usize>),
    /// Any type.
    Any,
}

impl std::fmt::Debug for IrType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IrType::Enum(opts) => {
                writeln!(f, "enum {{")?;
                for opt in opts {
                    match opt {
                        None => {
                            writeln!(f, "void")?;
                        }
                        Some(d) => writeln!(f, "<{d}>")?,
                    }
                }
                writeln!(f, "}}")?;
            }
            IrType::Struct(fields) => {
                writeln!(f, "struct {{")?;
                for field in fields {
                    field.fmt(f)?;
                }
                writeln!(f, "}}")?;
            }
            IrType::Closure(params, ret) => {
                writeln!(f, "closure (")?;
                for param in params {
                    match param {
                        None => {
                            writeln!(f, "void")?;
                        }
                        Some(d) => writeln!(f, "<{d}>")?,
                    }
                }
                match ret {
                    None => writeln!(f, ") -> void")?,
                    Some(d) => writeln!(f, ") -> <{d:?}>")?,
                }
            }
            IrType::Function(params, ret) => {
                writeln!(f, "fn (")?;
                for param in params {
                    match param {
                        None => {
                            writeln!(f, "void")?;
                        }
                        Some(d) => writeln!(f, "<{d}>")?,
                    }
                }
                match ret {
                    None => writeln!(f, ") -> void")?,
                    Some(d) => writeln!(f, ") -> <{d:?}>")?,
                }
            }
            IrType::Any => writeln!(f, "any")?,
        }
        Ok(())
    }
}

pub(crate) fn add_type(t: IrType, types: &mut Vec<Option<IrType>>) -> usize {
    for (i, t2) in types.iter().enumerate() {
        if let Some(tt) = t2 {
            if t == *tt {
                return i;
            }
        }
    }
    types.push(Some(t));
    types.len() - 1
}

pub(crate) fn compute_struct(
    term: &Term<TermMeta>,
    types: &mut Vec<Option<IrType>>,
    ce: (&mut Context<TermMeta>, &Environment<TermMeta>),
    mut level: usize,
    generics: &HashSet<usize>,
) -> Option<usize> {
    let (ctx, env) = ce;
    let mut t = term.clone();
    let mut r = vec![];
    let mut generics2 = generics.clone();
    loop {
        match &t.inner {
            TermInner::Variable(v) => {
                if generics2.contains(&(level - *v)) {
                    break;
                } else {
                    todo!();
                }
            }
            TermInner::Forall(a, b) => {
                if let TermInner::Prop = a.inner {
                    generics2.insert(level + 1);
                } else {
                    // TODO: handle recursive structs
                    let f = compute_type(a, types, ctx, env);
                    r.push(f);
                }
                t = *b.clone();
                level += 1;
            }
            _ => {
                t.convert(env, ctx).unwrap();
            }
        }
    }
    if r.is_empty() {
        None
    } else if r.len() == 1 {
        r[0]
    } else {
        Some(add_type(IrType::Struct(r), types))
    }
}

pub(crate) fn compute_enum(
    term: &Term<TermMeta>,
    types: &mut Vec<Option<IrType>>,
    ctx: &mut Context<TermMeta>,
    env: &Environment<TermMeta>,
) -> Option<usize> {
    let mut t = term.clone();
    let mut generics = HashSet::new();
    generics.insert(0);
    let mut r = vec![];
    let mut level = 0;
    loop {
        match &t.inner {
            TermInner::Variable(v) => {
                if generics.contains(&(level - *v)) {
                    break;
                } else {
                    todo!();
                }
            }
            TermInner::Forall(a, b) => {
                if let TermInner::Prop = a.inner {
                    generics.insert(level + 1);
                } else {
                    let s = compute_struct(a, types, (ctx, env), level, &generics);
                    r.push(s);
                }
                ctx.add_inner(None, (**a).clone());
                t = *b.clone();
                level += 1;
            }
            _ => {
                t.convert(env, ctx).unwrap();
            }
        }
    }
    for _ in 0..level {
        ctx.remove_inner();
    }
    if r.is_empty() {
        panic!("Falsehood found during compilation");
    } else if r.len() == 1 {
        r[0]
    } else {
        Some(add_type(IrType::Enum(r), types))
    }
}

/**
Computes the type of a term in IR form,
given the type of the term in CoC form.
*/
/*
Inductive types are of the form:
type(T: Type) -> E1 -> E2 -> ... -> T

Where E* represents an enum variant with zero or more fields:
S1 -> S2 -> ... -> T

Where S* is one of:
P1 -> P2 -> ... -> R    (non-recursive field)
P1 -> P2 -> ... -> T    (recursive field)

The P* types must not contain T at all. If they do,
the resulting type is valid but uninhabited
(negative-position induction, which is not possible
to construct). This should result in an error.
*/
pub(crate) fn compute_type_rec(
    term: &Term<TermMeta>,
    types: &mut Vec<Option<IrType>>,
    ctx: &mut Context<TermMeta>,
    env: &Environment<TermMeta>,
) -> (Option<usize>, Option<usize>) {
    match &term.inner {
        TermInner::Prop => (None, None),
        TermInner::Type(_) => (None, None),
        TermInner::Global(g) => {
            compute_type_rec(env.as_vec_ref()[*g].0.as_ref().unwrap(), types, ctx, env)
        }
        TermInner::Variable(v) => {
            let vt = ctx.variable_type(*v).unwrap();
            if vt.inner == TermInner::Prop {
                (Some(add_type(IrType::Enum(vec![]), types)), Some(*v))
            } else {
                let vv = ctx.variable_value(*v).unwrap().clone();
                compute_type_rec(&vv, types, ctx, env)
            }
            // TODO: use conversion wherever appropriate
        }
        TermInner::Forall(a, b) => {
            let r = if a.inner == TermInner::Prop {
                // TODO: check expressions equivalent to Prop
                ctx.add_inner(None, (**a).clone());
                let (bt, bg) = compute_type_rec(b, types, ctx, env);
                (
                    bt,
                    match bg {
                        None => None,
                        Some(0) => None,
                        Some(g) => Some(g - 1),
                    },
                )
                //compute_enum(b, types, ctx, env, )
            } else {
                let (at, ag) = compute_type_rec(a, types, ctx, env);
                ctx.add_inner(None, (**a).clone());
                let (bt, bg) = compute_type_rec(b, types, ctx, env);
                if let Some(g) = bg {
                    // the body contains an inductive type under construction
                    if let IrType::Enum(variants) = types[bt.unwrap()].as_ref().unwrap() {
                        if let Some(ga) = ag {
                            // the parameter type also contains an inductive type under construction
                            if ga + 1 == g {
                                if let IrType::Enum(variants_a) =
                                    types[at.unwrap()].as_ref().unwrap()
                                {
                                    let variants_clone = variants.clone();
                                    // convert parametr type into new enum variant
                                    let struct_type = match variants_a.len() {
                                        0 => None,          // no fields
                                        1 => variants_a[0], // single field
                                        _ => Some(add_type(
                                            IrType::Struct(variants_a.clone()),
                                            types,
                                        )), // struct of fields
                                    };
                                    let new_variants = IrType::Enum(
                                        [struct_type].into_iter().chain(variants_clone).collect(),
                                    );
                                    (Some(add_type(new_variants, types)), Some(ga))
                                } else {
                                    panic!()
                                }
                            } else {
                                todo!();
                            }
                        } else {
                            todo!();
                        }
                    } else {
                        panic!();
                    }
                } else {
                    // we're not building an inductive type
                    let closure_type = IrType::Closure(vec![at], bt);
                    (Some(add_type(closure_type, types)), None)
                }
                /*
                if ag.is_some() {
                    todo!();
                }
                if bg.is_some() {
                    todo!();
                }
                */
                //let t = compute_closure_type(&vec![at], bt, types);
                //t
            };
            ctx.remove_inner();
            r
        }
        TermInner::Lambda(_a, _b) => todo!(),
        TermInner::Apply(_, _) | TermInner::Let(_, _) => {
            let mut new_term = term.clone();
            new_term.convert(env, ctx).unwrap();
            compute_type_rec(&new_term, types, ctx, env)
        }
    }
}

pub(crate) fn compute_type(
    term: &Term<TermMeta>,
    types: &mut Vec<Option<IrType>>,
    ctx: &mut Context<TermMeta>,
    env: &Environment<TermMeta>,
) -> Option<usize> {
    let (t, ind) = compute_type_rec(term, types, ctx, env);
    assert!(ind.is_none());
    if let Some(tt) = t {
        if let IrType::Enum(variants) = types[tt].as_ref().unwrap() {
            if variants.is_empty() {
                panic!("Trivially uninhabited type found during type realization");
            }
        }
    }
    t
}
