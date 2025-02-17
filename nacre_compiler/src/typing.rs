use nacre_kernel::Term;
use nacre_kernel::TermInner;
use nacre_kernel::{Context, Environment};
use nacre_parser::TermMeta;

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

fn undo_inductive(inductive: Option<usize>, types: &mut Vec<Option<IrType>>) -> Option<usize> {
    match types[inductive.unwrap()].as_ref().unwrap() {
        IrType::Enum(variants) => {
            let variants_clone = variants.clone();
            let any_type = Some(add_type(IrType::Any, types));
            if variants_clone.is_empty() {
                any_type
            } else {
                let new_type = IrType::Closure(
                    variants_clone
                        .iter()
                        .map(|t| {
                            t.map(|ti| match types[ti].as_ref().unwrap() {
                                _ => todo!(),
                            })
                        })
                        .collect(),
                    any_type,
                );
                Some(add_type(new_type, types))
            }
        }
        IrType::Struct(fields) => {
            let new_type = IrType::Closure(
                fields
                    .iter()
                    .copied()
                    .map(|ti| {
                        // TODO: take recursive fields into account
                        ti
                    })
                    .collect(),
                Some(add_type(IrType::Any, types)),
            );
            Some(add_type(new_type, types))
        }
        _ => todo!(),
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
            } else if let Some(vv) = ctx.variable_value(*v) {
                let vvc = vv.clone();
                let vvt = vvc.compute_type(env, ctx).unwrap();
                compute_type_rec(&vvt.clone(), types, ctx, env)
            } else {
                let mut reduced = vt.clone();
                assert!(reduced.convert(env, ctx).unwrap());
                compute_type_rec(&reduced, types, ctx, env)
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
                    match types[bt.unwrap()].as_ref().unwrap() {
                        IrType::Enum(variants) => {
                            if let Some(ga) = ag {
                                // the parameter type also contains an inductive type under construction
                                if ga + 1 == g {
                                    // generics match, this is part of our inductive type
                                    match types[at.unwrap()].as_ref().unwrap() {
                                        IrType::Enum(variants_a) => {
                                            // the parameter type can be interpreted as an enum variant
                                            // that itself looks like an enum
                                            // this means we have a recursive inductive type
                                            // ((... -> T) -> (... -> T) -> T) -> ... -> T
                                            // ... or an enum variant with no fields
                                            // T -> ... -> T
                                            let variants_clone = variants.clone();
                                            // convert parameter type into new enum variant
                                            let struct_type = match variants_a.len() {
                                                0 => None,    // no fields
                                                _ => todo!(), // recursive field
                                            };
                                            let new_variants = IrType::Enum(
                                                [struct_type]
                                                    .into_iter()
                                                    .chain(variants_clone)
                                                    .collect(),
                                            );
                                            (Some(add_type(new_variants, types)), Some(ga))
                                        }
                                        IrType::Struct(fields) => {
                                            // the parameter type contains a tentatively-defined struct
                                            // definitely accept it and return an enum
                                            let variant = match fields.len() {
                                                0 => panic!(),
                                                1 => fields[0],
                                                _ => at,
                                            };
                                            let new_variants = IrType::Enum(
                                                [variant]
                                                    .into_iter()
                                                    .chain(variants.iter().copied())
                                                    .collect(),
                                            );
                                            (Some(add_type(new_variants, types)), Some(ga))
                                        }
                                        _ => {
                                            // the parameter type contains something else
                                            // tentatively generate a struct
                                            todo!();
                                        }
                                    }
                                } else {
                                    // the parameter type looks like an inductive type, but generics don't match
                                    // we thus have a non-inductive type
                                    // ((... -> U) -> (... -> U) -> U) -> ... -> T
                                    // tentatively generate a struct
                                    let variants_clone = variants.clone();
                                    let parameter_type = undo_inductive(at, types);
                                    let fields = [parameter_type]
                                        .into_iter()
                                        .chain(variants_clone.into_iter().map(|_t| todo!()))
                                        .collect();
                                    let struct_type = IrType::Struct(fields);
                                    (
                                        Some(add_type(struct_type, types)),
                                        if g == 0 { None } else { Some(g - 1) },
                                    )
                                }
                            } else {
                                // the parameter type contains an already-built inductive type
                                // tentatively generate a struct
                                let struct_type = IrType::Struct(vec![at]);
                                (Some(add_type(struct_type, types)), Some(bg.unwrap() - 1))
                            }
                        }
                        IrType::Struct(_fields) => {
                            // we are tentatively building a struct
                            // add parameter type to it
                            todo!();
                        }
                        _ => {
                            panic!();
                        }
                    }
                } else {
                    // we're not building an inductive type
                    let at_new = if ag.is_none() {
                        at
                    } else {
                        undo_inductive(at, types)
                    };
                    let closure_type = IrType::Closure(vec![at_new], bt);
                    (Some(add_type(closure_type, types)), None)
                }
            };
            ctx.remove_inner();
            r
        }
        TermInner::Lambda(_a, _b) => todo!(),
        TermInner::Apply(_, _) | TermInner::Let(_, _) => {
            let mut new_term = term.clone();
            new_term.convert(env, ctx).unwrap();
            let (t, g) = compute_type_rec(&new_term, types, ctx, env);
            if let TermInner::Let(_, _) = &term.inner {
                (
                    t,
                    match g {
                        None => None,
                        Some(0) => None,
                        Some(x) => Some(x - 1),
                    },
                )
            } else {
                (t, g)
            }
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
    if ind.is_none() || ind == Some(0) {
        t
    } else {
        undo_inductive(t, types)
    }
}

pub(crate) fn is_generic(
    term: &Term<TermMeta>,
    types: &Vec<Option<IrType>>,
    ctx: &mut Context<TermMeta>,
    env: &Environment<TermMeta>,
) -> bool {
    let term = term.normalize_in_ctx(env, ctx).unwrap();
    if let TermInner::Forall(a_g, b_g) = &term.inner {
        if a_g.inner != TermInner::Prop {
            return false;
        }
        let mut depth = 0;
        let mut t = b_g;
        while let TermInner::Forall(_, ti) = &t.inner {
            depth += 1;
            t = ti;
        }
        if let TermInner::Variable(v) = &t.inner {
            *v != depth
        } else {
            false
        }
    } else {
        false
    }
}
