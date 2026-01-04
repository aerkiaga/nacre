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
    /// An owned reference to an object of a type that encloses this type.
    Recursive(Option<usize>),
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
                    match field {
                        None => {
                            writeln!(f, "void")?;
                        }
                        Some(d) => writeln!(f, "<{d}>")?,
                    }
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
            IrType::Recursive(inner) => {
                writeln!(f, "recursive")?;
                if let Some(inn) = inner {
                    writeln!(f, " {inn}")?;
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
                                IrType::Struct(fields) => {
                                    add_type(IrType::Closure(fields.clone(), any_type), types)
                                }
                                _ => add_type(IrType::Closure(vec![Some(ti)], any_type), types),
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
                for typ in types.iter_mut() {
                    // resolve recursive types
                    if let Some(IrType::Recursive(None)) = *typ {
                        *typ = Some(IrType::Recursive(bt));
                    }
                }
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
                                            let struct_type_id = match variants_a.len() {
                                                0 => None, // no fields
                                                _ => {
                                                    // recursive field
                                                    let variants_a_clone = variants_a.clone();
                                                    let struct_fields = variants_a_clone
                                                        .iter()
                                                        .map(|field| {
                                                            let rec_type = IrType::Recursive(None);
                                                            let rec_type_id =
                                                                Some(add_type(rec_type, types));
                                                            match field {
                                                                None => rec_type_id, // non-closure
                                                                _ => todo!(),        // closure
                                                            }
                                                        })
                                                        .collect();
                                                    let struct_type = IrType::Struct(struct_fields);
                                                    Some(add_type(struct_type, types))
                                                }
                                            };
                                            let new_variants = IrType::Enum(
                                                [struct_type_id]
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
                                        .chain(variants_clone.into_iter().map(|_t| {
                                            // fields of the form ... -> T
                                            // TODO: actually handle this as recursive fields
                                            Some(add_type(IrType::Any, types))
                                        }))
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
                        IrType::Struct(fields) => {
                            // we are tentatively building a struct
                            // add parameter type to it
                            let struct_type = IrType::Struct(
                                [at].into_iter()
                                    .chain(fields.into_iter().copied())
                                    .collect(),
                            );
                            (Some(add_type(struct_type, types)), Some(bg.unwrap() - 1))
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

fn try_deduplicate_type_rec(
    t: Option<usize>,
    rr: usize,
    types: &mut Vec<Option<IrType>>,
    t2: Option<usize>,
    rr2: usize,
) -> Option<usize> {
    let tt = match t {
        Some(tt) => tt,
        None => return None,
    };
    let tt2 = match t2 {
        Some(tt2) => tt2,
        None => return Some(tt),
    };
    match types[tt].as_ref().unwrap() {
        IrType::Recursive(r) => {
            if let IrType::Recursive(r2) = types[tt2].as_ref().unwrap() {
                if *r == Some(rr) && *r2 == Some(rr2) {
                    return Some(tt2);
                }
            }
        }
        IrType::Enum(variants) => {
            if let IrType::Enum(variants2) = types[tt2].as_ref().unwrap() {
                if variants.len() == variants2.len() {
                    let mut equal = true;
                    let variants = variants.clone();
                    let variants2 = variants2.clone();
                    for n in 0..variants2.len() {
                        if variants2[n]
                            != try_deduplicate_type_rec(variants[n], rr, types, variants2[n], rr2)
                        {
                            equal = false;
                            break;
                        }
                    }
                    if equal {
                        return Some(tt2);
                    }
                }
            }
        }
        IrType::Struct(fields) => {
            if let IrType::Struct(fields2) = types[tt2].as_ref().unwrap() {
                if fields.len() == fields2.len() {
                    let mut equal = true;
                    let fields = fields.clone();
                    let fields2 = fields2.clone();
                    for n in 0..fields.len() {
                        if fields2[n]
                            != try_deduplicate_type_rec(fields[n], rr, types, fields2[n], rr2)
                        {
                            equal = false;
                            break;
                        }
                    }
                    if equal {
                        return Some(tt2);
                    }
                }
            }
        }
        IrType::Closure(params, ret) => {
            if let IrType::Closure(params2, ret2) = types[tt2].as_ref().unwrap() {
                if params.len() == params2.len() {
                    let mut equal = true;
                    let params = params.clone();
                    let params2 = params2.clone();
                    let ret2 = ret2.clone();
                    if ret2 != try_deduplicate_type_rec(*ret, rr, types, ret2, rr2) {
                        equal = false;
                    } else {
                        for n in 0..params.len() {
                            if params2[n]
                                != try_deduplicate_type_rec(params[n], rr, types, params2[n], rr2)
                            {
                                equal = false;
                                break;
                            }
                        }
                    }
                    if equal {
                        return Some(tt2);
                    }
                }
            }
        }
        IrType::Function(params, ret) => {
            if let IrType::Function(params2, ret2) = types[tt2].as_ref().unwrap() {
                if params.len() == params2.len() {
                    let mut equal = true;
                    let params = params.clone();
                    let params2 = params2.clone();
                    let ret2 = ret2.clone();
                    if ret2 != try_deduplicate_type_rec(*ret, rr, types, ret2, rr2) {
                        equal = false;
                    } else {
                        for n in 0..params.len() {
                            if params2[n]
                                != try_deduplicate_type_rec(params[n], rr, types, params2[n], rr2)
                            {
                                equal = false;
                                break;
                            }
                        }
                    }
                    if equal {
                        return Some(tt2);
                    }
                }
            }
        }
        _ => return Some(tt),
    }
    Some(tt)
}

fn deduplicate_type(t: Option<usize>, types: &mut Vec<Option<IrType>>) -> Option<usize> {
    if let Some(tt) = t {
        // first, try to de-duplicate inner types
        match types[tt].as_ref().unwrap() {
            IrType::Enum(variants) => {
                let variants = variants.clone();
                let mut new_variants = vec![];
                for v in variants {
                    new_variants.push(deduplicate_type(v, types));
                }
                types[tt] = Some(IrType::Enum(new_variants));
            }
            IrType::Struct(fields) => {
                let fields = fields.clone();
                let mut new_fields = vec![];
                for v in fields {
                    new_fields.push(deduplicate_type(v, types));
                }
                types[tt] = Some(IrType::Struct(new_fields));
            }
            IrType::Closure(params, ret) => {
                let params = params.clone();
                let new_ret = deduplicate_type(*ret, types);
                let mut new_params = vec![];
                for v in params {
                    new_params.push(deduplicate_type(v, types));
                }
                types[tt] = Some(IrType::Closure(new_params, new_ret));
            }
            IrType::Function(params, ret) => {
                let params = params.clone();
                let new_ret = deduplicate_type(*ret, types);
                let mut new_params = vec![];
                for v in params {
                    new_params.push(deduplicate_type(v, types));
                }
                types[tt] = Some(IrType::Function(new_params, new_ret));
            }
            _ => {}
        }
        // then, de-duplicate current type if equal
        for t2 in 0..tt {
            if types[tt].as_ref() == types[t2].as_ref() {
                return Some(t2);
            }
        }
        // finally, try to de-duplicate current type assuming it is recursive
        for t2 in 0..tt {
            let new_t = try_deduplicate_type_rec(Some(tt), tt, types, Some(t2), t2);
            if new_t != Some(tt) {
                return new_t;
            }
        }
        // if everything fails, type is already unique
        t
    } else {
        None
    }
}

pub(crate) fn compute_type(
    term: &Term<TermMeta>,
    types: &mut Vec<Option<IrType>>,
    ctx: &mut Context<TermMeta>,
    env: &Environment<TermMeta>,
) -> Option<usize> {
    let (mut t, ind) = compute_type_rec(term, types, ctx, env);
    let r = if ind.is_some() && ind.unwrap() != 0 {
        undo_inductive(t, types)
    } else if let Some(IrType::Struct(_)) = t.map(|tt| types[tt].as_ref().unwrap()) {
        undo_inductive(t, types)
    } else {
        loop {
            if let Some(IrType::Enum(variants)) = t.map(|tt| types[tt].as_ref().unwrap()) {
                if variants.len() == 1 {
                    t = variants[0];
                } else {
                    break;
                }
            } else {
                break;
            }
        }
        t
    };
    deduplicate_type(r, types)
}

pub(crate) fn is_generic(
    term: &Term<TermMeta>,
    _types: &Vec<Option<IrType>>,
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
