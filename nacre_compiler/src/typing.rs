use crate::IrDef;
use nacre_kernel::Term;
use nacre_kernel::TermInner;
use nacre_parser::TermMeta;
use std::collections::HashMap;
use std::sync::Arc;

type Definition = (Option<Arc<Term<TermMeta>>>, Arc<Term<TermMeta>>);

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
    mut term: &Term<TermMeta>,
    types: &mut Vec<Option<IrType>>,
    env: &Vec<Definition>,
    def: &IrDef,
    defs: &HashMap<usize, usize>,
    mut level: usize,
) -> Option<usize> {
    let mut fields = vec![];
    loop {
        match &term.inner {
            TermInner::Variable(v) => {
                if *v == level {
                    break;
                } else {
                    todo!();
                }
            }
            TermInner::Forall(a, b) => {
                fields.push(a);
                term = b;
                level += 1;
            }
            _ => todo!(),
        }
    }
    let r: Vec<_> = fields
        .into_iter()
        .map(|s| compute_type(s, types, env, def, defs))
        .collect();
    if r.is_empty() {
        None
    } else {
        Some(add_type(IrType::Struct(r), types))
    }
}

pub(crate) fn compute_enum(
    mut term: &Term<TermMeta>,
    types: &mut Vec<Option<IrType>>,
    env: &Vec<Definition>,
    def: &IrDef,
    defs: &HashMap<usize, usize>,
) -> Option<usize> {
    let mut structs = vec![];
    let mut level = 0;
    loop {
        match &term.inner {
            TermInner::Variable(v) => {
                if *v == level {
                    break;
                } else {
                    todo!();
                }
            }
            TermInner::Forall(a, b) => {
                structs.push(a);
                term = b;
                level += 1;
            }
            _ => todo!(),
        }
    }
    let r: Vec<_> = structs
        .into_iter()
        .enumerate()
        .map(|(n, s)| compute_struct(s, types, env, def, defs, n))
        .collect();
    if r.is_empty() {
        panic!("Falsehood found during compilation");
    } else if r.len() == 1 {
        r[0]
    } else {
        Some(add_type(IrType::Enum(r), types))
    }
}

pub(crate) fn compute_type(
    term: &Term<TermMeta>,
    types: &mut Vec<Option<IrType>>,
    env: &Vec<Definition>,
    def: &IrDef,
    defs: &HashMap<usize, usize>,
) -> Option<usize> {
    match &term.inner {
        TermInner::Prop => None,
        TermInner::Type(_) => None,
        TermInner::Global(g) => compute_type(env[*g].0.as_ref().unwrap(), types, env, def, defs),
        TermInner::Variable(_) => Some(add_type(IrType::Any, types)),
        TermInner::Forall(a, b) => {
            if a.inner == TermInner::Prop {
                compute_enum(b, types, env, def, defs)
            } else {
                let at = compute_type(a, types, env, def, defs);
                //let bb = b.make_outer_by_n(0).unwrap();
                let bt = compute_type(b, types, env, def, defs);
                let t = IrType::Closure(vec![at], bt);
                Some(add_type(t, types))
            }
        }
        TermInner::Lambda(_a, _b) => todo!(),
        TermInner::Apply(_a, _b) => todo!(),
        TermInner::Let(_a, _b) => todo!(),
    }
}

pub(crate) fn compute_closure(def: &IrDef, types: &mut Vec<Option<IrType>>) -> Option<usize> {
    if def.param_types.len() == 1 && def.param_types[0].is_some() {
        match types[def.param_types[0].unwrap()].as_ref().unwrap() {
            IrType::Any => {
                let r = def.code.last().unwrap().value_type;
                if r.is_some() {
                    if let IrType::Enum(e) = types[r.unwrap()].as_ref().unwrap() {
                        let e2 = [None].iter().chain(e.iter()).copied().collect();
                        return Some(add_type(IrType::Enum(e2), types));
                    }
                }
                return Some(add_type(IrType::Enum(vec![None]), types));
            }
            _ => {} // TODO
        }
    }
    let t = IrType::Closure(def.param_types.clone(), def.code.last().unwrap().value_type);
    Some(add_type(t, types))
}

pub(crate) fn compute_apply(
    a_type: Option<usize>,
    b_type: Option<usize>,
    types: &mut Vec<Option<IrType>>,
) -> Option<usize> {
    match types[a_type.unwrap()].as_ref().unwrap() {
        IrType::Closure(_, r) => *r,
        IrType::Enum(e) => {
            let variants = e.len();
            if variants == 1 {
                if e[0].is_none() {
                    b_type
                } else if let IrType::Closure(_, r) = types[b_type.unwrap()].as_ref().unwrap() {
                    *r
                } else {
                    panic!();
                }
            } else {
                let e2 = e.iter().skip(1).copied().collect();
                let t = IrType::Enum(e2);
                Some(add_type(t, types))
            }
        }
        _ => todo!(),
    }
}
