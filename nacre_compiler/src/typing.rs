use nacre_kernel::Term;
use nacre_kernel::TermInner;
use nacre_parser::TermMeta;

#[derive(Eq, PartialEq)]
pub enum IrType {
    Enum(Vec<Option<usize>>),
    Struct(Vec<usize>),
    Closure(Vec<Option<usize>>, usize),
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
                        Some(d) => d.fmt(f)?,
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
                        Some(d) => d.fmt(f)?,
                    }
                }
                writeln!(f, ") -> {ret:?}")?;
            }
        }
        Ok(())
    }
}

fn add_type(t: IrType, types: &mut Vec<Option<IrType>>) -> usize {
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

pub(crate) fn compute_type(
    term: &Term<TermMeta>,
    types: &mut Vec<Option<IrType>>,
) -> Option<usize> {
    // TODO: compute actual type
    let t = IrType::Struct(vec![]);
    Some(add_type(t, types))
}
