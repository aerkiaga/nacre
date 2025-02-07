use nacre_parser::get_definition_index;
use nacre_parser::get_global_environment;
use nacre_parser::get_global_environment_names;
use std::collections::HashSet;
use std::fmt::Write;

mod base;
mod code_transforms;
mod codegen;
mod pass_declosure;
mod pass_uncurry;
mod typing;

pub use typing::IrType;

/// An IR instruction, taking some operands and producing an output.
#[derive(Clone)]
pub enum IrInstr {
    /// Loads the parameter corresponding to some index from the current definition.
    Param(usize),
    /// Loads the capture corresponding to some index from the current definition.
    Capture(usize),
    /// Applies a locally-defined closure to a list of locally-defined parameters.
    Apply(usize, Vec<usize>),
    /// Builds a closure from a global definition and a list of locally-defined captures.
    Closure(usize, Vec<usize>),
    /// Builds a function from a global definition.
    Function(usize),
    /// Returns a copy of the input local as output.
    Move(usize),
    /// Builds an enum variant with an index and a contained local value.
    Enum(usize, Option<usize>),
}

impl std::fmt::Debug for IrInstr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Param(n) => write!(f, "param {}", n)?,
            Self::Capture(n) => write!(f, "capture {}", n)?,
            Self::Apply(func, p) => write!(
                f,
                "apply ${}{}",
                func,
                p.iter().fold(String::new(), |mut output, x| {
                    write!(output, " ${}", x).unwrap();
                    output
                })
            )?,
            Self::Closure(func, c) => write!(
                f,
                "closure @{}{}",
                func,
                c.iter().fold(String::new(), |mut output, x| {
                    write!(output, " ${}", x).unwrap();
                    output
                })
            )?,
            Self::Function(func) => write!(f, "fn @{}", func)?,
            Self::Move(n) => write!(f, "${}", n)?,
            Self::Enum(v, c) => match c {
                Some(inner) => write!(f, "enum {} ${}", v, inner)?,
                None => write!(f, "enum {}", v)?,
            },
        }
        Ok(())
    }
}

/// A single line of IR code.
#[derive(Clone)]
pub struct IrLoc {
    /// Instruction executed at this location.
    pub instr: IrInstr,
    pub value_type: Option<usize>,
}

impl std::fmt::Debug for IrLoc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.instr.fmt(f)?;
        match self.value_type {
            Some(t) => writeln!(f, " : <{t}>")?,
            None => writeln!(f, " : void")?,
        }
        Ok(())
    }
}

/// A global function definition.
pub struct IrDef {
    /// Index of the definition in the global environment, if it corresponds to a CoC definition.
    pub env_index: Option<usize>,
    /// Name of the definition, if it corresponds to a symbol in the code.
    pub name: Option<String>,
    /// Whether this symbol is externally visible.
    pub export: bool,
    /// Number of parameters taken by the function.
    pub params: usize,
    /// Types of all parameters.
    pub param_types: Vec<Option<usize>>,
    /// Set of indices of values captured by the function.
    pub captures: HashSet<usize>,
    /// Types of all captures.
    pub capture_types: Vec<Option<usize>>,
    /// IR code.
    pub code: Vec<IrLoc>,
}

impl std::fmt::Debug for IrDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(index) = self.env_index {
            writeln!(f, "// env index: {}", index)?;
        }
        if let Some(name) = &self.name {
            writeln!(f, "// name: {}", name)?;
        }
        if self.export {
            writeln!(f, "// export")?;
        }
        writeln!(
            f,
            "// {} params, {} captures",
            self.params,
            self.captures.len()
        )?;
        if !self.param_types.is_empty() {
            write!(f, "//")?;
            for typ in &self.param_types {
                match typ {
                    None => write!(f, " void")?,
                    Some(t) => write!(f, " <{t}>")?,
                }
            }
            writeln!(f)?;
        }
        if !self.capture_types.is_empty() {
            write!(f, "//")?;
            for typ in &self.capture_types {
                match typ {
                    None => write!(f, " void")?,
                    Some(t) => write!(f, " <{t}>")?,
                }
            }
            writeln!(f)?;
        }
        if !self.captures.is_empty() {
            let mut captures = self.captures.iter().collect::<Vec<_>>();
            captures.sort();
            writeln!(
                f,
                "// captures:{}",
                captures.iter().fold(String::new(), |mut output, x| {
                    write!(output, " {}", x).unwrap();
                    output
                })
            )?;
        }
        for (n, def) in self.code.iter().enumerate() {
            write!(f, "${} = ", n)?;
            def.fmt(f)?;
        }
        Ok(())
    }
}

/// IR representation of a program.
pub struct Ir {
    /// List of types, possibly containing gaps.
    pub types: Vec<Option<IrType>>,
    /// List of definitions, possibly containing gaps.
    pub defs: Vec<Option<IrDef>>,
}

impl std::fmt::Debug for Ir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "*** Intermediate representation ***")?;
        for (n, typ) in self.types.iter().enumerate() {
            if let Some(t) = typ {
                writeln!(f, "  <{}>:", n)?;
                t.fmt(f)?
            }
        }
        for (n, def) in self.defs.iter().enumerate() {
            if let Some(d) = def {
                writeln!(f, "  @{}:", n)?;
                d.fmt(f)?
            }
        }
        Ok(())
    }
}

impl Ir {
    pub fn emit_code(&self) -> Result<(), ()> {
        codegen::emit_code(self)
    }
}

/// Compute IR for a list of exported symbols, loading all necessary dependencies.
pub async fn compile(identifiers: Vec<String>) -> Result<Ir, ()> {
    let mut indices = vec![];
    for id in identifiers {
        indices.push(get_definition_index(&id).await?);
    }
    let env = get_global_environment().await;
    let names = get_global_environment_names().await;
    let mut ir = base::compute_initial_ir(&indices, &env, &names);
    pass_uncurry::pass_uncurry(&mut ir);
    pass_declosure::pass_declosure(&mut ir);
    Ok(ir)
}
