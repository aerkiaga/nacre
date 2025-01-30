use nacre_parser::get_definition_index;
use nacre_parser::get_global_environment;
use nacre_parser::get_global_environment_names;
use std::collections::HashSet;

mod base;
mod codegen;

enum IrInstr {
    Param(usize),
    Capture(usize),
    Apply(usize, Vec<usize>),
    Closure(usize, Vec<usize>),
    Move(usize),
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
                p.iter().map(|x| format!(" ${}", x)).collect::<String>()
            )?,
            Self::Closure(func, c) => write!(
                f,
                "closure @{}{}",
                func,
                c.iter().map(|x| format!(" ${}", x)).collect::<String>()
            )?,
            Self::Move(n) => write!(f, "${}", n)?,
        }
        Ok(())
    }
}

struct IrLoc {
    instr: IrInstr,
}

impl std::fmt::Debug for IrLoc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.instr.fmt(f)?;
        write!(f, "\n")?;
        Ok(())
    }
}

struct IrDef {
    env_index: Option<usize>,
    name: Option<String>,
    export: bool,
    params: usize,
    captures: HashSet<usize>,
    code: Vec<IrLoc>,
}

impl std::fmt::Debug for IrDef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(index) = self.env_index {
            write!(f, "// env index: {}\n", index)?;
        }
        if let Some(name) = &self.name {
            write!(f, "// name: {}\n", name)?;
        }
        if self.export {
            write!(f, "// export\n")?;
        }
        write!(
            f,
            "// {} params, {} captures\n",
            self.params,
            self.captures.len()
        )?;
        if self.captures.len() > 0 {
            let mut captures = self.captures.iter().collect::<Vec<_>>();
            captures.sort();
            write!(
                f,
                "// captures:{}\n",
                captures
                    .iter()
                    .map(|x| format!(" {}", x))
                    .collect::<String>()
            );
        }
        for (n, def) in self.code.iter().enumerate() {
            write!(f, "${} = ", n)?;
            def.fmt(f)?;
        }
        Ok(())
    }
}

pub struct Ir {
    defs: Vec<Option<IrDef>>,
}

impl std::fmt::Debug for Ir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "*** Intermediate representation ***\n")?;
        for (n, def) in self.defs.iter().enumerate() {
            match def {
                Some(d) => {
                    write!(f, "  @{}:\n", n)?;
                    d.fmt(f)?
                }
                None => {}
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

pub async fn compile(identifiers: Vec<String>) -> Result<Ir, ()> {
    let mut indices = vec![];
    for id in identifiers {
        indices.push(get_definition_index(&id).await?);
    }
    let env = get_global_environment().await.into_vec();
    let names = get_global_environment_names().await;
    let ir = base::compute_initial_ir(&indices, &env, &names);
    Ok(ir)
}
