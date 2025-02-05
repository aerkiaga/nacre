use crate::{IrInstr, IrLoc};
use std::collections::HashSet;

/// Finds the instruction at which the output of some instruction is produced.
pub(crate) fn trace_back(code: &Vec<IrLoc>, output: usize) -> usize {
    match code[output].instr {
        IrInstr::Move(i) => trace_back(code, i),
        _ => output,
    }
}

/// Increases all indices in the code by some amount, possibly to append it.
pub(crate) fn move_code(code: &mut [IrLoc], dist: usize) {
    for loc in code {
        loc.instr = match &loc.instr {
            IrInstr::Apply(f, p) => {
                IrInstr::Apply(*f + dist, p.iter().map(|x| *x + dist).collect())
            }
            IrInstr::Closure(f, c) => IrInstr::Closure(*f, c.iter().map(|x| *x + dist).collect()),
            IrInstr::Move(x) => IrInstr::Move(*x + dist),
            instr => instr.clone(),
        };
    }
}

fn move_rel(a: usize, rem: usize) -> usize {
    if a > rem {
        a - 1
    } else {
        a
    }
}

/// Removes an instruction.
pub(crate) fn remove_loc(code: &mut Vec<IrLoc>, n: usize) -> IrLoc {
    let len = code.len();
    for loc in &mut code[n + 1..len] {
        loc.instr = match &loc.instr {
            IrInstr::Apply(f, p) => {
                IrInstr::Apply(move_rel(*f, n), p.iter().map(|x| move_rel(*x, n)).collect())
            }
            IrInstr::Closure(f, c) => {
                IrInstr::Closure(*f, c.iter().map(|x| move_rel(*x, n)).collect())
            }
            IrInstr::Move(x) => IrInstr::Move(move_rel(*x, n)),
            instr => instr.clone(),
        };
    }
    code.remove(n)
}

/// Increases all parameter in the code by some amount, possibly to append tham.
pub(crate) fn move_params(code: &mut Vec<IrLoc>, dist: usize) {
    for loc in code {
        loc.instr = match &loc.instr {
            IrInstr::Param(p) => IrInstr::Param(*p + dist),
            instr => instr.clone(),
        };
    }
}

/// Replaces each capture in the code by an input local from a list.
pub(crate) fn replace_captures(code: &mut Vec<IrLoc>, captures: &[usize]) {
    for loc in code {
        loc.instr = match &loc.instr {
            IrInstr::Capture(c) => IrInstr::Move(captures[*c]),
            instr => instr.clone(),
        };
    }
}

fn trace_deps(code: &Vec<IrLoc>, output: usize) -> HashSet<usize> {
    let mut r = HashSet::new();
    r.insert(output);
    match &code[output].instr {
        IrInstr::Apply(f, p) => p.iter().fold(&r | &trace_deps(code, *f), |y, x| {
            &y | &trace_deps(code, *x)
        }),
        IrInstr::Closure(_, p) => p.iter().fold(r, |y, x| &y | &trace_deps(code, *x)),
        IrInstr::Move(l) => &r | &trace_deps(code, *l),
        IrInstr::Enum(_, c) => match c {
            Some(inner) => &r | &trace_deps(code, *inner),
            None => r,
        },
        IrInstr::Param(_) | IrInstr::Capture(_) => r,
    }
}

/// Removes all instructions that are not dependencies of the specified one.
pub(crate) fn clean_up_nondeps(code: &mut Vec<IrLoc>, output: usize) {
    let deps = trace_deps(code, output);
    for n in (0..code.len()).rev() {
        if !deps.contains(&n) {
            remove_loc(code, n);
        }
    }
}
