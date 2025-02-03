use crate::{Ir, IrInstr, IrLoc};
use std::collections::HashSet;

fn trace_back(code: &Vec<IrLoc>, output: usize) -> usize {
    match code[output].instr {
        IrInstr::Move(i) => trace_back(code, i),
        _ => output,
    }
}

fn move_code(code: &mut [IrLoc], dist: usize) {
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

fn remove_loc(code: &mut Vec<IrLoc>, n: usize) -> IrLoc {
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

fn remove_back(code: &mut Vec<IrLoc>, output: usize) {
    let element = remove_loc(code, output);
    if let IrInstr::Move(i) = element.instr {
        remove_back(code, i)
    }
}

fn move_params(code: &mut Vec<IrLoc>, dist: usize) {
    for loc in code {
        loc.instr = match &loc.instr {
            IrInstr::Param(p) => IrInstr::Param(*p + dist),
            instr => instr.clone(),
        };
    }
}

fn replace_captures(code: &mut Vec<IrLoc>, captures: &[usize]) {
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
        _ => r,
    }
}

fn clean_up_nondeps(code: &mut Vec<IrLoc>, output: usize) {
    let deps = trace_deps(code, output);
    for n in (0..code.len()).rev() {
        if !deps.contains(&n) {
            remove_loc(code, n);
        }
    }
}

fn uncurry_def(ir: &mut Ir, n: usize) {
    let def = match &ir.defs[n] {
        None => return,
        Some(d) => d,
    };
    let code_index = trace_back(&def.code, def.code.len() - 1);
    // check if the current definition is curried
    let (closure_index, captures) = match &def.code[code_index].instr {
        IrInstr::Closure(i, c) => (*i, c.clone()),
        _ => return,
    };
    let len = def.code.len();
    let n_params = def.params;
    // make sure the returned closure is uncurried too
    uncurry_def(ir, closure_index);
    // prepare receiver
    let def = match &mut ir.defs[n] {
        None => return,
        Some(d) => d,
    };
    remove_back(&mut def.code, len - 1);
    let len = def.code.len();
    // prepare code to append
    let mut second_code = ir.defs[closure_index].as_ref().unwrap().code.clone();
    move_code(&mut second_code, len);
    move_params(&mut second_code, n_params);
    replace_captures(&mut second_code, &captures);
    // get values
    let other_def = ir.defs[closure_index].as_ref().unwrap();
    let other_params = other_def.params;
    let mut other_param_types = other_def.param_types.clone();
    // append code
    let def = match &mut ir.defs[n] {
        None => return,
        Some(d) => d,
    };
    def.code.append(&mut second_code);
    // join definitions
    def.params += other_params;
    def.param_types.append(&mut other_param_types);
}

fn uncurry_applications(ir: &mut Ir, n: usize) {
    let def = match &mut ir.defs[n] {
        None => return,
        Some(d) => d,
    };
    let code = &mut def.code;
    for n in 0..code.len() {
        if let IrInstr::Apply(f, p) = &code[n].instr {
            let origin = trace_back(code, *f);
            if let IrInstr::Apply(f_origin, p_origin) = &code[origin].instr {
                code[n].instr = IrInstr::Apply(
                    *f_origin,
                    p_origin.iter().chain(p.iter()).copied().collect(),
                );
            }
        }
    }
    clean_up_nondeps(code, code.len() - 1);
}

pub(crate) fn pass_uncurry(ir: &mut Ir) {
    // First, uncurry all global definitions
    for n in 0..ir.defs.len() {
        uncurry_def(ir, n);
    }
    // Second, uncurry all types
    // TODO
    // Then, uncurry applications themselves
    for n in 0..ir.defs.len() {
        uncurry_applications(ir, n);
    }
    // Finally, identify any remaining partial applications
    // TODO
}
