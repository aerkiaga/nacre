use crate::{Ir, IrInstr, IrLoc, IrType};
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
        IrInstr::Enum(_, c) => match c {
            Some(inner) => &r | &trace_deps(code, *inner),
            None => r,
        },
        IrInstr::Param(_) | IrInstr::Capture(_) => r,
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

fn uncurry_type(ir: &mut Ir, n: usize) {
    if let Some(IrType::Closure(p1, Some(r1i))) = &ir.types[n] {
        let r = *r1i;
        if let IrType::Closure(p2, r2) = ir.types[r].as_ref().unwrap() {
            let mut p = p1.clone();
            let mut p2c = p2.clone();
            let rr = *r2;
            uncurry_type(ir, r);
            p.append(&mut p2c);
            ir.types[n] = Some(IrType::Closure(p, rr));
        }
    }
}

fn handle_partial(ir: &mut Ir, n: usize) {
    let def = match &mut ir.defs[n] {
        None => return,
        Some(d) => d,
    };
    let code = &mut def.code;
    for n in 0..code.len() {
        if let IrInstr::Apply(f, p) = &code[n].instr {
            let origin = trace_back(code, *f);
            match &ir.types[code[origin].value_type.unwrap()].as_ref().unwrap() {
                IrType::Enum(v) => {
                    if p.len() != v.len() {
                        todo!();
                    }
                }
                IrType::Closure(p2, _) => {
                    if p.len() != p2.len() {
                        todo!();
                    }
                }
                IrType::Struct(_) => {
                    if p.len() != 1 {
                        todo!();
                    }
                }
                _ => panic!(),
            }
        }
    }
}

fn get_def_dependencies(r: &mut HashSet<usize>, ir: &Ir, n: usize) {
    r.insert(n);
    let code = &ir.defs[n].as_ref().unwrap().code;
    for loc in code {
        if let IrInstr::Closure(f, _) = &loc.instr {
            if !r.contains(f) {
                get_def_dependencies(r, ir, *f);
            }
        };
    }
}

fn pass_clean_up(ir: &mut Ir) {
    let mut deps = HashSet::new();
    for (n, def) in ir.defs.iter().enumerate() {
        if let Some(d) = def {
            if d.export && !deps.contains(&n) {
                get_def_dependencies(&mut deps, ir, n);
            }
        }
    }
    for n in 0..ir.defs.len() {
        if !deps.contains(&n) {
            ir.defs[n] = None;
        }
    }
}

pub(crate) fn pass_uncurry(ir: &mut Ir) {
    // First, uncurry all global definitions
    for n in 0..ir.defs.len() {
        uncurry_def(ir, n);
    }
    // Second, uncurry all types
    for n in 0..ir.types.len() {
        uncurry_type(ir, n);
    }
    // Then, uncurry applications themselves
    for n in 0..ir.defs.len() {
        uncurry_applications(ir, n);
    }
    // Finally, identify any remaining partial applications
    for n in 0..ir.defs.len() {
        handle_partial(ir, n);
    }
    pass_clean_up(ir);
}
