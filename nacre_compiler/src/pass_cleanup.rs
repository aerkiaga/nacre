use crate::{Ir, IrInstr};
use std::collections::HashSet;

fn compute_def_deps(ir: &Ir, ir_index: usize, r: &mut HashSet<usize>) {
    if r.contains(&ir_index) {
        return;
    }
    r.insert(ir_index);
    let code = &ir.defs[ir_index].as_ref().unwrap().code;
    for loc in code {
        match loc.instr {
            IrInstr::Closure(f, _) | IrInstr::Function(f) => {
                compute_def_deps(ir, f, r);
            }
            _ => {}
        }
    }
}

pub(crate) fn pass_cleanup(ir: &mut Ir) {
    let mut used_defs = HashSet::new();
    for n in 0..ir.defs.len() {
        if let Some(def) = &ir.defs[n] {
            if def.export {
                let deps = compute_def_deps(ir, n, &mut used_defs);
            }
        }
    }
    for n in 0..ir.defs.len() {
        if !used_defs.contains(&n) {
            ir.defs[n] = None;
        }
    }
}
