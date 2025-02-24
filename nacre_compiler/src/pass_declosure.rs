use crate::code_transforms::{trace_back, trace_forwards};
use crate::{Ir, IrInstr, IrType, typing};

fn declosure_def(ir: &mut Ir, n: usize) {
    let def = match &mut ir.defs[n] {
        None => return,
        Some(d) => d,
    };
    let code = &mut def.code;
    for n in 0..code.len() {
        let loc = &code[n];
        if let IrInstr::Closure(f, c) = &loc.instr {
            if c.is_empty() {
                let ff = *f;
                let usages = trace_forwards(code, n);
                for usage in usages.iter() {
                    let direct_application = if let IrInstr::Apply(f2, _) = code[*usage].instr {
                        trace_back(code, f2) == n
                    } else {
                        false
                    };
                    if !direct_application {
                        todo!();
                    }
                }
                let loc = &mut code[n];
                loc.instr = IrInstr::Function(ff);
                loc.value_type = if let IrType::Closure(p, r) =
                    ir.types[loc.value_type.unwrap()].as_ref().unwrap()
                {
                    Some(typing::add_type(
                        IrType::Function(p.clone(), *r),
                        &mut ir.types,
                    ))
                } else {
                    panic!();
                };
            }
        }
    }
}

pub(crate) fn pass_declosure(ir: &mut Ir) {
    for n in 0..ir.defs.len() {
        declosure_def(ir, n);
    }
}
