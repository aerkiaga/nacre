use crate::code_transforms::{change_type, clean_up_nondeps, insert_loc, trace_back};
use crate::typing;
use crate::{Ir, IrInstr, IrLoc, IrType};
use std::collections::HashMap;

fn specialize_def(
    ir: &mut Ir,
    ir_index: usize,
    param_types: &Vec<Option<usize>>,
    return_type: Option<usize>,
    specialization_map: &mut HashMap<usize, Vec<usize>>,
) -> usize {
    match specialization_map.get(&ir_index) {
        None => {
            specialization_map.insert(ir_index, vec![]);
        }
        Some(specializations) => {
            for s in specializations {
                let def = ir.defs[*s].as_ref().unwrap();
                let def_param_types = &def.param_types;
                let def_return_type = &def.code.last().unwrap().value_type;
                // TODO: return existing specialization if types match
                todo!();
            }
        }
    }
    ir.defs.push(ir.defs[ir_index].clone());
    let def = ir.defs.last_mut().unwrap().as_mut().unwrap();
    def.env_index = None;
    def.name = None;
    def.export = false;
    let code = &mut def.code;
    let last_index = code.len() - 1;
    change_type(code, last_index, return_type);
    assert!(param_types.len() == def.param_types.len());
    for n in 0..param_types.len() {
        if param_types[n] != def.param_types[n] {
            todo!();
        }
    }
    def.param_types = param_types.clone();
    specialization_map
        .get_mut(&ir_index)
        .unwrap()
        .push(ir.defs.len() - 1);
    ir.defs.len() - 1
}

fn try_specialize_closure(
    ir: &mut Ir,
    ir_index: usize,
    apply_index: usize,
    left_index: usize,
    param_types: &Vec<Option<usize>>,
    return_type: Option<usize>,
    specialization_map: &mut HashMap<usize, Vec<usize>>,
) -> usize {
    let def = match &ir.defs[ir_index] {
        None => return 0,
        Some(d) => d,
    };
    let code = &def.code;
    let left_type = code[left_index].value_type.unwrap();
    let mut do_specialize = false;
    let mut new_value_type = None;
    // check if the left-hand value needs to be specialized
    match &ir.types[left_type].as_ref().unwrap() {
        IrType::Closure(closure_params, closure_return)
        | IrType::Function(closure_params, closure_return) => {
            assert!(closure_params.len() == param_types.len());
            if *closure_return != return_type {
                // check return type
                do_specialize = true;
            } else {
                // check parameter types
                for n in 0..closure_params.len() {
                    if closure_params[n] != param_types[n] {
                        do_specialize = true;
                        break;
                    }
                }
            }
            new_value_type = Some(IrType::Closure(param_types.clone(), return_type));
        }
        _ => todo!(),
    }
    if do_specialize {
        let left_instr = &code[left_index].instr;
        match left_instr {
            IrInstr::Function(fn_ir_index) => {
                let new_index = specialize_def(
                    ir,
                    *fn_ir_index,
                    param_types,
                    return_type,
                    specialization_map,
                );
                let new_instr = IrInstr::Function(new_index);
                let new_loc = IrLoc {
                    instr: new_instr,
                    value_type: new_value_type.map(|t| typing::add_type(t, &mut ir.types)),
                };
                let code = &mut ir.defs[ir_index].as_mut().unwrap().code;
                insert_loc(code, left_index + 1, new_loc);
                match &mut code[apply_index + 1].instr {
                    IrInstr::Apply(f, _) => {
                        *f = left_index + 1;
                    }
                    _ => todo!(),
                }
                1
            }
            _ => todo!(),
        }
    } else {
        0
    }
}

fn specialize_applications(
    ir: &mut Ir,
    ir_index: usize,
    specialization_map: &mut HashMap<usize, Vec<usize>>,
) {
    let mut n = 0;
    loop {
        let def = match &ir.defs[ir_index] {
            None => return,
            Some(d) => d,
        };
        let code = &def.code;
        if n >= code.len() {
            break;
        }
        let loc = &code[n];
        match &loc.instr {
            IrInstr::Apply(f, p) => {
                let left_index = trace_back(code, *f);
                let param_types: Vec<_> = p
                    .into_iter()
                    .map(|pt| trace_back(code, *pt))
                    .map(|pi| code[pi].value_type)
                    .collect();
                let return_type = loc.value_type;
                let index_delta = try_specialize_closure(
                    ir,
                    ir_index,
                    n,
                    left_index,
                    &param_types,
                    return_type,
                    specialization_map,
                );
                n += index_delta;
            }
            _ => {}
        }
        n += 1;
    }
    let code = &mut ir.defs[ir_index].as_mut().unwrap().code;
    let last_index = code.len() - 1;
    clean_up_nondeps(code, last_index);
}

pub(crate) fn pass_specialize(ir: &mut Ir) {
    let mut specialization_map = HashMap::new();
    let mut n = 0;
    loop {
        if n >= ir.defs.len() {
            break;
        }
        specialize_applications(ir, n, &mut specialization_map);
        n += 1;
    }
}
