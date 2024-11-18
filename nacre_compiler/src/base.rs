use crate::{Ir, IrDef, IrInstr};
use nacre_kernel::Term;
use nacre_kernel::TermInner;
use nacre_parser::TermMeta;
use std::collections::HashSet;
use std::sync::Arc;

type Definition = (Option<Arc<Term<TermMeta>>>, Arc<Term<TermMeta>>);

fn compute_ir_instruction(
    ir: &mut Ir,
    ir_index: usize,
    env_to_ir_index: &mut Vec<Option<usize>>,
    term: &Term<TermMeta>,
    env: &Vec<Definition>,
    names: &Vec<String>,
) -> Result<(), ()> {
    match &term.inner {
        TermInner::Global(g) => {
            match env_to_ir_index[*g] {
                Some(ir_index2) => {}
                None => {
                    ir.defs.push(None);
                    compute_ir_definition(ir, ir.defs.len() - 1, *g, env_to_ir_index, env, names);
                }
            }
            let closure_ir_index = env_to_ir_index[*g].unwrap();
            let captures = ir.defs[closure_ir_index].as_ref().unwrap().captures.clone();
            let def = ir.defs[ir_index].as_mut().unwrap();
            for (n, c) in captures.iter().enumerate() {
                if *c == 0 {
                    def.code.push(IrInstr::Param(0));
                } else {
                    def.code.push(IrInstr::Capture(c - 1));
                    def.captures.insert(c - 1);
                }
            }
            let passed_captures = ((def.code.len() - captures.len())..(def.code.len())).collect();
            def.code
                .push(IrInstr::Closure(closure_ir_index, passed_captures));
        }
        TermInner::Variable(v) => {
            let def = ir.defs[ir_index].as_mut().unwrap();
            if *v == 0 {
                def.code.push(IrInstr::Param(0));
            } else {
                def.code.push(IrInstr::Capture(*v - 1));
                def.captures.insert(*v - 1);
            }
        }
        TermInner::Lambda(a, b) => {
            let def = ir.defs[ir_index].as_mut().unwrap();
            if let TermInner::Prop = a.inner {
                compute_ir_instruction(ir, ir_index, env_to_ir_index, b, env, names);
            } else if def.params == 0 && def.code.len() == 0 {
                def.params = 1;
                compute_ir_instruction(ir, ir_index, env_to_ir_index, b, env, names);
            } else {
                ir.defs.push(Some(IrDef {
                    env_index: None,
                    name: None,
                    export: false,
                    params: 1,
                    captures: HashSet::new(),
                    code: vec![],
                }));
                let closure_ir_index = ir.defs.len() - 1;
                compute_ir_instruction(ir, ir.defs.len() - 1, env_to_ir_index, b, env, names);
                let captures = ir.defs[closure_ir_index].as_ref().unwrap().captures.clone();
                let def = ir.defs[ir_index].as_mut().unwrap();
                for (n, c) in captures.iter().enumerate() {
                    if *c == 0 {
                        def.code.push(IrInstr::Param(0));
                    } else {
                        def.code.push(IrInstr::Capture(c - 1));
                        def.captures.insert(c - 1);
                    }
                }
                let passed_captures =
                    ((def.code.len() - captures.len())..(def.code.len())).collect();
                def.code
                    .push(IrInstr::Closure(closure_ir_index, passed_captures));
            }
        }
        TermInner::Apply(a, b) => {
            compute_ir_instruction(ir, ir_index, env_to_ir_index, a, env, names);
            let a_index = ir.defs[ir_index].as_ref().unwrap().code.len() - 1;
            if let Ok(_) = compute_ir_instruction(ir, ir_index, env_to_ir_index, b, env, names) {
                let b_index = ir.defs[ir_index].as_ref().unwrap().code.len() - 1;
                ir.defs[ir_index]
                    .as_mut()
                    .unwrap()
                    .code
                    .push(IrInstr::Apply(a_index, vec![b_index]));
            }
        }
        TermInner::Let(a, b) => {
            todo!();
        }
        _ => {
            return Err(());
        }
    }
    Ok(())
}

fn compute_ir_definition(
    ir: &mut Ir,
    ir_index: usize,
    env_index: usize,
    env_to_ir_index: &mut Vec<Option<usize>>,
    env: &Vec<Definition>,
    names: &Vec<String>,
) {
    env_to_ir_index[env_index] = Some(ir_index);
    let term = env[env_index].0.as_ref().unwrap();
    ir.defs[ir_index] = Some(IrDef {
        env_index: Some(env_index),
        name: Some(names[env_index].clone()),
        export: false,
        params: 0,
        captures: HashSet::new(),
        code: vec![],
    });
    compute_ir_instruction(ir, ir_index, env_to_ir_index, &term, env, names);
}

pub(crate) fn compute_initial_ir(
    indices: &Vec<usize>,
    env: &Vec<Definition>,
    names: &Vec<String>,
) -> Ir {
    let mut env_to_ir_index: Vec<Option<usize>> = (0..env.len()).map(|_| None).collect();
    let reserved_indices = indices.len();
    let mut r = Ir {
        defs: (0..reserved_indices).map(|_| None).collect(),
    };
    for n in 0..reserved_indices {
        let initial_env_index = indices[n];
        r.defs[n] = None;
        compute_ir_definition(
            &mut r,
            n,
            initial_env_index,
            &mut env_to_ir_index,
            env,
            names,
        );
    }
    for n in 0..reserved_indices {
        match &mut r.defs[n] {
            Some(ref mut d) => {
                d.export = true;
            }
            None => {}
        }
    }
    r
}
