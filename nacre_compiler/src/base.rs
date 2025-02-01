use crate::{Ir, IrDef, IrInstr, IrLoc};
use nacre_kernel::Term;
use nacre_kernel::TermInner;
use nacre_parser::TermMeta;
use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Arc;

type Definition = (Option<Arc<Term<TermMeta>>>, Arc<Term<TermMeta>>);

fn compute_global(
    g: &usize,
    ir: &mut Ir,
    ir_index: usize,
    env_to_ir_index: &mut Vec<Option<usize>>,
    env: &Vec<Definition>,
    names: &Vec<String>,
) -> Result<(), ()> {
    match env_to_ir_index[*g] {
        Some(_ir_index2) => {}
        None => {
            ir.defs.push(None);
            compute_ir_definition(ir, ir.defs.len() - 1, *g, env_to_ir_index, env, names)?;
        }
    }
    let closure_ir_index = env_to_ir_index[*g].unwrap();
    let captures = ir.defs[closure_ir_index].as_ref().unwrap().captures.clone();
    let def = ir.defs[ir_index].as_mut().unwrap();
    for c in captures.iter() {
        if *c == 0 {
            def.code.push(IrLoc {
                instr: IrInstr::Param(0),
            });
        } else {
            def.code.push(IrLoc {
                instr: IrInstr::Capture(c - 1),
            });
            def.captures.insert(c - 1);
        }
    }
    let passed_captures = ((def.code.len() - captures.len())..(def.code.len())).collect();
    def.code.push(IrLoc {
        instr: IrInstr::Closure(closure_ir_index, passed_captures),
    });
    Ok(())
}

fn compute_variable(
    v: &usize,
    ir: &mut Ir,
    ir_index: usize,
    defs: &mut HashMap<usize, usize>,
) -> Result<(), ()> {
    let def = ir.defs[ir_index].as_mut().unwrap();
    match defs.get(v) {
        Some(&usize::MAX) => def.code.push(IrLoc {
            instr: IrInstr::Param(0),
        }),
        Some(n) => def.code.push(IrLoc {
            instr: IrInstr::Move(*n),
        }),
        _ => {
            let mut par = 0;
            for (k, v) in defs.iter() {
                if *v == usize::MAX {
                    par = *k;
                }
            }
            def.code.push(IrLoc {
                instr: IrInstr::Capture(*v - par - 1),
            });
            def.captures.insert(*v - par - 1);
        }
    }
    Ok(())
}

fn compute_lambda(
    lam: (&Term<TermMeta>, &Term<TermMeta>),
    ir: &mut Ir,
    ir_index: usize,
    env_to_ir_index: &mut Vec<Option<usize>>,
    env: &Vec<Definition>,
    names: &Vec<String>,
    defs: &mut HashMap<usize, usize>,
) -> Result<(), ()> {
    let (a, b) = lam;
    let def = ir.defs[ir_index].as_mut().unwrap();
    let new_defs = HashMap::new();
    let old_defs: HashMap<_, _>;
    (old_defs, *defs) = (defs.clone(), new_defs);
    defs.insert(0, usize::MAX);
    if let TermInner::Prop = a.inner {
        compute_ir_instruction(ir, ir_index, env_to_ir_index, b, env, names, defs)?;
    } else if def.params == 0 && def.code.is_empty() {
        def.params = 1;
        compute_ir_instruction(ir, ir_index, env_to_ir_index, b, env, names, defs)?;
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
        compute_ir_instruction(ir, ir.defs.len() - 1, env_to_ir_index, b, env, names, defs)?;
        let captures = ir.defs[closure_ir_index].as_ref().unwrap().captures.clone();
        let def = ir.defs[ir_index].as_mut().unwrap();
        for c in captures.iter() {
            match defs.get(c) {
                Some(&usize::MAX) => def.code.push(IrLoc {
                    instr: IrInstr::Param(0),
                }),
                Some(n) => def.code.push(IrLoc {
                    instr: IrInstr::Move(*n),
                }),
                _ => {
                    let mut par = 0;
                    for (k, v) in defs.iter() {
                        if *v == usize::MAX {
                            par = *k;
                        }
                    }
                    def.code.push(IrLoc {
                        instr: IrInstr::Capture(c - par - 1),
                    });
                    def.captures.insert(c - par - 1);
                }
            }
        }
        let passed_captures = ((def.code.len() - captures.len())..(def.code.len())).collect();
        def.code.push(IrLoc {
            instr: IrInstr::Closure(closure_ir_index, passed_captures),
        });
    }
    *defs = old_defs;
    Ok(())
}

fn compute_apply(
    app: (&Term<TermMeta>, &Term<TermMeta>),
    ir: &mut Ir,
    ir_index: usize,
    env_to_ir_index: &mut Vec<Option<usize>>,
    env: &Vec<Definition>,
    names: &Vec<String>,
    defs: &mut HashMap<usize, usize>,
) -> Result<(), ()> {
    let (a, b) = app;
    compute_ir_instruction(ir, ir_index, env_to_ir_index, a, env, names, defs)?;
    let a_index = ir.defs[ir_index].as_ref().unwrap().code.len() - 1;
    if compute_ir_instruction(ir, ir_index, env_to_ir_index, b, env, names, defs).is_ok() {
        let b_index = ir.defs[ir_index].as_ref().unwrap().code.len() - 1;
        ir.defs[ir_index].as_mut().unwrap().code.push(IrLoc {
            instr: IrInstr::Apply(a_index, vec![b_index]),
        });
    }
    Ok(())
}

fn compute_let(
    lt: (&Term<TermMeta>, &Term<TermMeta>),
    ir: &mut Ir,
    ir_index: usize,
    env_to_ir_index: &mut Vec<Option<usize>>,
    env: &Vec<Definition>,
    names: &Vec<String>,
    defs: &mut HashMap<usize, usize>,
) -> Result<(), ()> {
    let (a, b) = lt;
    compute_ir_instruction(ir, ir_index, env_to_ir_index, a, env, names, defs)?;
    let a_index = ir.defs[ir_index].as_ref().unwrap().code.len() - 1;
    let mut new_defs = HashMap::new();
    let old_defs: HashMap<_, _>;
    for (k, v) in defs.iter() {
        new_defs.insert(k + 1, *v);
    }
    (old_defs, *defs) = (defs.clone(), new_defs);
    defs.insert(0, a_index);
    compute_ir_instruction(ir, ir_index, env_to_ir_index, b, env, names, defs)?;
    *defs = old_defs;
    Ok(())
}

fn compute_ir_instruction(
    ir: &mut Ir,
    ir_index: usize,
    env_to_ir_index: &mut Vec<Option<usize>>,
    term: &Term<TermMeta>,
    env: &Vec<Definition>,
    names: &Vec<String>,
    defs: &mut HashMap<usize, usize>,
) -> Result<(), ()> {
    match &term.inner {
        TermInner::Global(g) => compute_global(g, ir, ir_index, env_to_ir_index, env, names),
        TermInner::Variable(v) => compute_variable(v, ir, ir_index, defs),
        TermInner::Lambda(a, b) => {
            compute_lambda((a, b), ir, ir_index, env_to_ir_index, env, names, defs)
        }
        TermInner::Apply(a, b) => {
            compute_apply((a, b), ir, ir_index, env_to_ir_index, env, names, defs)
        }
        TermInner::Let(a, b) => {
            compute_let((a, b), ir, ir_index, env_to_ir_index, env, names, defs)
        }
        _ => Err(()),
    }
}

fn compute_ir_definition(
    ir: &mut Ir,
    ir_index: usize,
    env_index: usize,
    env_to_ir_index: &mut Vec<Option<usize>>,
    env: &Vec<Definition>,
    names: &Vec<String>,
) -> Result<(), ()> {
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
    compute_ir_instruction(
        ir,
        ir_index,
        env_to_ir_index,
        term,
        env,
        names,
        &mut HashMap::new(),
    )
}

pub(crate) fn compute_initial_ir(
    indices: &[usize],
    env: &Vec<Definition>,
    names: &Vec<String>,
) -> Ir {
    let mut env_to_ir_index: Vec<Option<usize>> = (0..env.len()).map(|_| None).collect();
    let reserved_indices = indices.len();
    let mut r = Ir {
        defs: (0..reserved_indices).map(|_| None).collect(),
    };
    for (n, initial_env_index) in indices.iter().enumerate().take(reserved_indices) {
        r.defs[n] = None;
        compute_ir_definition(
            &mut r,
            n,
            *initial_env_index,
            &mut env_to_ir_index,
            env,
            names,
        )
        .unwrap();
    }
    for n in 0..reserved_indices {
        if let Some(ref mut d) = &mut r.defs[n] {
            d.export = true;
        }
    }
    r
}
