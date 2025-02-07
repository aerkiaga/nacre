use crate::typing;
use crate::{Ir, IrDef, IrInstr, IrLoc, IrType};
use nacre_kernel::Term;
use nacre_kernel::TermInner;
use nacre_kernel::{Context, Environment};
use nacre_parser::TermMeta;
use std::collections::HashMap;
use std::collections::HashSet;

fn compute_inductive_const(
    ir: &mut Ir,
    ir_index: usize,
    term: &Term<TermMeta>,
    ce: (&mut Context<TermMeta>, &Environment<TermMeta>),
    defs: &mut HashMap<usize, usize>,
) -> Result<(), ()> {
    let (ctx, env) = ce;
    let new_term = term.normalize_in_ctx(env, ctx).unwrap();
    let mut t = &new_term;
    let mut variant_count = 0;
    let term_type = new_term.compute_type(env, ctx).unwrap();
    while let TermInner::Lambda(_a, b) = &t.inner {
        variant_count += 1;
        t = b;
        //ctx.add_inner(None, (**a).clone());
    }
    // TODO: handle structs, enum contents
    let variant = if let TermInner::Variable(v) = t.inner {
        assert!(v < variant_count);
        variant_count - v - 1
    } else {
        panic!()
    };
    assert!(variant_count > 1); // TODO: handle structs
                                // TODO: move computation into crate::typing
    let def = ir.defs[ir_index].as_mut().unwrap();
    let value_type = typing::compute_enum(&term_type, &mut ir.types, ctx, env, def, defs);
    /*for _ in 0..variant_count {
        ctx.remove_inner();
    }*/
    def.code.push(IrLoc {
        instr: IrInstr::Enum(variant, None),
        value_type,
    });
    Ok(())
}

fn compute_global(
    g: &usize,
    ir: &mut Ir,
    ir_index: usize,
    env_to_ir_index: &mut Vec<Option<usize>>,
    env: &Environment<TermMeta>,
    names: &Vec<String>,
) -> Result<(), ()> {
    match env_to_ir_index[*g] {
        Some(_ir_index2) => {}
        // compute definition if it does not exist
        None => {
            ir.defs.push(None);
            compute_ir_definition(ir, ir.defs.len() - 1, *g, env_to_ir_index, env, names)?;
        }
    }
    let closure_ir_index = env_to_ir_index[*g].unwrap();
    let closure = ir.defs[closure_ir_index].as_ref().unwrap();
    let closure_type = typing::compute_closure(closure, &mut ir.types);
    let captures = closure.captures.clone();
    debug_assert!(captures.is_empty());
    let other_def = ir.defs[closure_ir_index].as_ref().unwrap();
    let n_params = other_def.params;
    let return_type = other_def.code.last().unwrap().value_type;
    let def = ir.defs[ir_index].as_mut().unwrap();
    let passed_captures = vec![];
    def.code.push(IrLoc {
        instr: IrInstr::Closure(closure_ir_index, passed_captures),
        value_type: closure_type,
    });
    if n_params == 0 {
        def.code.push(IrLoc {
            instr: IrInstr::Apply(def.code.len() - 1, vec![]),
            value_type: return_type,
        });
    }
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
        // variable corresponds to lambda parameter
        Some(&usize::MAX) => def.code.push(IrLoc {
            instr: IrInstr::Param(0),
            value_type: def.param_types[0],
        }),
        // variable corresponds to let definition
        Some(n) => def.code.push(IrLoc {
            instr: IrInstr::Move(*n),
            value_type: def.code[*n].value_type,
        }),
        // variable corresponds to something else (capture)
        _ => {
            let mut par = 0;
            for (k, v) in defs.iter() {
                if *v == usize::MAX {
                    par = *k;
                }
            }
            def.code.push(IrLoc {
                instr: IrInstr::Capture(*v - par - 1),
                value_type: def.capture_types[*v - par - 1],
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
    ce: (&mut Context<TermMeta>, &Environment<TermMeta>),
    names: &Vec<String>,
    defs: &mut HashMap<usize, usize>,
) -> Result<(), ()> {
    let (a, b) = lam;
    let (ctx, env) = ce;
    let def = ir.defs[ir_index].as_mut().unwrap();
    let param_type = typing::compute_type(a, &mut ir.types, ctx, env, def, defs);
    let new_defs = HashMap::new();
    let old_defs: HashMap<_, _>;
    (old_defs, *defs) = (defs.clone(), new_defs);
    defs.insert(0, usize::MAX); // innermost definition is now a parameter
    ctx.add_inner(None, a.clone());
    if let TermInner::Prop = a.inner {
        compute_inductive_const(ir, ir_index, b, (ctx, env), defs)?;
    } else if def.params == 0 && def.code.is_empty() {
        // outer body of global definition
        def.params = 1;
        def.param_types = vec![param_type];
        compute_ir_instruction(ir, ir_index, env_to_ir_index, b, (ctx, env), names, defs)?;
    } else {
        // inner lambda, create anonymous global definition
        let mut capture_types = vec![];
        let mut i = 0;
        loop {
            match defs.get(&i) {
                None => break,
                Some(&usize::MAX) => {
                    capture_types.push(def.param_types[0]);
                }
                Some(n) => {
                    capture_types.push(def.code[*n].value_type);
                }
            }
            i += 1;
        }
        ir.defs.push(Some(IrDef {
            env_index: None,
            name: None,
            export: false,
            params: 1,
            param_types: vec![param_type],
            captures: HashSet::new(),
            capture_types,
            code: vec![],
        }));
        let closure_ir_index = ir.defs.len() - 1;
        compute_ir_instruction(
            ir,
            ir.defs.len() - 1,
            env_to_ir_index,
            b,
            (ctx, env),
            names,
            defs,
        )?;
        let closure = ir.defs[closure_ir_index].as_ref().unwrap();
        let closure_type = typing::compute_closure(closure, &mut ir.types);
        let n_params = closure.params;
        let return_type = closure.code.last().unwrap().value_type;
        // pass down captures
        let captures = closure.captures.clone();
        let def = ir.defs[ir_index].as_mut().unwrap();
        for c in captures.iter() {
            match defs.get(c) {
                // capture parameter
                Some(&usize::MAX) => def.code.push(IrLoc {
                    instr: IrInstr::Param(0),
                    value_type: def.param_types[0],
                }),
                // capture local definition
                Some(n) => def.code.push(IrLoc {
                    instr: IrInstr::Move(*n),
                    value_type: def.code[*n].value_type,
                }),
                // capture enclosing capture
                _ => {
                    let mut par = 0;
                    // find enclosing parameter
                    for (k, v) in defs.iter() {
                        if *v == usize::MAX {
                            par = *k;
                        }
                    }
                    def.code.push(IrLoc {
                        instr: IrInstr::Capture(c - par - 1),
                        value_type: def.capture_types[c - par - 1],
                    });
                    def.captures.insert(c - par - 1);
                }
            }
        }
        let passed_captures = ((def.code.len() - captures.len())..(def.code.len())).collect();
        def.code.push(IrLoc {
            instr: IrInstr::Closure(closure_ir_index, passed_captures),
            value_type: closure_type,
        });
        if n_params == 0 {
            def.code.push(IrLoc {
                instr: IrInstr::Apply(def.code.len() - 1, vec![]),
                value_type: return_type,
            });
        }
    }
    ctx.remove_inner();
    *defs = old_defs;
    Ok(())
}

fn compute_apply(
    app: (&Term<TermMeta>, &Term<TermMeta>),
    ir: &mut Ir,
    ir_index: usize,
    env_to_ir_index: &mut Vec<Option<usize>>,
    ce: (&mut Context<TermMeta>, &Environment<TermMeta>),
    names: &Vec<String>,
    defs: &mut HashMap<usize, usize>,
) -> Result<(), ()> {
    let (a, b) = app;
    let (ctx, env) = ce;
    let b_type = b.compute_type(env, ctx).unwrap();
    let b_ir_type = typing::compute_type(
        &b_type,
        &mut ir.types,
        ctx,
        env,
        ir.defs[ir_index].as_ref().unwrap(),
        defs,
    );
    match b_ir_type {
        Some(_) => {
            compute_ir_instruction(ir, ir_index, env_to_ir_index, a, (ctx, env), names, defs)?;
            let a_index = ir.defs[ir_index].as_mut().unwrap().code.len() - 1;
            if compute_ir_instruction(ir, ir_index, env_to_ir_index, b, (ctx, env), names, defs)
                .is_ok()
            {
                let def = ir.defs[ir_index].as_mut().unwrap();
                let b_index = def.code.len() - 1;
                let a_type = def.code[a_index].value_type;
                let b_type = def.code[b_index].value_type;
                if b_type.is_none() {
                    def.code.push(IrLoc {
                        instr: IrInstr::Move(a_index),
                        value_type: a_type,
                    });
                    return Ok(());
                }
                let typ = typing::compute_apply(a_type, b_type, &mut ir.types);
                def.code.push(IrLoc {
                    instr: IrInstr::Apply(a_index, vec![b_index]),
                    value_type: typ,
                });
            }
        }
        None => {
            let a_type = a.compute_type(env, ctx).unwrap();
            let a_ir_type = typing::compute_type(
                &a_type,
                &mut ir.types,
                ctx,
                env,
                ir.defs[ir_index].as_ref().unwrap(),
                defs,
            );
            // TODO move logic into `typing`
            let is = a_ir_type.is_some()
                && matches!(
                    ir.types[a_ir_type.unwrap()].as_ref().unwrap(),
                    IrType::Enum(_) | IrType::Struct(_)
                );
            if is {
                compute_ir_instruction(ir, ir_index, env_to_ir_index, a, (ctx, env), names, defs)?;
            } else {
                let mut new_term = Term {
                    inner: TermInner::Apply(a.clone().into(), b.clone().into()),
                    meta: TermMeta::default().into(),
                };
                new_term.convert(env, ctx).unwrap();
                compute_ir_instruction(
                    ir,
                    ir_index,
                    env_to_ir_index,
                    &new_term,
                    (ctx, env),
                    names,
                    defs,
                )?;
            }
        }
    }
    Ok(())
}

fn compute_let(
    lt: (&Term<TermMeta>, &Term<TermMeta>),
    ir: &mut Ir,
    ir_index: usize,
    env_to_ir_index: &mut Vec<Option<usize>>,
    ce: (&mut Context<TermMeta>, &Environment<TermMeta>),
    names: &Vec<String>,
    defs: &mut HashMap<usize, usize>,
) -> Result<(), ()> {
    let (a, b) = lt;
    let (ctx, env) = ce;
    compute_ir_instruction(ir, ir_index, env_to_ir_index, a, (ctx, env), names, defs)?;
    let a_index = ir.defs[ir_index].as_ref().unwrap().code.len() - 1;
    let mut new_defs = HashMap::new();
    let old_defs: HashMap<_, _>;
    for (k, v) in defs.iter() {
        new_defs.insert(k + 1, *v);
    }
    (old_defs, *defs) = (defs.clone(), new_defs);
    defs.insert(0, a_index);
    let a_type = a.compute_type(env, ctx).unwrap();
    ctx.add_inner(Some(a.clone()), a_type);
    compute_ir_instruction(ir, ir_index, env_to_ir_index, b, (ctx, env), names, defs)?;
    ctx.remove_inner();
    *defs = old_defs;
    Ok(())
}

fn compute_ir_instruction(
    ir: &mut Ir,
    ir_index: usize,
    env_to_ir_index: &mut Vec<Option<usize>>,
    term: &Term<TermMeta>,
    ce: (&mut Context<TermMeta>, &Environment<TermMeta>),
    names: &Vec<String>,
    defs: &mut HashMap<usize, usize>,
) -> Result<(), ()> {
    match &term.inner {
        TermInner::Global(g) => compute_global(g, ir, ir_index, env_to_ir_index, ce.1, names),
        TermInner::Variable(v) => compute_variable(v, ir, ir_index, defs),
        TermInner::Lambda(a, b) => {
            compute_lambda((a, b), ir, ir_index, env_to_ir_index, ce, names, defs)
        }
        TermInner::Apply(a, b) => {
            compute_apply((a, b), ir, ir_index, env_to_ir_index, ce, names, defs)
        }
        TermInner::Let(a, b) => compute_let((a, b), ir, ir_index, env_to_ir_index, ce, names, defs),
        _ => Err(()),
    }
}

fn compute_ir_definition(
    ir: &mut Ir,
    ir_index: usize,
    env_index: usize,
    env_to_ir_index: &mut Vec<Option<usize>>,
    env: &Environment<TermMeta>,
    names: &Vec<String>,
) -> Result<(), ()> {
    env_to_ir_index[env_index] = Some(ir_index);
    let term = env.as_vec_ref()[env_index].0.as_ref().unwrap();
    ir.defs[ir_index] = Some(IrDef {
        env_index: Some(env_index),
        name: Some(names[env_index].clone()),
        export: false,
        params: 0,
        param_types: vec![],
        captures: HashSet::new(),
        capture_types: vec![],
        code: vec![],
    });
    let mut ctx = Context::new();
    let ce = (&mut ctx, env);
    compute_ir_instruction(
        ir,
        ir_index,
        env_to_ir_index,
        term,
        ce,
        names,
        &mut HashMap::new(),
    )
}

pub(crate) fn compute_initial_ir(
    indices: &[usize],
    env: &Environment<TermMeta>,
    names: &Vec<String>,
) -> Ir {
    let mut env_to_ir_index: Vec<Option<usize>> =
        (0..env.as_vec_ref().len()).map(|_| None).collect();
    let reserved_indices = indices.len();
    let mut r = Ir {
        types: vec![],
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
