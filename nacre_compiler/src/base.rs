use crate::typing;
use crate::{Ir, IrDef, IrInstr, IrLoc, IrType};
use nacre_kernel::Term;
use nacre_kernel::TermInner;
use nacre_kernel::{Context, Environment};
use nacre_parser::TermMeta;
use std::collections::HashSet;

fn get_def(defs: &[usize], n: isize) -> Option<usize> {
    if n < 0 || n as usize >= defs.len() {
        None
    } else {
        defs.get(defs.len() - 1 - n as usize).copied()
    }
}

fn compute_inductive_const(
    ir: &mut Ir,
    ir_index: usize,
    term: &Term<TermMeta>,
    ce: (&mut Context<TermMeta>, &Environment<TermMeta>),
    defs: &Vec<usize>,
) -> Result<(), ()> {
    let (ctx, env) = ce;
    let mut variant_count = 0;
    let term_type = term.compute_type(env, ctx).unwrap();
    let value_type = typing::compute_type(&term_type, &mut ir.types, ctx, env);
    let mut t = if let TermInner::Lambda(_, b) = &term.inner {
        b
    } else {
        panic!()
    }
    .normalize_in_ctx(env, ctx)
    .unwrap();
    let (variant, fields) = 'outer: loop {
        match &t.inner {
            TermInner::Variable(v) => {
                assert!(*v < variant_count);
                break (variant_count - v - 1, vec![]);
            }
            TermInner::Lambda(a, b) => {
                if let TermInner::Prop = a.inner {
                    for _ in 0..variant_count {
                        ctx.remove_inner();
                    }
                    return Err(());
                }
                variant_count += 1;
                ctx.add_inner(None, (**a).clone());
                t = *b.clone();
            }
            TermInner::Apply(a, b) => {
                let ta = a;
                let field_terms = vec![b];
                loop {
                    match ta.inner {
                        TermInner::Variable(v) => {
                            assert!(v < variant_count);
                            let fields = field_terms
                                .into_iter()
                                .map(|ft| match ft.inner {
                                    TermInner::Variable(fv) => fv - variant_count - 1,
                                    _ => todo!(),
                                })
                                .collect();
                            break 'outer (variant_count - v - 1, fields);
                        }
                        _ => todo!(),
                    }
                }
            }
            _ => {
                if !t.convert(env, ctx).unwrap() {
                    for _ in 0..variant_count {
                        ctx.remove_inner();
                    }
                    return Err(());
                }
            }
        }
    };
    let field_indices: Vec<_> = fields
        .into_iter()
        .map(|f| {
            compute_variable(&f, ir, ir_index, &mut defs.clone()).unwrap();
            ir.defs[ir_index].as_ref().unwrap().code.len() - 1
        })
        .collect();
    let def = ir.defs[ir_index].as_mut().unwrap();
    assert!(variant_count > 1); // TODO: handle pure structs
    // TODO: move computation into crate::typing
    for _ in 0..variant_count {
        ctx.remove_inner();
    }
    let inner_value = match field_indices.len() {
        0 => None,
        1 => Some(field_indices[0]),
        _ => todo!(),
    };
    def.code.push(IrLoc {
        instr: IrInstr::Enum(variant, inner_value),
        value_type,
    });
    Ok(())
}

fn compute_global(
    g: &usize,
    ir: &mut Ir,
    ir_index: usize,
    env_to_ir_index: &mut Vec<Option<usize>>,
    ce: (&mut Context<TermMeta>, &Environment<TermMeta>),
    names: &Vec<String>,
) -> Result<(), ()> {
    let (ctx, env) = ce;
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
    let term_type = &env.as_vec_ref()[*g].1;
    let mut closure_type = typing::compute_type(term_type, &mut ir.types, ctx, env);
    if let IrType::Closure(_, _) = ir.types[closure_type.unwrap()].as_ref().unwrap() {
    } else {
        let new_type = IrType::Closure(vec![], closure_type);
        closure_type = Some(typing::add_type(new_type, &mut ir.types));
    }
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

fn compute_variable(v: &usize, ir: &mut Ir, ir_index: usize, defs: &mut [usize]) -> Result<(), ()> {
    let def = ir.defs[ir_index].as_mut().unwrap();
    match get_def(defs, *v as isize) {
        // variable corresponds to lambda parameter
        Some(usize::MAX) => def.code.push(IrLoc {
            instr: IrInstr::Param(0),
            value_type: def.param_types[0],
        }),
        // variable corresponds to let definition
        Some(n) => def.code.push(IrLoc {
            instr: IrInstr::Move(n),
            value_type: def.code[n].value_type,
        }),
        // variable corresponds to something else (capture)
        _ => {
            def.code.push(IrLoc {
                instr: IrInstr::Capture(*v - defs.len()),
                value_type: def.capture_types[*v - defs.len()],
            });
            def.captures.insert(*v - defs.len());
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
    defs: &mut Vec<usize>,
) -> Result<(), ()> {
    let (a, b) = lam;
    let (ctx, env) = ce;
    let term: Term<TermMeta> = (
        TermInner::Lambda(a.clone().into(), b.clone().into()),
        &TermMeta::default().into(),
    )
        .into();
    let term_type = term.compute_type(env, ctx).unwrap();
    let mut closure_type = typing::compute_type(&term_type, &mut ir.types, ctx, env);
    let def = ir.defs[ir_index].as_mut().unwrap();
    let param_type = typing::compute_type(a, &mut ir.types, ctx, env);
    let mut defs2o = vec![];
    let defs2 = &mut defs2o;
    defs2.push(usize::MAX); // innermost definition is now a parameter
    if let TermInner::Prop = a.inner {
        if compute_inductive_const(ir, ir_index, &term, (ctx, env), defs).is_err() {
            ctx.add_inner(None, a.clone());
            defs.push(usize::MAX - 1);
            compute_ir_instruction(ir, ir_index, env_to_ir_index, b, (ctx, env), names, defs)?;
            defs.pop();
            ctx.remove_inner();
        }
    } else if def.params == 0 && def.code.is_empty() {
        // outer body of global definition
        ctx.add_inner(None, a.clone());
        def.params = 1;
        def.param_types = vec![param_type];
        compute_ir_instruction(ir, ir_index, env_to_ir_index, b, (ctx, env), names, defs2)?;
        ctx.remove_inner();
    } else {
        // inner lambda, create anonymous global definition
        ctx.add_inner(None, a.clone());
        let mut capture_types = vec![];
        for i in 0..defs.len() {
            match get_def(defs, i as isize) {
                None => break,
                Some(n) => {
                    if n == usize::MAX {
                        capture_types.push(def.param_types[0]);
                    } else if n == usize::MAX - 1 {
                        capture_types.push(None);
                    } else {
                        capture_types.push(def.code[n].value_type);
                    }
                }
            }
        }
        capture_types.append(&mut def.capture_types.clone());
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
            defs2,
        )?;
        let closure = ir.defs[closure_ir_index].as_ref().unwrap();
        if let IrType::Closure(_, _) = ir.types[closure_type.unwrap()].as_ref().unwrap() {
        } else {
            let new_type = IrType::Closure(vec![], closure_type);
            closure_type = Some(typing::add_type(new_type, &mut ir.types));
        }
        let n_params = closure.params;
        let return_type = closure.code.last().unwrap().value_type;
        // pass down captures
        let captures = closure.captures.clone();
        let def = ir.defs[ir_index].as_mut().unwrap();
        for c in captures.iter() {
            match get_def(defs, *c as isize) {
                Some(n) => {
                    if n == usize::MAX {
                        // capture parameter
                        def.code.push(IrLoc {
                            instr: IrInstr::Param(0),
                            value_type: def.param_types[0],
                        })
                    } else if n == usize::MAX - 1 {
                        // capture nothing
                    } else {
                        // capture local value
                        def.code.push(IrLoc {
                            instr: IrInstr::Move(n),
                            value_type: def.code[n].value_type,
                        })
                    }
                }
                None => {
                    // capture enclosing capture
                    def.code.push(IrLoc {
                        instr: IrInstr::Capture(c - defs2.len()),
                        value_type: def.capture_types[c - defs2.len()],
                    });
                    def.captures.insert(c - defs2.len());
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
        ctx.remove_inner();
    }
    Ok(())
}

fn compute_apply(
    app: (&Term<TermMeta>, &Term<TermMeta>),
    ir: &mut Ir,
    ir_index: usize,
    env_to_ir_index: &mut Vec<Option<usize>>,
    ce: (&mut Context<TermMeta>, &Environment<TermMeta>),
    names: &Vec<String>,
    defs: &mut Vec<usize>,
) -> Result<(), ()> {
    let (a, b) = app;
    let (ctx, env) = ce;
    let b_type = b.compute_type(env, ctx).unwrap();
    let b_ir_type = typing::compute_type(&b_type, &mut ir.types, ctx, env);
    let mut term = Term {
        inner: TermInner::Apply(a.clone().into(), b.clone().into()),
        meta: TermMeta::default().into(),
    };
    let term_type = term.compute_type(env, ctx).unwrap();
    let value_type = typing::compute_type(&term_type, &mut ir.types, ctx, env);
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
                def.code.push(IrLoc {
                    instr: IrInstr::Apply(a_index, vec![b_index]),
                    value_type,
                });
            }
        }
        None => {
            let a_type = a.compute_type(env, ctx).unwrap();
            let a_ir_type = typing::compute_type(&a_type, &mut ir.types, ctx, env);
            // TODO move logic into `typing`
            let is = a_ir_type.is_some()
                && matches!(
                    ir.types[a_ir_type.unwrap()].as_ref().unwrap(),
                    IrType::Enum(_) | IrType::Struct(_)
                );
            if is {
                // either an inductive or a generic type being specialized
                let def = ir.defs[ir_index].as_mut().unwrap();
                let len = def.code.len();
                compute_ir_instruction(ir, ir_index, env_to_ir_index, a, (ctx, env), names, defs)?;
                let def = ir.defs[ir_index].as_mut().unwrap();
                if def.code.len() > len && typing::is_generic(&a_type, &ir.types, ctx, env) {
                    def.code.last_mut().unwrap().value_type = value_type;
                }
            } else {
                term.convert(env, ctx).unwrap();
                compute_ir_instruction(
                    ir,
                    ir_index,
                    env_to_ir_index,
                    &term,
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
    defs: &mut Vec<usize>,
) -> Result<(), ()> {
    let (a, b) = lt;
    let (ctx, env) = ce;
    compute_ir_instruction(ir, ir_index, env_to_ir_index, a, (ctx, env), names, defs)?;
    let a_index = ir.defs[ir_index].as_ref().unwrap().code.len() - 1;
    defs.push(a_index);
    let a_type = a.compute_type(env, ctx).unwrap();
    ctx.add_inner(Some(a.clone()), a_type);
    compute_ir_instruction(ir, ir_index, env_to_ir_index, b, (ctx, env), names, defs)?;
    ctx.remove_inner();
    defs.pop();
    Ok(())
}

fn compute_ir_instruction(
    ir: &mut Ir,
    ir_index: usize,
    env_to_ir_index: &mut Vec<Option<usize>>,
    term: &Term<TermMeta>,
    ce: (&mut Context<TermMeta>, &Environment<TermMeta>),
    names: &Vec<String>,
    defs: &mut Vec<usize>,
) -> Result<(), ()> {
    match &term.inner {
        TermInner::Global(g) => compute_global(g, ir, ir_index, env_to_ir_index, ce, names),
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
    compute_ir_instruction(ir, ir_index, env_to_ir_index, term, ce, names, &mut vec![])
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
        if let &mut Some(ref mut d) = &mut r.defs[n] {
            d.export = true;
        }
    }
    r
}
