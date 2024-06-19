use crate::kernel_err::TermMeta;
use crate::parser2::AbstractSyntaxTree;
use crate::*;

use async_recursion::async_recursion;
use rooster_kernel::Context;
use rooster_kernel::Environment;
use rooster_kernel::Term;
use rooster_kernel::TermInner;
use std::collections::HashMap;
use std::collections::HashSet;

// TODO: use CoW
fn compute_dependencies(
    ast: &AbstractSyntaxTree,
    locals: &HashSet<String>,
    filename: &str,
) -> HashSet<String> {
    match ast {
        AbstractSyntaxTree::Block(statements, _) => {
            let mut r = HashSet::new();
            let mut new_locals = locals.clone();
            for statement in statements {
                if let AbstractSyntaxTree::Assignment(name, _, is_definition, _, _) = statement {
                    if *is_definition {
                        if let AbstractSyntaxTree::Identifier(components, _) = &**name {
                            if components.len() > 1 {
                                panic!();
                            }
                            new_locals.insert(components[0].clone());
                        } else {
                            panic!();
                        }
                    }
                }
                r = &r | &compute_dependencies(statement, &new_locals, filename);
            }
            r
        }
        AbstractSyntaxTree::List(expressions, _) => expressions
            .iter()
            .map(|x| compute_dependencies(x, locals, filename))
            .reduce(|a, x| &a | &x)
            .unwrap_or_default(),
        AbstractSyntaxTree::Enclosed(inner, _, _) => compute_dependencies(inner, locals, filename),
        AbstractSyntaxTree::Identifier(components, _) => {
            let name = components.join("::");
            if name.get(..4) == Some("Type")
                && (name.len() == 4 || name[4..].parse::<usize>().is_ok())
            {
                return HashSet::new();
            }
            let mut r = HashSet::new();
            if !locals.contains(&name) {
                r.insert(name);
            }
            r
        }
        AbstractSyntaxTree::Assignment(_, value, _, def_type, _) => {
            let type_deps = match def_type {
                Some(dt) => compute_dependencies(dt, locals, filename),
                None => HashSet::new(),
            };
            &compute_dependencies(value, locals, filename) | &type_deps
        }
        AbstractSyntaxTree::Application(left, right, _) => {
            &compute_dependencies(left, locals, filename)
                | &compute_dependencies(right, locals, filename)
        }
        AbstractSyntaxTree::Forall(name, var_type, value, _) => {
            &compute_dependencies(var_type, locals, filename)
                | &match name {
                    Some(s) => {
                        let mut new_locals = locals.clone();
                        new_locals.insert(s.clone());
                        compute_dependencies(value, &new_locals, filename)
                    }
                    None => compute_dependencies(value, locals, filename),
                }
        }
        AbstractSyntaxTree::Lambda(name, var_type, value, _) => {
            &compute_dependencies(var_type, locals, filename)
                | &{
                    let mut new_locals = locals.clone();
                    new_locals.insert(name.clone());
                    compute_dependencies(value, &new_locals, filename)
                }
        }
        AbstractSyntaxTree::Operator(op, left, right, _) => {
            match &**op {
                "." => {
                    compute_dependencies(left, locals, filename)
                    // Can't compute dependency on method here,
                    // so it's added only when translating to term
                }
                _ => panic!(),
            }
        }
        AbstractSyntaxTree::Empty => HashSet::new(),
        AbstractSyntaxTree::Typed(_, _, range) => {
            report::send(Report {
                is_error: true,
                filename: filename.to_string(),
                offset: range.start,
                message: "Invalid statement".to_string(),
                note: None,
                help: None,
                labels: vec![(range.clone(), "not a statement".to_string())],
            });
            HashSet::new()
        }
        AbstractSyntaxTree::SpecialApp(_, _, _, range) => {
            report::send(Report {
                is_error: true,
                filename: filename.to_string(),
                offset: range.start,
                message: "Incomplete expression".to_string(),
                note: None,
                help: None,
                labels: vec![(range.clone(), "unfinished expression".to_string())],
            });
            HashSet::new()
        }
    }
}

fn find_var(
    left: &Term<TermMeta>,
    right: &Term<TermMeta>,
    n: usize,
) -> (Option<Term<TermMeta>>, bool) {
    match &left.inner {
        TermInner::Prop => (None, matches!(right.inner, TermInner::Prop)),
        TermInner::Type(n1) => {
            if let TermInner::Type(n2) = &right.inner {
                (None, *n1 == *n2)
            } else {
                (None, false)
            }
        }
        TermInner::Global(g1) => {
            if let TermInner::Global(g2) = &right.inner {
                (None, *g1 == *g2)
            } else {
                (None, false)
            }
        }
        TermInner::Variable(v1) => {
            if *v1 == n {
                let term = right.make_outer_by_n(n + 1).ok();
                (term, true)
            } else if let TermInner::Variable(v2) = &right.inner {
                (None, *v1 == *v2)
            } else {
                (None, false)
            }
        }
        TermInner::Forall(l1, r1) => {
            if let TermInner::Forall(l2, r2) = &right.inner {
                let (tl, bl) = find_var(&l1, &l2, n);
                let (tr, br) = find_var(&r1, &r2, n + 1);
                (tl.or(tr), bl && br)
            } else {
                (None, false)
            }
        }
        TermInner::Lambda(l1, r1) => {
            if let TermInner::Lambda(l2, r2) = &right.inner {
                let (tl, bl) = find_var(&l1, &l2, n);
                let (tr, br) = find_var(&r1, &r2, n + 1);
                (tl.or(tr), bl && br)
            } else {
                (None, false)
            }
        }
        TermInner::Apply(l1, r1) => {
            if let TermInner::Apply(l2, r2) = &right.inner {
                let (tl, bl) = find_var(&l1, &l2, n);
                let (tr, br) = find_var(&r1, &r2, n);
                (tl.or(tr), bl && br)
            } else {
                (None, false)
            }
        }
        TermInner::Let(l1, r1) => {
            if let TermInner::Let(l2, r2) = &right.inner {
                let (tl, bl) = find_var(&l1, &l2, n);
                let (tr, br) = find_var(&r1, &r2, n + 1);
                (tl.or(tr), bl && br)
            } else {
                (None, false)
            }
        }
    }
}

fn contains_var(t: &Term<TermMeta>, n: usize) -> bool {
    match &t.inner {
        TermInner::Prop => false,
        TermInner::Type(_) => false,
        TermInner::Global(_) => false,
        TermInner::Variable(v) => {
            if *v == n {
                true
            } else {
                false
            }
        }
        TermInner::Forall(l, r) => contains_var(&l, n) || contains_var(&r, n + 1),
        TermInner::Lambda(l, r) => contains_var(&l, n) || contains_var(&r, n + 1),
        TermInner::Apply(l, r) => contains_var(&l, n) || contains_var(&r, n),
        TermInner::Let(l, r) => contains_var(&l, n) || contains_var(&r, n + 1),
    }
}

fn get_forall_params<'a>(term: &'a Term<TermMeta>) -> Vec<&'a Term<TermMeta>> {
    match &term.inner {
        TermInner::Forall(l, r) => {
            let mut rest = get_forall_params(&r);
            rest.insert(0, l);
            rest
        }
        _ => vec![term],
    }
}

fn get_lambda_params<'a>(term: &'a Term<TermMeta>) -> Vec<&'a Term<TermMeta>> {
    match &term.inner {
        TermInner::Lambda(l, r) => {
            let mut rest = get_lambda_params(&r);
            rest.insert(0, l);
            rest
        }
        _ => vec![term],
    }
}

fn get_apply_params<'a>(term: &'a Term<TermMeta>) -> Vec<&'a Term<TermMeta>> {
    match &term.inner {
        TermInner::Apply(l, r) => {
            let mut rest = get_apply_params(&l);
            rest.push(r);
            rest
        }
        _ => vec![term],
    }
}

fn check_unnecessary_param(
    left_term: &Term<TermMeta>,
    right_term: &Term<TermMeta>,
    env: &Environment<TermMeta>,
    ctx: &mut Context<TermMeta>,
    filename: &str,
) {
    // (left_term) (right_term)
    if let TermInner::Apply(ref all, ref alr) = left_term.inner {
        let mut ll = all;
        let mut lr = alr;
        // (ll) (lr) (right_term)
        let mut n = 0;
        while left_term.meta.range == ll.meta.range {
            if let TermInner::Apply(ref nll, ref nlr) = ll.inner {
                ll = nll;
                lr = nlr;
                n += 1;
            } else {
                return;
            }
        }
        // (ll) (lr) ... (right_term)
        let tll = match ll.compute_type(env, ctx) {
            Ok(x) => x,
            Err(_) => return,
        };
        let ctll = match tll.normalize_in_ctx(env, ctx) {
            Ok(x) => x,
            Err(_) => return,
        };
        let mut params = get_forall_params(&ctll);
        params.pop();
        // (λ:params[0]. λ:params[1]. ...) (lr) ... (right_term)
        let tright = match right_term.compute_type(env, ctx) {
            Ok(x) => x,
            Err(_) => return,
        };
        let ctright = match tright.normalize_in_ctx(env, ctx) {
            Ok(x) => x,
            Err(_) => return,
        };
        let mut ntype_params = 0;
        for param in &params {
            // find param matching right term
            let ictright = match ctright.make_inner_by_n(ntype_params) {
                Ok(x) => x,
                Err(_) => return,
            };
            let mut values = vec![];
            for n in 0..ntype_params {
                // deduce omitted params
                let (value, equal) = find_var(param, &ictright, ntype_params - n - 1);
                ctx.add_inner(value.clone(), (*param).clone());
                values.push(value);
            }
            let cparam = match param.normalize_in_ctx(env, ctx) {
                Ok(x) => x,
                Err(_) => return,
            };
            for n in 0..ntype_params {
                ctx.remove_inner();
            }
            if cparam == ictright {
                if ntype_params > 0 && values.iter().all(|x| x.is_some()) {
                    report::send(Report {
                        is_error: false,
                        filename: filename.to_string(),
                        offset: ll.meta.range.start,
                        message: "Explicit parameter is unnecessary here".to_string(),
                        note: None,
                        help: Some("omit parameter".to_string()),
                        labels: vec![
                            (ll.meta.range.clone(), "Function here".to_string()),
                            (lr.meta.range.clone(), "Unnecessary parameter".to_string()),
                        ],
                    });
                }
                break;
            } else {
                ntype_params += 1;
            }
        }
        if ntype_params == params.len() {
            return;
        }
    }
}

fn check_reorder_prototype(
    term: &Term<TermMeta>,
    env: &Environment<TermMeta>,
    ctx: &mut Context<TermMeta>,
    filename: &str,
) {
    let cterm = match term.normalize_in_ctx(env, ctx) {
        Ok(x) => x,
        Err(_) => return,
    };
    let mut params = get_forall_params(&term);
    if params.len() == 1 {
        params = get_lambda_params(&term);
    }
    params.pop();
    if params.len() < 3 {
        return;
    }
    for n in 2..params.len() {
        if contains_var(params[n], n - 1)
            && (1..n).all(|x| {
                !contains_var(params[n], n - x - 1)
                    && !contains_var(params[x], x - 1)
                    && !contains_var(params[x], x)
            })
            && term.meta.range.start <= params[0].meta.range.start
            && term.meta.range.end >= params[n].meta.range.end
        {
            report::send(Report {
                is_error: false,
                filename: filename.to_string(),
                offset: term.meta.range.start,
                message: "Parameters in definition could be reordered to allow omission"
                    .to_string(),
                note: None,
                help: Some("reorder parameters".to_string()),
                labels: vec![
                    (
                        params[0].meta.range.clone(),
                        "To allow omitting this".to_string(),
                    ),
                    (
                        params[1].meta.range.start..params[n - 1].meta.range.end,
                        "Place this".to_string(),
                    ),
                    (params[n].meta.range.clone(), "After this".to_string()),
                ],
            });
        }
    }
}

#[async_recursion]
pub(crate) async fn convert_to_term_rec(
    ast: &AbstractSyntaxTree,
    locals: &HashMap<String, usize>,
    level: usize,
    filename: &str,
    env: &mut Environment<TermMeta>,
    ctx: &mut Context<TermMeta>,
) -> Result<Term<TermMeta>, ()> {
    match ast {
        AbstractSyntaxTree::Block(statements, _) => {
            let mut rv = vec![];
            let mut metas = vec![];
            let mut new_locals = locals.clone();
            let mut new_level = level;
            let mut n = 0;
            for statement in statements {
                match statement {
                    AbstractSyntaxTree::Assignment(
                        name,
                        value,
                        is_definition,
                        def_type,
                        assignment_range,
                    ) => {
                        if def_type.is_some() {
                            panic!();
                        }
                        if *is_definition {
                            if let AbstractSyntaxTree::Identifier(components, _) = &**name {
                                if components.len() > 1 {
                                    panic!();
                                }
                                let value = convert_to_term_rec(
                                    value,
                                    &new_locals,
                                    new_level,
                                    filename,
                                    env,
                                    ctx,
                                )
                                .await?;
                                let tvalue = value.compute_type(env, ctx).map_err(|x| {
                                    kernel_err::report(x, filename);
                                    ()
                                })?;
                                ctx.add_inner(Some(value.clone()), tvalue);
                                rv.push(value);
                                metas.push(Arc::new(TermMeta {
                                    range: assignment_range.clone(),
                                    name: Some(components.join("::")),
                                    filename: filename.to_string(),
                                }));
                                new_locals.insert(components[0].clone(), new_level);
                                new_level += 1;
                            } else {
                                panic!();
                            }
                        } else {
                            todo!();
                        }
                    }
                    AbstractSyntaxTree::Empty => {}
                    _ => {
                        if n != statements.len() - 1 {
                            let range = statement.get_range();
                            report::send(Report {
                                is_error: false,
                                filename: filename.to_string(),
                                offset: range.start,
                                message: "This statement has no effect".to_string(),
                                note: None,
                                help: None,
                                labels: vec![(
                                    range.clone(),
                                    "Not a definition, assignment, or last expression in scope"
                                        .to_string(),
                                )],
                            });
                            // TODO: change this when global instrumenting is in place
                        } else {
                            rv.push(
                                convert_to_term_rec(
                                    statement,
                                    &new_locals,
                                    new_level,
                                    filename,
                                    env,
                                    ctx,
                                )
                                .await?,
                            );
                        }
                    }
                }
                n += 1;
            }
            let mut r = rv.pop().unwrap();
            for value in rv.into_iter().rev() {
                ctx.remove_inner();
                let meta = metas.pop().unwrap();
                r = (TermInner::Let(Box::new(value), Box::new(r)), &meta).into();
                n += 1;
            }
            Ok(r)
        }
        AbstractSyntaxTree::List(_expressions, _) => panic!(),
        AbstractSyntaxTree::Enclosed(inner, _, _) => {
            convert_to_term_rec(inner, locals, level, filename, env, ctx).await
        }
        AbstractSyntaxTree::Identifier(components, identifier_range) => {
            let name = components.join("::");
            let meta = Arc::new(TermMeta {
                range: identifier_range.clone(),
                name: Some(name.clone()),
                filename: filename.to_string(),
            });
            if name.get(..4) == Some("Type") {
                if name.len() == 4 {
                    return Ok((TermInner::Prop, &meta).into());
                }
                if let Ok(n) = name[4..].parse::<usize>() {
                    if n > 0 {
                        return Ok((TermInner::Type(n - 1), &meta).into());
                    } else {
                        panic!("No Type0");
                    }
                }
            }
            match locals.get(&name) {
                Some(n) => Ok((TermInner::Variable(level - n - 1), &meta).into()),
                None => {
                    match kernel::get_global_index(&path::make_absolute(&name, filename)).await {
                        Ok(n) => {
                            debug_assert!(env.contains_global(n));
                            Ok((TermInner::Global(n), &meta).into())
                        }
                        Err(_) => {
                            report::send(Report {
                                is_error: true,
                                filename: filename.to_string(),
                                offset: identifier_range.start,
                                message: "Undefined identifier".to_string(),
                                note: None,
                                help: None,
                                labels: vec![(
                                    identifier_range.clone(),
                                    "No definition found".to_string(),
                                )],
                            });
                            Err(())
                        }
                    }
                }
            }
        }
        AbstractSyntaxTree::Assignment(_, _, _, _, _) => panic!(),
        AbstractSyntaxTree::Application(left, right, application_range) => {
            let mut left_term =
                convert_to_term_rec(left, locals, level, filename, env, ctx).await?;
            let right_term = convert_to_term_rec(right, locals, level, filename, env, ctx).await?;
            // (left_term) (right_term)
            if let AbstractSyntaxTree::Identifier(components, identifier_range) = &**left {
                if components.len() > 1 {
                    let tright = right_term.compute_type(env, ctx).map_err(|x| {
                        kernel_err::report(x, filename);
                        ()
                    })?;
                    let params = get_apply_params(&tright);
                    if let Some(type_name) = &params[0].meta.name {
                        if components
                            .iter()
                            .take(components.len() - 1)
                            .map(|x| x.clone())
                            .collect::<Vec<_>>()
                            .join("::")
                            == *type_name
                        {
                            report::send(Report {
                                is_error: false,
                                filename: filename.to_string(),
                                offset: application_range.start,
                                message: "Method call syntax can make expression more readable"
                                    .to_string(),
                                note: None,
                                help: Some(format!(
                                    "Use `<self>.{}` instead",
                                    components.last().unwrap()
                                )),
                                labels: vec![
                                    (identifier_range.clone(), "Explicit method name".to_string()),
                                    (
                                        right_term.meta.range.clone(),
                                        "`self` parameter here".to_string(),
                                    ),
                                ],
                            });
                        }
                    }
                }
            }
            let tleft = left_term.compute_type(env, ctx).map_err(|x| {
                kernel_err::report(x, filename);
                ()
            })?;
            let ctleft = tleft.normalize_in_ctx(env, ctx).map_err(|x| {
                kernel_err::report(x, filename);
                ()
            })?;
            let mut params = get_forall_params(&ctleft);
            params.pop();
            // (λ:params[0]. λ:params[1]. ...) (right_term)
            if params.len() >= 2 {
                // at least one for the omitted param, one for the explicit one
                let tright = right_term.compute_type(env, ctx).map_err(|x| {
                    kernel_err::report(x, filename);
                    ()
                })?;
                let ctright = tright.normalize_in_ctx(env, ctx).map_err(|x| {
                    kernel_err::report(x, filename);
                    ()
                })?;
                let mut ntype_params = 0;
                for param in &params {
                    // find param matching right term
                    let ictright = ctright.make_inner_by_n(ntype_params).map_err(|x| {
                        kernel_err::report(x, filename);
                        ()
                    })?;
                    let mut values = vec![];
                    for n in 0..ntype_params {
                        // deduce omitted params
                        let (value, equal) = find_var(param, &ictright, ntype_params - n - 1);
                        ctx.add_inner(value.clone(), (*param).clone());
                        values.push(value);
                    }
                    let cparam = param.normalize_in_ctx(env, ctx).map_err(|x| {
                        kernel_err::report(x, filename);
                        ()
                    })?;
                    for n in 0..ntype_params {
                        ctx.remove_inner();
                    }
                    if cparam == ictright {
                        for n in 0..ntype_params {
                            match &values[n] {
                                Some(v) => {
                                    let meta = left_term.meta.clone();
                                    let iv = v.make_outer_by_n(n).map_err(|x| {
                                        kernel_err::report(x, filename);
                                        ()
                                    })?;
                                    left_term = (
                                        TermInner::Apply(Box::new(left_term), Box::new(iv)),
                                        &meta,
                                    )
                                        .into();
                                }
                                None => panic!(),
                            }
                        }
                        break;
                    } else {
                        ntype_params += 1;
                    }
                }
                if ntype_params == params.len() {
                    panic!();
                }
            }
            check_unnecessary_param(&left_term, &right_term, env, ctx, filename);
            let meta = Arc::new(TermMeta {
                range: application_range.clone(),
                name: None,
                filename: filename.to_string(),
            });
            Ok((
                TermInner::Apply(Box::new(left_term), Box::new(right_term)),
                &meta,
            )
                .into())
        }
        AbstractSyntaxTree::Forall(name, var_type, value, forall_range) => {
            let type_term =
                convert_to_term_rec(var_type, locals, level, filename, env, ctx).await?;
            ctx.add_inner(None, type_term.clone());
            let mut new_locals = locals.clone();
            if let Some(s) = name {
                new_locals.insert(s.to_string(), level);
            }
            let mut new_level = level;
            new_level += 1;
            let value_term =
                convert_to_term_rec(value, &new_locals, new_level, filename, env, ctx).await?;
            ctx.remove_inner();
            let meta = Arc::new(TermMeta {
                range: forall_range.clone(),
                name: name.clone(),
                filename: filename.to_string(),
            });
            let r = (
                TermInner::Forall(Box::new(type_term), Box::new(value_term)),
                &meta,
            )
                .into();
            check_reorder_prototype(&r, env, ctx, filename);
            Ok(r)
        }
        AbstractSyntaxTree::Lambda(name, var_type, value, lambda_range) => {
            let type_term =
                convert_to_term_rec(var_type, locals, level, filename, env, ctx).await?;
            ctx.add_inner(None, type_term.clone());
            let mut new_locals = locals.clone();
            new_locals.insert(name.to_string(), level);
            let mut new_level = level;
            new_level += 1;
            let value_term =
                convert_to_term_rec(value, &new_locals, new_level, filename, env, ctx).await?;
            ctx.remove_inner();
            let meta = Arc::new(TermMeta {
                range: lambda_range.clone(),
                name: Some(name.clone()),
                filename: filename.to_string(),
            });
            let r = (
                TermInner::Lambda(Box::new(type_term), Box::new(value_term)),
                &meta,
            )
                .into();
            check_reorder_prototype(&r, env, ctx, filename);
            Ok(r)
        }
        AbstractSyntaxTree::Operator(op, left, right, operator_range) => match &**op {
            "." => {
                if let AbstractSyntaxTree::Identifier(components, identifier_range) = &**right {
                    let left_term =
                        convert_to_term_rec(left, locals, level, filename, env, ctx).await?;
                    let tleft = left_term.compute_type(env, ctx).map_err(|x| {
                        kernel_err::report(x, filename);
                        ()
                    })?;
                    let params = get_apply_params(&tleft);
                    if let Some(type_name) = &params[0].meta.name {
                        let mut method_components = vec![type_name.clone()];
                        method_components.append(
                            &mut components
                                .into_iter()
                                .map(|x| x.clone())
                                .collect::<Vec<String>>(),
                        );
                        let name = method_components.join("::");
                        let ident = AbstractSyntaxTree::Identifier(
                            method_components,
                            identifier_range.clone(),
                        );
                        let r = AbstractSyntaxTree::Application(
                            Box::new(ident),
                            Box::new(*left.clone()),
                            identifier_range.clone(),
                        );
                        let path = path::make_absolute(&name, &filename);
                        let new_env = kernel::update_environment(env.clone(), &path).await?;
                        *env = new_env;
                        convert_to_term_rec(&r, locals, level, filename, env, ctx).await
                    } else {
                        panic!();
                    }
                } else {
                    panic!();
                }
            }
            _ => panic!(),
        },
        _ => Err(()),
    }
}

pub(crate) async fn convert_to_term(
    ast: &AbstractSyntaxTree,
    filename: &str,
    env: &mut Environment<TermMeta>,
) -> Result<Term<TermMeta>, ()> {
    let mut ctx = Context::new();
    convert_to_term_rec(ast, &HashMap::new(), 0, filename, env, &mut ctx).await
}

fn dependency_loader(
    logical_path: &str,
) -> Pin<Box<dyn Future<Output = Result<HashSet<String>, ()>> + Send + '_>> {
    let logical_path_string = logical_path.to_string();
    Box::pin(async move {
        let (ast, filename) = get_ast(&logical_path_string).await?;
        let deps = compute_dependencies(ast, &HashSet::new(), &filename);
        let (type_ast, _) = get_type_ast(&logical_path_string).await?;
        let type_deps = match type_ast {
            Some(ast) => compute_dependencies(ast, &HashSet::new(), &filename),
            None => HashSet::new(),
        };
        Ok((&deps | &type_deps)
            .iter()
            .map(|dep| path::make_absolute(dep, &filename))
            .collect())
    })
}

// The global cache storing dependencies for each top-level definition
static DEPENDENCY__CACHE: cache::Cache<HashSet<String>> = cache::Cache::new(dependency_loader as _);

// Get direct dependencies of a top-level definition.
pub(crate) async fn get_direct_dependencies(
    logical_path: &str,
) -> Result<Arc<HashSet<String>>, ()> {
    DEPENDENCY__CACHE.get(logical_path).await
}

// Get all dependencies dependencies of a top-level definition.
#[async_recursion]
pub(crate) async fn get_all_dependencies(logical_path: &str) -> Result<Arc<HashSet<String>>, ()> {
    let deps = get_direct_dependencies(logical_path).await?;
    let mut r = HashSet::new();
    for dep in &*deps {
        let more_deps = get_all_dependencies(dep).await?;
        r = &r | &more_deps;
    }
    r = &r | &deps;
    Ok(Arc::new(r))
}
