use crate::kernel_err::TermMeta;
use crate::*;

use async_recursion::async_recursion;
use once_cell::sync::Lazy;
use nacre_ast::AbstractSyntaxTree;
use nacre_kernel::Context;
use nacre_kernel::Environment;
use nacre_kernel::Term;
use nacre_kernel::TermInner;
use std::collections::HashMap;
use std::collections::HashSet;
use std::ops::Range;

pub(crate) fn compute_imports(ast: &AbstractSyntaxTree) -> HashMap<String, Vec<String>> {
    let mut r = HashMap::new();
    match ast {
        AbstractSyntaxTree::Block(statements, _) => {
            for statement in statements {
                r.extend(compute_imports(statement));
            }
        }
        AbstractSyntaxTree::Import(components, _) => {
            let mut components = components.clone();
            let key = components.pop().unwrap();
            let value = components;
            r.insert(key, value);
        }
        _ => {}
    }
    r
}

pub(crate) fn apply_imports(name: String, imports: &HashMap<String, Vec<String>>) -> String {
    let components = name.split("::").collect::<Vec<_>>();
    match imports.get(components[0]) {
        Some(prefix) => prefix.join("::") + "::" + &name,
        None => name,
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
                let (tl, bl) = find_var(l1, l2, n);
                let (tr, br) = find_var(r1, r2, n + 1);
                (tl.or(tr), bl && br)
            } else {
                (None, false)
            }
        }
        TermInner::Lambda(l1, r1) => {
            if let TermInner::Lambda(l2, r2) = &right.inner {
                let (tl, bl) = find_var(l1, l2, n);
                let (tr, br) = find_var(r1, r2, n + 1);
                (tl.or(tr), bl && br)
            } else {
                (None, false)
            }
        }
        TermInner::Apply(l1, r1) => {
            if let TermInner::Apply(l2, r2) = &right.inner {
                let (tl, bl) = find_var(l1, l2, n);
                let (tr, br) = find_var(r1, r2, n);
                (tl.or(tr), bl && br)
            } else {
                (None, false)
            }
        }
        TermInner::Let(l1, r1) => {
            if let TermInner::Let(l2, r2) = &right.inner {
                let (tl, bl) = find_var(l1, l2, n);
                let (tr, br) = find_var(r1, r2, n + 1);
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
        TermInner::Variable(v) => *v == n,
        TermInner::Forall(l, r) => contains_var(l, n) || contains_var(r, n + 1),
        TermInner::Lambda(l, r) => contains_var(l, n) || contains_var(r, n + 1),
        TermInner::Apply(l, r) => contains_var(l, n) || contains_var(r, n),
        TermInner::Let(l, r) => contains_var(l, n) || contains_var(r, n + 1),
    }
}

fn get_forall_params(term: &Term<TermMeta>) -> Vec<&Term<TermMeta>> {
    match &term.inner {
        TermInner::Forall(l, r) => {
            let mut rest = get_forall_params(r);
            rest.insert(0, l);
            rest
        }
        _ => vec![term],
    }
}

fn get_lambda_params(term: &Term<TermMeta>) -> Vec<&Term<TermMeta>> {
    match &term.inner {
        TermInner::Lambda(l, r) => {
            let mut rest = get_lambda_params(r);
            rest.insert(0, l);
            rest
        }
        _ => vec![term],
    }
}

fn get_apply_params(term: &Term<TermMeta>) -> Vec<&Term<TermMeta>> {
    match &term.inner {
        TermInner::Apply(l, r) => {
            let mut rest = get_apply_params(l);
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
        while left_term.meta.range == ll.meta.range {
            if let TermInner::Apply(ref nll, ref nlr) = ll.inner {
                ll = nll;
                lr = nlr;
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
                let (value, _) = find_var(param, &ictright, ntype_params - n - 1);
                ctx.add_inner(value.clone(), (*param).clone());
                values.push(value);
            }
            let cparam = match param.normalize_in_ctx(env, ctx) {
                Ok(x) => x,
                Err(_) => return,
            };
            for _ in 0..ntype_params {
                ctx.remove_inner();
            }
            if cparam == ictright {
                if ntype_params > 0
                    && values.iter().all(|x| x.is_some())
                    && (0..params.len() + 1).all(|x| !contains_var(params[0], x))
                {
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
    }
}

fn check_reorder_prototype(
    term: &Term<TermMeta>,
    _env: &Environment<TermMeta>,
    _ctx: &mut Context<TermMeta>,
    filename: &str,
) {
    let mut params = get_forall_params(term);
    if params.len() == 1 {
        params = get_lambda_params(term);
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

fn check_variable_name(
    name: &str,
    type_term: &Term<TermMeta>,
    env: &mut Environment<TermMeta>,
    ctx: &mut Context<TermMeta>,
    range: &Range<usize>,
    filename: &str,
) -> Result<(), ()> {
    let ttype = match type_term.compute_type(env, ctx) {
        Ok(x) => x,
        Err(_) => return Ok(()),
    };
    let cttype = match ttype.normalize_in_ctx(env, ctx) {
        Ok(x) => x,
        Err(_) => return Ok(()),
    };
    let should_be_title_case = !matches!(cttype.inner, TermInner::Prop);
    let mut is_symbolic = false;
    let mut is_snake_case = false;
    let mut is_mixed_case = false;
    let mut is_not_title_case = false;
    let mut start = true;
    for ch in name.chars() {
        if !ch.is_alphanumeric() && ch != '_' {
            is_symbolic = true;
        } else if ch == '_' {
            is_snake_case = true;
        } else if ch.is_uppercase() {
            is_mixed_case = true;
        } else if ch.is_alphabetic() && start {
            is_not_title_case = true;
        }
        start = false;
    }
    if is_symbolic {
        report::send(Report {
            is_error: false,
            filename: filename.to_string(),
            offset: range.start,
            message: "Identifier contains symbol".to_string(),
            note: Some(
                "Identifiers should comprise only alphanumeric characters and underscore"
                    .to_string(),
            ),
            help: None,
            labels: vec![(range.clone(), "Contains symbol".to_string())],
        });
    }
    let is_title_case = is_mixed_case & !is_not_title_case & !is_snake_case;
    if is_title_case != should_be_title_case {
        report::send(Report {
            is_error: false,
            filename: filename.to_string(),
            offset: range.start,
            message: "Identifier uses incorrect capitalization".to_string(),
            note: Some(
                (if should_be_title_case {
                    "Type identifiers should use `TitleCase`"
                } else {
                    "Object identifiers should use `snake_case`"
                })
                .to_string(),
            ),
            help: None,
            labels: vec![(
                range.clone(),
                format!(
                    "Uses `{}`",
                    if is_mixed_case && is_snake_case {
                        "Mixed_Case"
                    } else if is_snake_case || !is_mixed_case {
                        "snake_case"
                    } else if is_not_title_case {
                        "camelCase"
                    } else {
                        "TitleCase"
                    }
                )
                .to_string(),
            )],
        });
    }
    if name.len() >= 4 && name[0..4] == *"Type" && name[4..].chars().all(|ch| ch.is_ascii_digit()) {
        report::send(Report {
            is_error: true,
            filename: filename.to_string(),
            offset: range.start,
            message: "Identifier is a reserved type name".to_string(),
            note: None,
            help: None,
            labels: vec![(range.clone(), "Defined in the `Type` hierarchy".to_string())],
        });
        return Err(());
    }
    if const { Lazy::new(|| HashSet::from(["fn", "impl", "let", "type"])) }.contains(name) {
        report::send(Report {
            is_error: true,
            filename: filename.to_string(),
            offset: range.start,
            message: "Identifier is a reserved keyword".to_string(),
            note: None,
            help: None,
            labels: vec![(range.clone(), "Reserved".to_string())],
        });
        return Err(());
    }
    Ok(())
}

#[async_recursion]
pub(crate) async fn convert_to_term_rec(
    ast: &AbstractSyntaxTree,
    locals: &HashMap<String, usize>,
    level: usize,
    filename: &str,
    env: &mut Environment<TermMeta>,
    ctx: &mut Context<TermMeta>,
    imports: &HashMap<String, Vec<String>>,
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
                        _nparams,
                        assignment_range,
                    ) => {
                        if def_type.is_some() {
                            panic!();
                        }
                        if *is_definition {
                            if let AbstractSyntaxTree::Identifier(components, identifier_range) =
                                &**name
                            {
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
                                    imports,
                                )
                                .await?;
                                let tvalue = value.compute_type(env, ctx).map_err(|x| {
                                    kernel_err::report(x, filename);
                                })?;
                                let name = components.join("::");
                                check_variable_name(
                                    &name,
                                    &tvalue,
                                    env,
                                    ctx,
                                    identifier_range,
                                    filename,
                                )?;
                                ctx.add_inner(Some(value.clone()), tvalue);
                                rv.push(value);
                                metas.push(Arc::new(TermMeta {
                                    range: assignment_range.clone(),
                                    name: Some(name),
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
                                    imports,
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
                    let global_name = apply_imports(name, imports);
                    if !path::check_path_access(&global_name, filename) {
                        report::send(Report {
                            is_error: true,
                            filename: filename.to_string(),
                            offset: identifier_range.start,
                            message: format!("Identifier `{}` is private", &global_name),
                            note: Some("Paths containing a leading underscore are only visible from within their enclosing scope".to_string()),
                            help: None,
                            labels: vec![(
                                identifier_range.clone(),
                                "Private identifier".to_string(),
                            )],
                        });
                        return Err(());
                    }
                    let path = path::make_absolute(&global_name, filename);
                    let (new_env, index) =
                        match kernel::update_environment(env.clone(), &path).await {
                            Ok(env) => env,
                            Err(_) => {
                                report::send(Report {
                                    is_error: true,
                                    filename: filename.to_string(),
                                    offset: identifier_range.start,
                                    message: format!("Undefined identifier `{}`", path),
                                    note: None,
                                    help: None,
                                    labels: vec![(
                                        identifier_range.clone(),
                                        "No definition found".to_string(),
                                    )],
                                });
                                return Err(());
                            }
                        };
                    *env = new_env;
                    debug_assert!(env.contains_global(index));
                    Ok((TermInner::Global(index), &meta).into())
                }
            }
        }
        AbstractSyntaxTree::Assignment(_, _, _, _, _, _) => panic!(),
        AbstractSyntaxTree::Application(left, right, application_range) => {
            let mut left_term =
                convert_to_term_rec(left, locals, level, filename, env, ctx, imports).await?;
            let right_term =
                convert_to_term_rec(right, locals, level, filename, env, ctx, imports).await?;
            // (left_term) (right_term)
            if let AbstractSyntaxTree::Identifier(components, identifier_range) = &**left {
                if components.len() > 1 && identifier_range.end < right_term.meta.range.start {
                    let tright = right_term.compute_type(env, ctx).map_err(|x| {
                        kernel_err::report(x, filename);
                    })?;
                    let params = get_apply_params(&tright);
                    if let Some(type_name) = &params[0].meta.name {
                        if components
                            .iter()
                            .take(components.len() - 1)
                            .cloned()
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
            })?;
            let ctleft = tleft.normalize_in_ctx(env, ctx).map_err(|x| {
                kernel_err::report(x, filename);
            })?;
            let mut params = get_forall_params(&ctleft);
            params.pop();
            // (λ:params[0]. λ:params[1]. ...) (right_term)
            if params.len() >= 2 {
                // at least one for the omitted param, one for the explicit one
                let tright = right_term.compute_type(env, ctx).map_err(|x| {
                    kernel_err::report(x, filename);
                })?;
                let ctright = tright.normalize_in_ctx(env, ctx).map_err(|x| {
                    kernel_err::report(x, filename);
                })?;
                let mut ntype_params = 0;
                for param in &params {
                    // find param matching right term
                    let ictright = ctright.make_inner_by_n(ntype_params).map_err(|x| {
                        kernel_err::report(x, filename);
                    })?;
                    let mut values = vec![];
                    for n in 0..ntype_params {
                        // deduce omitted params
                        let (value, _) = find_var(param, &ictright, ntype_params - n - 1);
                        ctx.add_inner(value.clone(), (*param).clone());
                        values.push(value);
                    }
                    let cparam = param.normalize_in_ctx(env, ctx).map_err(|x| {
                        kernel_err::report(x, filename);
                    })?;
                    for _ in 0..ntype_params {
                        ctx.remove_inner();
                    }
                    if cparam == ictright {
                        for (n, value) in values.iter().enumerate().take(ntype_params) {
                            match &value {
                                Some(v) => {
                                    let meta = left_term.meta.clone();
                                    let iv = v.make_outer_by_n(n).map_err(|x| {
                                        kernel_err::report(x, filename);
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
                convert_to_term_rec(var_type, locals, level, filename, env, ctx, imports).await?;
            if let Some(s) = name {
                if value.get_range().start > forall_range.start {
                    check_variable_name(
                        s,
                        &type_term,
                        env,
                        ctx,
                        &(forall_range.start..forall_range.start + s.len()),
                        filename,
                    )?;
                }
            }
            ctx.add_inner(None, type_term.clone());
            let mut new_locals = locals.clone();
            if let Some(s) = name {
                new_locals.insert(s.to_string(), level);
            }
            let mut new_level = level;
            new_level += 1;
            let value_term =
                convert_to_term_rec(value, &new_locals, new_level, filename, env, ctx, imports)
                    .await?;
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
                convert_to_term_rec(var_type, locals, level, filename, env, ctx, imports).await?;
            check_variable_name(
                name,
                &type_term,
                env,
                ctx,
                &(lambda_range.start..lambda_range.start + name.len()),
                filename,
            )?;
            ctx.add_inner(None, type_term.clone());
            let mut new_locals = locals.clone();
            new_locals.insert(name.to_string(), level);
            let mut new_level = level;
            new_level += 1;
            let value_term =
                convert_to_term_rec(value, &new_locals, new_level, filename, env, ctx, imports)
                    .await?;
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
        AbstractSyntaxTree::Operator(op, left, right, _) => match &**op {
            "." => {
                if let AbstractSyntaxTree::Identifier(components, identifier_range) = &**right {
                    let left_term =
                        convert_to_term_rec(left, locals, level, filename, env, ctx, imports)
                            .await?;
                    let tleft = left_term.compute_type(env, ctx).map_err(|x| {
                        kernel_err::report(x, filename);
                    })?;
                    let params = get_apply_params(&tleft);
                    if let Some(type_name) = &params[0].meta.name {
                        let mut method_components = vec![type_name.clone()];
                        method_components.append(&mut components.clone());
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
                        let global_name = apply_imports(name, imports);
                        if !path::check_path_access(&global_name, filename) {
                            report::send(Report {
                                is_error: true,
                                filename: filename.to_string(),
                                offset: identifier_range.start,
                                message: format!("Method `{}` is private", &global_name),
                                note: Some("Paths containing a leading underscore are only visible from within their enclosing scope".to_string()),
                                help: None,
                                labels: vec![(
                                    identifier_range.clone(),
                                    "Private method".to_string(),
                                )],
                            });
                            return Err(());
                        }
                        let path = path::make_absolute(&global_name, filename);
                        let (new_env, _) =
                            match kernel::update_environment(env.clone(), &path).await {
                                Ok(env) => env,
                                Err(_) => {
                                    report::send(Report {
                                        is_error: true,
                                        filename: filename.to_string(),
                                        offset: identifier_range.start,
                                        message: format!("Undefined method `{}`", path),
                                        note: None,
                                        help: None,
                                        labels: vec![(
                                            identifier_range.clone(),
                                            "No definition found".to_string(),
                                        )],
                                    });
                                    return Err(());
                                }
                            };
                        *env = new_env;
                        convert_to_term_rec(&r, locals, level, filename, env, ctx, imports).await
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
    let file_ast = get_file_ast(filename).await?;
    let imports = compute_imports(&file_ast);
    let mut ctx = Context::new();
    convert_to_term_rec(ast, &HashMap::new(), 0, filename, env, &mut ctx, &imports).await
}
