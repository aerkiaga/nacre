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

fn find_x0(left: Term<TermMeta>, right: Term<TermMeta>) -> (Option<Term<TermMeta>>, bool) {
    match left.inner {
        TermInner::Prop => (None, matches!(right.inner, TermInner::Prop)),
        TermInner::Type(n1) => {
            if let TermInner::Type(n2) = right.inner {
                (None, n1 == n2)
            } else {
                (None, false)
            }
        }
        TermInner::Global(g1) => {
            if let TermInner::Global(g2) = right.inner {
                (None, g1 == g2)
            } else {
                (None, false)
            }
        }
        TermInner::Variable(v1) => {
            if v1 == 0 {
                (Some(right), true)
            } else if let TermInner::Variable(v2) = right.inner {
                (None, v1 == v2)
            } else {
                (None, false)
            }
        }
        TermInner::Forall(l1, r1) => {
            if let TermInner::Forall(l2, r2) = right.inner {
                let (tl, bl) = find_x0(*l1, *l2);
                let (tr, br) = find_x0(*r1, *r2);
                (tl.or(tr), bl && br)
            } else {
                (None, false)
            }
        }
        TermInner::Lambda(l1, r1) => {
            if let TermInner::Lambda(l2, r2) = right.inner {
                let (tl, bl) = find_x0(*l1, *l2);
                let (tr, br) = find_x0(*r1, *r2);
                (tl.or(tr), bl && br)
            } else {
                (None, false)
            }
        }
        TermInner::Apply(l1, r1) => {
            if let TermInner::Apply(l2, r2) = right.inner {
                let (tl, bl) = find_x0(*l1, *l2);
                let (tr, br) = find_x0(*r1, *r2);
                (tl.or(tr), bl && br)
            } else {
                (None, false)
            }
        }
        TermInner::Let(l1, r1) => {
            if let TermInner::Let(l2, r2) = right.inner {
                let (tl, bl) = find_x0(*l1, *l2);
                let (tr, br) = find_x0(*r1, *r2);
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

#[async_recursion]
pub(crate) async fn convert_to_term_rec(
    ast: &AbstractSyntaxTree,
    locals: &HashMap<String, usize>,
    level: usize,
    filename: &str,
    env: &Environment<TermMeta>,
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
                        Ok(n) => Ok((TermInner::Global(n), &meta).into()),
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
            let tleft = left_term.compute_type(env, ctx).map_err(|x| {
                kernel_err::report(x, filename);
                ()
            })?;
            let ctleft = tleft.normalize_in_ctx(env, ctx).map_err(|x| {
                kernel_err::report(x, filename);
                ()
            })?;
            // TODO: allow omitting more than one in a row
            if let TermInner::Forall(ref ll, lr) = ctleft.inner {
                if let TermInner::Prop = ll.inner {
                    if let TermInner::Forall(ref lrl, _) = lr.inner {
                        let tright = right_term.compute_type(env, ctx).map_err(|x| {
                            kernel_err::report(x, filename);
                            ()
                        })?;
                        let ctright = tright.normalize_in_ctx(env, ctx).map_err(|x| {
                            kernel_err::report(x, filename);
                            ()
                        })?;
                        if let TermInner::Prop = ctright.inner {
                        } else {
                            // Omitted type parameter
                            ctx.add_inner(None, *ll.clone());
                            let clrl = lrl.normalize_in_ctx(env, ctx).map_err(|x| {
                                kernel_err::report(x, filename);
                                ()
                            })?;
                            ctx.remove_inner();
                            let (value, equal) = find_x0(clrl, ctright);
                            if !equal {
                                panic!();
                            }
                            match value {
                                Some(v) => {
                                    let meta = left_term.meta.clone();
                                    left_term =
                                        (TermInner::Apply(Box::new(left_term), Box::new(v)), &meta)
                                            .into();
                                }
                                None => panic!(),
                            }
                        }
                    }
                }
            }
            if let TermInner::Apply(ref ll, ref lr) = left_term.inner {
                if left_term.meta.range != ll.meta.range {
                    // Term wasn't produced by parameter omission
                    let tlr = lr.compute_type(env, ctx).map_err(|x| {
                        kernel_err::report(x, filename);
                        ()
                    })?;
                    let ctlr = tlr.normalize_in_ctx(env, ctx).map_err(|x| {
                        kernel_err::report(x, filename);
                        ()
                    })?;
                    if let TermInner::Prop = ctlr.inner {
                        // Non-omitted type parameter
                        let tright = right_term.compute_type(env, ctx).map_err(|x| {
                            kernel_err::report(x, filename);
                            ()
                        })?;
                        let ctright = tright.normalize_in_ctx(env, ctx).map_err(|x| {
                            kernel_err::report(x, filename);
                            ()
                        })?;
                        if let TermInner::Prop = ctright.inner {
                            // Next is also type parameter, ignore
                            // until multiple omissions in a row are allowed
                        } else {
                            let tll = ll.compute_type(env, ctx).map_err(|x| {
                                kernel_err::report(x, filename);
                                ()
                            })?;
                            let ctll = tll.normalize_in_ctx(env, ctx).map_err(|x| {
                                kernel_err::report(x, filename);
                                ()
                            })?;
                            if let TermInner::Forall(lll, llr) = ctll.inner {
                                if let TermInner::Forall(llll, lllr) = llr.inner {
                                    let cllll = llll.normalize_in_ctx(env, ctx).map_err(|x| {
                                        kernel_err::report(x, filename);
                                        ()
                                    })?;
                                    if contains_var(&llll, 0) {
                                        report::send(Report {
                                            is_error: false,
                                            filename: filename.to_string(),
                                            offset: ll.meta.range.start,
                                            message: "Explicit type parameter is unnecessary here"
                                                .to_string(),
                                            note: None,
                                            help: Some("omit parameter".to_string()),
                                            labels: vec![
                                                (
                                                    ll.meta.range.clone(),
                                                    "Function here".to_string(),
                                                ),
                                                (
                                                    lr.meta.range.clone(),
                                                    "Unnecessary parameter".to_string(),
                                                ),
                                            ],
                                        });
                                    }
                                }
                            }
                        }
                    }
                }
            }
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
            Ok((
                TermInner::Forall(Box::new(type_term), Box::new(value_term)),
                &meta,
            )
                .into())
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
            Ok((
                TermInner::Lambda(Box::new(type_term), Box::new(value_term)),
                &meta,
            )
                .into())
        }
        _ => Err(()),
    }
}

pub(crate) async fn convert_to_term(
    ast: &AbstractSyntaxTree,
    filename: &str,
    env: &Environment<TermMeta>,
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
        // TODO: transform relative (to filename) paths to be absolute
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
