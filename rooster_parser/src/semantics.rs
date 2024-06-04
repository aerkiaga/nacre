use crate::parser2::AbstractSyntaxTree;
use crate::*;

use async_recursion::async_recursion;
use rooster_kernel::Term;
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
                match &**statement {
                    AbstractSyntaxTree::Assignment(name, _, is_definition, _, _) => {
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
                    _ => {}
                }
                r = &r | &compute_dependencies(statement, &new_locals, filename);
            }
            r
        }
        AbstractSyntaxTree::List(expressions, _) => expressions
            .into_iter()
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
        AbstractSyntaxTree::Empty => {
            panic!();
        }
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

#[async_recursion]
pub(crate) async fn convert_to_term(
    ast: &AbstractSyntaxTree,
    locals: &HashMap<String, usize>,
    level: usize,
    filename: &str,
) -> Result<Term, ()> {
    match ast {
        AbstractSyntaxTree::Block(statements, _) => {
            let mut rv = vec![];
            let mut new_locals = locals.clone();
            let mut new_level = level;
            let mut n = 0;
            for statement in statements {
                match &**statement {
                    AbstractSyntaxTree::Assignment(name, value, is_definition, def_type, _) => {
                        if def_type.is_some() {
                            panic!();
                        }
                        if *is_definition {
                            if let AbstractSyntaxTree::Identifier(components, _) = &**name {
                                if components.len() > 1 {
                                    panic!();
                                }
                                rv.push(
                                    convert_to_term(value, &new_locals, new_level, filename)
                                        .await?,
                                );
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
                                convert_to_term(statement, &new_locals, new_level, filename)
                                    .await?,
                            );
                        }
                    }
                }
                n += 1;
            }
            let mut r = rv.pop().unwrap();
            for value in rv.into_iter().rev() {
                r = Term::Let(Box::new(value), Box::new(r));
            }
            Ok(r)
        }
        AbstractSyntaxTree::List(expressions, _) => panic!(),
        AbstractSyntaxTree::Enclosed(inner, _, _) => {
            convert_to_term(inner, locals, level, filename).await
        }
        AbstractSyntaxTree::Identifier(components, _) => {
            let name = components.join("::");
            if name.get(..4) == Some("Type") {
                if name.len() == 4 {
                    return Ok(Term::Prop);
                }
                if let Ok(n) = name[4..].parse::<usize>() {
                    if n > 0 {
                        return Ok(Term::Type(n - 1));
                    } else {
                        panic!("No Type0");
                    }
                }
            }
            match locals.get(&name) {
                Some(n) => Ok(Term::Variable(level - n - 1)),
                None => kernel::get_global_index(&path::make_absolute(&name, filename))
                    .await
                    .map(|n| Term::Global(n)),
            }
        }
        AbstractSyntaxTree::Assignment(_, _, _, _, _) => panic!(),
        AbstractSyntaxTree::Application(left, right, _) => {
            let left_term = convert_to_term(left, locals, level, filename).await?;
            let right_term = convert_to_term(right, locals, level, filename).await?;
            Ok(Term::Apply(Box::new(left_term), Box::new(right_term)))
        }
        AbstractSyntaxTree::Forall(name, var_type, value, _) => {
            let type_term = convert_to_term(var_type, locals, level, filename).await?;
            let mut new_locals = locals.clone();
            if let Some(s) = name {
                new_locals.insert(s.to_string(), level);
            }
            let mut new_level = level;
            new_level += 1;
            let value_term = convert_to_term(value, &new_locals, new_level, filename).await?;
            Ok(Term::Forall(Box::new(type_term), Box::new(value_term)))
        }
        AbstractSyntaxTree::Lambda(name, var_type, value, _) => {
            let type_term = convert_to_term(var_type, locals, level, filename).await?;
            let mut new_locals = locals.clone();
            new_locals.insert(name.to_string(), level);
            let mut new_level = level;
            new_level += 1;
            let value_term = convert_to_term(value, &new_locals, new_level, filename).await?;
            Ok(Term::Lambda(Box::new(type_term), Box::new(value_term)))
        }
        _ => panic!(),
    }
}

fn dependency_loader(
    logical_path: &str,
) -> Pin<Box<dyn Future<Output = Result<HashSet<String>, ()>> + Send + '_>> {
    let logical_path_string = logical_path.to_string();
    Box::pin(async move {
        let (ast, filename) = get_ast(&logical_path_string).await?;
        let deps = compute_dependencies(&ast, &HashSet::new(), &filename);
        let (type_ast, _) = get_type_ast(&logical_path_string).await?;
        let type_deps = match type_ast {
            Some(ast) => compute_dependencies(&ast, &HashSet::new(), &filename),
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
