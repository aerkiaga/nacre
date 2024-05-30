use crate::parser2::AbstractSyntaxTree;
use crate::*;

use std::collections::HashSet;

// TODO: use CoW
fn compute_dependencies(ast: &AbstractSyntaxTree, locals: &HashSet<String>) -> HashSet<String> {
    match ast {
        AbstractSyntaxTree::Block(statements, _) => {
            let mut r = HashSet::new();
            let mut new_locals = locals.clone();
            for statement in statements {
                match &**statement {
                    AbstractSyntaxTree::Assignment(name, _, is_definition, _) => {
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
                r = &r | &compute_dependencies(statement, &new_locals);
            }
            r
        }
        AbstractSyntaxTree::List(expressions, _) => expressions
            .into_iter()
            .map(|x| compute_dependencies(x, locals))
            .reduce(|a, x| &a | &x)
            .unwrap_or_default(),
        AbstractSyntaxTree::Enclosed(inner, _, _) => compute_dependencies(inner, locals),
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
        AbstractSyntaxTree::Assignment(_, value, _, _) => compute_dependencies(value, locals),
        AbstractSyntaxTree::Application(left, right, _) => {
            &compute_dependencies(left, locals) | &compute_dependencies(right, locals)
        }
        AbstractSyntaxTree::Forall(name, var_type, value, _) => {
            &compute_dependencies(var_type, locals)
                | &match name {
                    Some(s) => {
                        let mut new_locals = locals.clone();
                        new_locals.insert(s.clone());
                        compute_dependencies(value, &new_locals)
                    }
                    None => compute_dependencies(value, locals),
                }
        }
        AbstractSyntaxTree::Lambda(name, var_type, value, _) => {
            &compute_dependencies(var_type, locals)
                | &{
                    let mut new_locals = locals.clone();
                    new_locals.insert(name.clone());
                    compute_dependencies(value, &new_locals)
                }
        }
        _ => panic!(),
    }
}

fn dependency_loader(
    logical_path: &str,
) -> Pin<Box<dyn Future<Output = Result<HashSet<String>, ()>> + Send + '_>> {
    let logical_path_string = logical_path.to_string();
    Box::pin(async move {
        let (ast, filename) = get_ast(&logical_path_string).await.unwrap();
        let deps = compute_dependencies(&ast, &HashSet::new());
        // TODO: transform relative (to filename) paths to be absolute
        Ok(deps
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
