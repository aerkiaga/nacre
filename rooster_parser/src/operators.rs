use crate::parser2::AbstractSyntaxTree;
use crate::*;

use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::mpsc;

// TODO: error handling
fn namespace_handler(left: AbstractSyntaxTree, right: AbstractSyntaxTree) -> AbstractSyntaxTree {
    if let AbstractSyntaxTree::Identifier(mut left_components) = left {
        if let AbstractSyntaxTree::Identifier(mut right_components) = right {
            left_components.append(&mut right_components);
            AbstractSyntaxTree::Identifier(left_components)
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

// TODO: handle macros, including their special ordering
fn application_handler(left: AbstractSyntaxTree, right: AbstractSyntaxTree) -> AbstractSyntaxTree {
    match &left {
        AbstractSyntaxTree::Identifier(components) => {
            if components.len() == 1 && components[0] == "type" {
                AbstractSyntaxTree::TypeApp(Box::new(left), Box::new(right))
            } else if components.len() == 1 && components[0] == "fn" {
                AbstractSyntaxTree::FnApp(Box::new(left), Box::new(right))
            } else {
                AbstractSyntaxTree::Application(Box::new(left), Box::new(right))
            }
        }
        AbstractSyntaxTree::TypeApp(_, _) => {
            AbstractSyntaxTree::TypeApp(Box::new(left), Box::new(right))
        }
        AbstractSyntaxTree::FnApp(_, _) => {
            if let AbstractSyntaxTree::Enclosed(_, ch) = right {
                if ch == '(' {
                    return AbstractSyntaxTree::FnApp(Box::new(left), Box::new(right));
                }
            }
            let mut params = left.fn_app_flatten();
            if params.len() < 2 {
                panic!();
            }
            let mut r = right;
            let l = params.len();
            params.remove(0);
            for param_group in params.into_iter().rev() {
                if let AbstractSyntaxTree::Enclosed(inner, ch) = param_group {
                    if ch == '(' {
                        for element in inner.into_list() {
                            if let AbstractSyntaxTree::Typed(var_name, var_type) = *element {
                                if let AbstractSyntaxTree::Identifier(components) = *var_name {
                                    if components.len() != 1 {
                                        panic!();
                                    }
                                    r = AbstractSyntaxTree::Lambda(
                                        components[0].clone(),
                                        var_type,
                                        Box::new(r),
                                    )
                                } else {
                                    panic!();
                                }
                            } else {
                                panic!();
                            }
                        }
                    } else {
                        panic!();
                    }
                } else {
                    panic!();
                }
            }
            r
        }
        _ => AbstractSyntaxTree::Application(Box::new(left), Box::new(right)),
    }
}

fn arrow_handler(left: AbstractSyntaxTree, right: AbstractSyntaxTree) -> AbstractSyntaxTree {
    if let AbstractSyntaxTree::TypeApp(_, _) = left {
        let mut params = left.type_app_flatten();
        if params.len() < 2 {
            panic!();
        }
        let mut r = right;
        let l = params.len();
        params.remove(0);
        for param_group in params.into_iter().rev() {
            if let AbstractSyntaxTree::Enclosed(inner, ch) = param_group {
                if ch == '(' {
                    for element in inner.into_list() {
                        if let AbstractSyntaxTree::Typed(var_name, var_type) = *element {
                            if let AbstractSyntaxTree::Identifier(components) = *var_name {
                                if components.len() != 1 {
                                    panic!();
                                }
                                r = AbstractSyntaxTree::Forall(
                                    Some(components[0].clone()),
                                    var_type,
                                    Box::new(r),
                                )
                            } else {
                                panic!();
                            }
                        } else {
                            panic!();
                        }
                    }
                } else {
                    panic!();
                }
            } else {
                panic!();
            }
        }
        r
    } else {
        AbstractSyntaxTree::Forall(None, Box::new(left), Box::new(right))
    }
}

fn colon_handler(left: AbstractSyntaxTree, right: AbstractSyntaxTree) -> AbstractSyntaxTree {
    AbstractSyntaxTree::Typed(Box::new(left), Box::new(right))
}

fn comma_handler(left: AbstractSyntaxTree, right: AbstractSyntaxTree) -> AbstractSyntaxTree {
    if let AbstractSyntaxTree::List(mut left_statements) = left {
        if let AbstractSyntaxTree::List(mut right_statements) = right {
            left_statements.append(&mut right_statements);
        } else {
            left_statements.push(Box::new(right));
        }
        AbstractSyntaxTree::List(left_statements)
    } else if let parser2::AbstractSyntaxTree::List(mut right_statements) = right {
        right_statements.insert(0, Box::new(left));
        AbstractSyntaxTree::List(right_statements)
    } else {
        AbstractSyntaxTree::List(vec![Box::new(left), Box::new(right)])
    }
}

fn equals_handler(left: AbstractSyntaxTree, right: AbstractSyntaxTree) -> AbstractSyntaxTree {
    // TODO: if leftmost is "let", set last field to true
    AbstractSyntaxTree::Assignment(Box::new(left), Box::new(right), false)
}

// TODO: check if they are actually statements
// TODO: ignore parser2::AbstractSyntaxTree::Empty if present
fn semicolon_handler(left: AbstractSyntaxTree, right: AbstractSyntaxTree) -> AbstractSyntaxTree {
    if let AbstractSyntaxTree::Block(mut left_statements) = left {
        if let AbstractSyntaxTree::Block(mut right_statements) = right {
            left_statements.append(&mut right_statements);
        } else {
            left_statements.push(Box::new(right));
        }
        AbstractSyntaxTree::Block(left_statements)
    } else if let parser2::AbstractSyntaxTree::Block(mut right_statements) = right {
        right_statements.insert(0, Box::new(left));
        AbstractSyntaxTree::Block(right_statements)
    } else {
        AbstractSyntaxTree::Block(vec![Box::new(left), Box::new(right)])
    }
}

// TODO: make some operators left-associative
// TODO: use floating-point precedence values
// TODO: implement macro operator
// TODO: use string interning for this
// TODO: move operator table into separate module
pub(crate) static OPERATOR_TABLE: Lazy<
    HashMap<
        String,
        (
            usize,
            bool,
            fn(AbstractSyntaxTree, AbstractSyntaxTree) -> AbstractSyntaxTree,
        ),
    >,
> = Lazy::new(|| {
    // TODO: implement actual handlers
    let mut r = HashMap::new();
    //name, (precedence, right associative)
    r.insert("::".to_string(), (6, true, namespace_handler as _)); // Namespace
    r.insert("".to_string(), (5, false, application_handler as _)); // Application
    r.insert("->".to_string(), (4, true, arrow_handler as _)); // If/function type
    r.insert(":".to_string(), (3, false, colon_handler as _)); // Type
    r.insert(",".to_string(), (2, true, comma_handler as _)); // Comma
    r.insert("=".to_string(), (1, false, equals_handler as _)); // Assignment
    r.insert(";".to_string(), (0, true, semicolon_handler as _)); // Statement
    r
});
