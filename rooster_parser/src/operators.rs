use crate::parser2::AbstractSyntaxTree;
use crate::*;

use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::ops::Range;

// TODO: error handling
fn namespace_handler(
    left: AbstractSyntaxTree,
    right: AbstractSyntaxTree,
    range: Range<usize>,
) -> Result<AbstractSyntaxTree, ()> {
    if let AbstractSyntaxTree::Identifier(mut left_components, left_range) = left {
        if let AbstractSyntaxTree::Identifier(mut right_components, right_range) = right {
            left_components.append(&mut right_components);
            Ok(AbstractSyntaxTree::Identifier(
                left_components,
                left_range.start..right_range.end,
            ))
        } else {
            panic!();
        }
    } else {
        panic!();
    }
}

// TODO: handle macros, including their special ordering
fn application_handler(
    left: AbstractSyntaxTree,
    right: AbstractSyntaxTree,
    range: Range<usize>,
) -> Result<AbstractSyntaxTree, ()> {
    match &left {
        AbstractSyntaxTree::Identifier(components, left_range) => {
            let left_start = left_range.start;
            let right_range = right.get_range();
            if components.len() == 1 && components[0] == "type" {
                Ok(AbstractSyntaxTree::TypeApp(
                    Box::new(left),
                    Box::new(right),
                    left_start..right_range.end,
                ))
            } else if components.len() == 1 && components[0] == "fn" {
                Ok(AbstractSyntaxTree::FnApp(
                    Box::new(left),
                    Box::new(right),
                    left_start..right_range.end,
                ))
            } else {
                Ok(AbstractSyntaxTree::Application(
                    Box::new(left),
                    Box::new(right),
                    left_start..right_range.end,
                ))
            }
        }
        AbstractSyntaxTree::TypeApp(_, _, left_range) => {
            let left_start = left_range.start;
            let right_range = right.get_range();
            Ok(AbstractSyntaxTree::TypeApp(
                Box::new(left),
                Box::new(right),
                left_start..right_range.end,
            ))
        }
        AbstractSyntaxTree::FnApp(_, _, left_range) => {
            let left_start = left_range.start;
            if let AbstractSyntaxTree::Enclosed(_, ch, right_range) = &right {
                let right_end = right_range.end;
                if *ch == '(' {
                    return Ok(AbstractSyntaxTree::FnApp(
                        Box::new(left),
                        Box::new(right),
                        left_start..right_end,
                    ));
                }
            }
            let mut params = left.fn_app_flatten();
            if params.len() < 2 {
                panic!();
            }
            let right_range = right.get_range();
            let mut r = right;
            let l = params.len();
            params.remove(0);
            for param_group in params.into_iter().rev() {
                if let AbstractSyntaxTree::Enclosed(inner, ch, _) = param_group {
                    if ch == '(' {
                        for element in inner.into_list() {
                            if let AbstractSyntaxTree::Typed(var_name, var_type, _) = *element {
                                if let AbstractSyntaxTree::Identifier(
                                    components,
                                    identifier_range,
                                ) = *var_name
                                {
                                    if components.len() != 1 {
                                        panic!();
                                    }
                                    r = AbstractSyntaxTree::Lambda(
                                        components[0].clone(),
                                        var_type,
                                        Box::new(r),
                                        identifier_range,
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
            Ok(r)
        }
        _ => {
            let left_start = left.get_range().start;
            let right_end = right.get_range().end;
            Ok(AbstractSyntaxTree::Application(
                Box::new(left),
                Box::new(right),
                left_start..right_end,
            ))
        }
    }
}

fn arrow_handler(
    left: AbstractSyntaxTree,
    right: AbstractSyntaxTree,
    range: Range<usize>,
) -> Result<AbstractSyntaxTree, ()> {
    if let AbstractSyntaxTree::TypeApp(_, _, left_range) = &left {
        let mut params = left.type_app_flatten();
        if params.len() < 2 {
            panic!();
        }
        let mut r = right;
        let l = params.len();
        params.remove(0);
        for param_group in params.into_iter().rev() {
            if let AbstractSyntaxTree::Enclosed(inner, ch, param_group_range) = param_group {
                if ch == '(' {
                    for element in inner.into_list() {
                        if let AbstractSyntaxTree::Typed(var_name, var_type, _) = *element {
                            if let AbstractSyntaxTree::Identifier(components, identifier_range) =
                                *var_name
                            {
                                if components.len() != 1 {
                                    report::send(Report {
                                        is_error: true,
                                        offset: identifier_range.start,
                                        message: "parameter names cannot be namespaced".to_string(),
                                        note: None,
                                        help: Some(format!(
                                            "rename variable to `{}`",
                                            components.last().unwrap()
                                        )),
                                        labels: vec![(
                                            identifier_range,
                                            "contains `::`".to_string(),
                                        )],
                                    });
                                    return Err(());
                                }
                                r = AbstractSyntaxTree::Forall(
                                    Some(components[0].clone()),
                                    var_type,
                                    Box::new(r),
                                    identifier_range,
                                )
                            } else {
                                let identifier_range = var_name.get_range();
                                report::send(Report {
                                    is_error: true,
                                    offset: identifier_range.start,
                                    message: "parameter name expected".to_string(),
                                    note: None,
                                    help: None,
                                    labels: vec![(
                                        identifier_range,
                                        "not a valid identifier".to_string(),
                                    )],
                                });
                                return Err(());
                            }
                        } else {
                            let element_range = element.get_range();
                            report::send(Report {
                                is_error: true,
                                offset: element_range.start,
                                message: "type definition parameter expected".to_string(),
                                note: None,
                                help: Some(
                                    "parameters must follow the syntax `<name>: <type>`"
                                        .to_string(),
                                ),
                                labels: vec![(
                                    element_range,
                                    "not a valid parameter definition".to_string(),
                                )],
                            });
                            return Err(());
                        }
                    }
                } else {
                    report::send(Report {
                        is_error: true,
                        offset: param_group_range.start,
                        message: "type definition parameters must be enclosed in parentheses"
                            .to_string(),
                        note: None,
                        help: Some(
                            "correct format is `type (<name>: <type>, ...) ... -> <type>`"
                                .to_string(),
                        ),
                        labels: vec![(
                            param_group_range,
                            format!(
                                "enclosed in {}",
                                match ch {
                                    '[' => "brackets",
                                    '{' => "braces",
                                    _ => panic!(),
                                }
                            ),
                        )],
                    });
                    return Err(());
                }
            } else {
                let param_group_range = param_group.get_range();
                report::send(Report {
                    is_error: true,
                    offset: param_group_range.start,
                    message: "type definition parameters must be enclosed in parentheses"
                        .to_string(),
                    note: None,
                    help: Some(
                        "correct format is `type (<name>: <type>, ...) ... -> <type>`".to_string(),
                    ),
                    labels: vec![(param_group_range, "not enclosed".to_string())],
                });
                return Err(());
            }
        }
        Ok(r)
    } else {
        let left_start = left.get_range().start;
        let right_end = right.get_range().end;
        Ok(AbstractSyntaxTree::Forall(
            None,
            Box::new(left),
            Box::new(right),
            left_start..right_end,
        ))
    }
}

fn colon_handler(
    left: AbstractSyntaxTree,
    right: AbstractSyntaxTree,
    range: Range<usize>,
) -> Result<AbstractSyntaxTree, ()> {
    let left_start = left.get_range().start;
    let right_end = right.get_range().end;
    Ok(AbstractSyntaxTree::Typed(
        Box::new(left),
        Box::new(right),
        left_start..right_end,
    ))
}

// TODO: accept trailing comma
fn comma_handler(
    left: AbstractSyntaxTree,
    right: AbstractSyntaxTree,
    range: Range<usize>,
) -> Result<AbstractSyntaxTree, ()> {
    if let AbstractSyntaxTree::List(mut left_statements, left_range) = left {
        let right_range = right.get_range();
        if let AbstractSyntaxTree::List(mut right_statements, _) = right {
            left_statements.append(&mut right_statements);
        } else {
            left_statements.push(Box::new(right));
        }
        Ok(AbstractSyntaxTree::List(
            left_statements,
            left_range.start..right_range.end,
        ))
    } else if let parser2::AbstractSyntaxTree::List(mut right_statements, right_range) = right {
        let left_range = left.get_range();
        right_statements.insert(0, Box::new(left));
        Ok(AbstractSyntaxTree::List(
            right_statements,
            left_range.start..right_range.end,
        ))
    } else {
        let left_start = left.get_range().start;
        let right_end = right.get_range().end;
        Ok(AbstractSyntaxTree::List(
            vec![Box::new(left), Box::new(right)],
            left_start..right_end,
        ))
    }
}

fn equals_handler(
    left: AbstractSyntaxTree,
    right: AbstractSyntaxTree,
    range: Range<usize>,
) -> Result<AbstractSyntaxTree, ()> {
    // TODO: if leftmost is "let", set last field to true
    let left_start = left.get_range().start;
    let right_end = right.get_range().end;
    Ok(AbstractSyntaxTree::Assignment(
        Box::new(left),
        Box::new(right),
        false,
        left_start..right_end,
    ))
}

// TODO: check if they are actually statements
// TODO: ignore parser2::AbstractSyntaxTree::Empty if present
fn semicolon_handler(
    left: AbstractSyntaxTree,
    right: AbstractSyntaxTree,
    range: Range<usize>,
) -> Result<AbstractSyntaxTree, ()> {
    if let AbstractSyntaxTree::Block(mut left_statements, left_range) = left {
        let right_range = right.get_range();
        if let AbstractSyntaxTree::Block(mut right_statements, _) = right {
            left_statements.append(&mut right_statements);
        } else {
            left_statements.push(Box::new(right));
        }
        Ok(AbstractSyntaxTree::Block(
            left_statements,
            left_range.start..right_range.end,
        ))
    } else if let parser2::AbstractSyntaxTree::Block(mut right_statements, right_range) = right {
        let left_range = left.get_range();
        right_statements.insert(0, Box::new(left));
        Ok(AbstractSyntaxTree::Block(
            right_statements,
            left_range.start..right_range.end,
        ))
    } else {
        let left_start = left.get_range().start;
        let right_end = right.get_range().end;
        Ok(AbstractSyntaxTree::Block(
            vec![Box::new(left), Box::new(right)],
            left_start..right_end,
        ))
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
            fn(
                AbstractSyntaxTree,
                AbstractSyntaxTree,
                Range<usize>,
            ) -> Result<AbstractSyntaxTree, ()>,
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
