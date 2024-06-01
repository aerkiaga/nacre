use crate::parser2::AbstractSyntaxTree;
use crate::*;

use std::ops::Range;

// TODO: handle macros, including their special ordering
pub(crate) fn application_handler(
    left: AbstractSyntaxTree,
    right: AbstractSyntaxTree,
    filename: String,
    _range: Range<usize>,
) -> Result<AbstractSyntaxTree, ()> {
    match &left {
        AbstractSyntaxTree::Identifier(components, left_range) => {
            let left_start = left_range.start;
            let right_range = right.get_range();
            if components.len() == 1 {
                if match &*components[0] {
                    "fn" | "type" => true,
                    _ => false,
                } {
                    let name = components[0].clone();
                    return Ok(AbstractSyntaxTree::SpecialApp(
                        Box::new(left),
                        Box::new(right),
                        name,
                        left_start..right_range.end,
                    ));
                }
            }
            Ok(AbstractSyntaxTree::Application(
                Box::new(left),
                Box::new(right),
                left_start..right_range.end,
            ))
        }
        AbstractSyntaxTree::SpecialApp(_, _, keyword, left_range) => {
            let left_start = left_range.start;
            let right_range = right.get_range();
            if let AbstractSyntaxTree::Enclosed(_, ch, right_range) = &right {
                let right_end = right_range.end;
                if *ch == '(' {
                    let key = keyword.clone();
                    return Ok(AbstractSyntaxTree::SpecialApp(
                        Box::new(left),
                        Box::new(right),
                        key,
                        left_start..right_end,
                    ));
                } else {
                    if !match &**keyword {
                        "fn" => *ch == '{',
                        "type" => false,
                        _ => panic!(),
                    } {
                        panic!();
                    }
                }
            } else {
                let right_start = right.get_range().start;
                match &**keyword {
                    "fn" => {
                        report::send(Report {
                            is_error: true,
                            filename: filename,
                            offset: right_range.start,
                            message: format!("function body must be enclosed in braces"),
                            note: None,
                            help: None,
                            labels: vec![(right_range, "not enclosed".to_string())],
                        });
                    }
                    "type" => {
                        report::send(Report {
                            is_error: true,
                            filename: filename,
                            offset: right_range.start,
                            message: format!(
                                "type definition return type must be preceded by `->`"
                            ),
                            note: None,
                            help: None,
                            labels: vec![(right_range, "return type here".to_string())],
                        });
                    }
                    _ => panic!(),
                };
                return Err(());
            }
            let mut params = left.special_app_flatten("fn");
            if params.len() < 2 {
                panic!();
            }
            let mut r = right;
            params.remove(0);
            let mut n = 0;
            let max_n = params.len() - 1;
            for param_group in params.into_iter().rev() {
                if let AbstractSyntaxTree::Enclosed(inner, ch, param_group_range) = param_group {
                    if ch == '(' {
                        for element in inner.into_list().into_iter().rev() {
                            if let AbstractSyntaxTree::Typed(var_name, var_type, _) = *element {
                                if let AbstractSyntaxTree::Identifier(
                                    components,
                                    identifier_range,
                                ) = *var_name
                                {
                                    if components.len() != 1 {
                                        report::send(Report {
                                            is_error: true,
                                            filename: filename,
                                            offset: identifier_range.start,
                                            message: "parameter names cannot be namespaced"
                                                .to_string(),
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
                                    r = AbstractSyntaxTree::Lambda(
                                        components[0].clone(),
                                        var_type,
                                        Box::new(r),
                                        identifier_range,
                                    )
                                } else {
                                    let identifier_range = var_name.get_range();
                                    report::send(Report {
                                        is_error: true,
                                        filename: filename,
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
                                    filename: filename,
                                    offset: element_range.start,
                                    message: "function parameter expected".to_string(),
                                    note: Some(
                                        "parameters must follow the syntax `<name>: <type>`"
                                            .to_string(),
                                    ),
                                    help: None,
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
                            filename: filename,
                            offset: param_group_range.start,
                            message: "function parameters must be enclosed in parentheses"
                                .to_string(),
                            note: Some(
                                "correct syntax is `fn (<name>: <type>, ...) ... {<body>}`"
                                    .to_string(),
                            ),
                            help: None,
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
                    if n == max_n {
                        if let AbstractSyntaxTree::Identifier(_, _) = param_group {
                            let range_start = param_group.get_range().start;
                            let range_end = r.get_range().end;
                            return Ok(AbstractSyntaxTree::Assignment(
                                Box::new(param_group),
                                Box::new(r),
                                true,
                                range_start..range_end,
                            ));
                        } else {
                            panic!();
                        }
                    } else {
                        panic!();
                    }
                }
                n += 1;
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
