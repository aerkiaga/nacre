use crate::parser2::AbstractSyntaxTree;
use crate::*;

use std::ops::Range;

pub(crate) fn parse_prototype(
    left: &AbstractSyntaxTree,
    right: AbstractSyntaxTree,
    filename: String,
    keyword: &str,
) -> Result<AbstractSyntaxTree, ()> {
    let mut params = left.clone().special_app_flatten(keyword);
    if params.len() < 2 {
        panic!();
    }
    let mut r = right;
    params.remove(0);
    let mut n = 0;
    let max_n = params.len() - 1;
    for param_group in params.into_iter().rev() {
        if let AbstractSyntaxTree::Enclosed(inner, ch, param_group_range) = param_group {
            if &*keyword == "impl" {
                panic!();
            }
            if ch == '(' {
                for element in inner.into_list().into_iter().rev() {
                    if let AbstractSyntaxTree::Typed(var_name, var_type, _) = *element {
                        if let AbstractSyntaxTree::Identifier(components, identifier_range) =
                            *var_name
                        {
                            if components.len() != 1 {
                                report::send(Report {
                                    is_error: true,
                                    filename: filename,
                                    offset: identifier_range.start,
                                    message: "parameter names cannot be namespaced".to_string(),
                                    note: None,
                                    help: Some(format!(
                                        "rename variable to `{}`",
                                        components.last().unwrap()
                                    )),
                                    labels: vec![(identifier_range, "contains `::`".to_string())],
                                });
                                return Err(());
                            }
                            r = match keyword {
                                "fn" => AbstractSyntaxTree::Lambda(
                                    components[0].clone(),
                                    var_type,
                                    Box::new(r),
                                    identifier_range,
                                ),
                                "type" => AbstractSyntaxTree::Forall(
                                    Some(components[0].clone()),
                                    var_type,
                                    Box::new(r),
                                    identifier_range,
                                ),
                                _ => panic!(),
                            };
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
                            message: "parameter definition expected".to_string(),
                            note: Some(
                                "parameter definitions must follow the syntax `<name>: <type>`"
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
                    message: "parameter definitions must be enclosed in parentheses".to_string(),
                    note: Some(match keyword {
                        "fn" => {
                            "correct syntax is `fn (<name>: <type>, ...) ... {<body>}`".to_string()
                        }
                        "type" => {
                            "correct syntax is `type (<name>: <type>, ...) ... -> <return type>`"
                                .to_string()
                        }
                        _ => panic!(),
                    }),
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
                if let AbstractSyntaxTree::Identifier(ref components, _) = param_group {
                    let range_start = param_group.get_range().start;
                    let range_end = r.get_range().end;
                    if &*keyword == "impl" {
                        if let AbstractSyntaxTree::Enclosed(inner, ch, _) = r {
                            if ch != '{' {
                                panic!();
                            }
                            return Ok(inner.do_namespace(components));
                        }
                        panic!();
                    } else {
                        return Ok(AbstractSyntaxTree::Assignment(
                            Box::new(param_group),
                            Box::new(r),
                            true,
                            None,
                            range_start..range_end,
                        ));
                    }
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
