use crate::ast::InternalAST;
use crate::*;

use nacre_types::report;
use nacre_types::report::Report;

fn parse_prototype_parentheses(
    ast_info: (InternalAST, std::ops::Range<usize>),
    filename: String,
    context_info: (&mut InternalAST, &mut usize, &mut bool),
    left_start: usize,
    keyword: &str,
    n: usize,
    max_n: usize,
) -> Result<(), ()> {
    let (inner, param_group_range) = ast_info;
    let (r, nparams, at_least_one) = context_info;
    for element in inner.into_list().into_iter().rev() {
        if let InternalAST::Typed(var_name, var_type, _) = element {
            if let AbstractSyntaxTree::Identifier(components, identifier_range) = *var_name {
                if components.len() != 1 {
                    report::send(Report {
                        is_error: true,
                        filename,
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
                let rr = r.get_range();
                let start_range = if n == max_n {
                    left_start
                } else {
                    identifier_range.start
                };
                *nparams += 1;
                *r = match keyword {
                    "fn" => InternalAST::Ast(AbstractSyntaxTree::Lambda(
                        components[0].clone(),
                        var_type,
                        Box::new((*r).clone().get_inner(&filename)?),
                        start_range..rr.end,
                    )),
                    "type" => InternalAST::Ast(AbstractSyntaxTree::Forall(
                        Some(components[0].clone()),
                        var_type,
                        Box::new((*r).clone().get_inner(&filename)?),
                        start_range..rr.end,
                    )),
                    _ => unreachable!(),
                };
                *at_least_one = true;
            } else {
                let mut identifier_range = var_name.get_range();
                if identifier_range == (0..0) {
                    identifier_range = param_group_range.start..param_group_range.start;
                }
                report::send(Report {
                    is_error: true,
                    filename,
                    offset: identifier_range.start,
                    message: "parameter name expected".to_string(),
                    note: None,
                    help: None,
                    labels: vec![(identifier_range, "not a valid identifier".to_string())],
                });
                return Err(());
            }
        } else {
            let element_range = element.get_range();
            report::send(Report {
                is_error: true,
                filename,
                offset: element_range.start,
                message: "parameter definition expected".to_string(),
                note: Some(
                    "parameter definitions must follow the syntax `<name>: <type>`".to_string(),
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
    Ok(())
}

fn parse_prototype_bare(
    right: InternalAST,
    param_group: InternalAST,
    filename: String,
    context_info: (InternalAST, usize, bool),
    left_start: usize,
    keyword: &str,
) -> Result<InternalAST, ()> {
    let (r, nparams, at_least_one) = context_info;
    if let InternalAST::Ast(AbstractSyntaxTree::Identifier(ref components, _)) = param_group {
        let param_group_range = param_group.get_range();
        let range_end = r.get_range().end;
        if keyword == "impl" {
            if let InternalAST::Enclosed(inner, _, _) = right {
                if let InternalAST::Ast(ast) = *inner {
                    Ok(InternalAST::Ast(ast.do_namespace(components, &filename)?))
                } else {
                    panic!();
                }
            } else {
                unreachable!();
            }
        } else {
            if !at_least_one {
                report::send(Report {
                    is_error: true,
                    filename,
                    offset: param_group_range.start,
                    message: "paremeter definition expected".to_string(),
                    note: None,
                    help: None,
                    labels: vec![(
                        param_group_range.start..param_group_range.end,
                        "has no parameters".to_string(),
                    )],
                });
                return Err(());
            }
            if let InternalAST::Ast(ast) = param_group {
                Ok(InternalAST::Ast(AbstractSyntaxTree::Assignment(
                    Box::new(ast),
                    Box::new(r.get_inner(&filename)?),
                    true,
                    None,
                    nparams,
                    left_start..range_end,
                )))
            } else {
                panic!();
            }
        }
    } else {
        unreachable!();
    }
}

pub(crate) fn parse_prototype(
    left: &InternalAST,
    right: InternalAST,
    filename: String,
    keyword: &str,
) -> Result<InternalAST, ()> {
    let mut params = left.clone().special_app_flatten(keyword);
    if params.len() < 2 {
        unreachable!();
    }
    let mut r = right.clone();
    params.remove(0);
    let max_n = params.len() - 1;
    let mut nparams = 0;
    let mut at_least_one = false;
    let left_start = left.get_range().start;
    for (n, param_group) in params.into_iter().rev().enumerate() {
        if let InternalAST::Enclosed(inner, ch, param_group_range) = param_group {
            if keyword == "impl" {
                let left_range = left.get_range();
                report::send(Report {
                    is_error: true,
                    filename,
                    offset: param_group_range.start,
                    message: "`impl` block is missing a namespace".to_string(),
                    note: Some("correct syntax is `impl <namespace> {...}`".to_string()),
                    help: None,
                    labels: vec![(
                        left_range.start..param_group_range.end,
                        "`impl` block here".to_string(),
                    )],
                });
                return Err(());
            }
            if ch == '(' {
                parse_prototype_parentheses(
                    (*inner, param_group_range),
                    filename.clone(),
                    (&mut r, &mut nparams, &mut at_least_one),
                    left_start,
                    keyword,
                    n,
                    max_n,
                )?;
            } else {
                report::send(Report {
                    is_error: true,
                    filename,
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
                        _ => unreachable!(),
                    }),
                    help: None,
                    labels: vec![(
                        param_group_range,
                        format!(
                            "enclosed in {}",
                            match ch {
                                '[' => "brackets",
                                '{' => "braces",
                                _ => unreachable!(),
                            }
                        ),
                    )],
                });
                return Err(());
            }
        } else if n == max_n {
            return parse_prototype_bare(
                right,
                param_group,
                filename.clone(),
                (r, nparams, at_least_one),
                left_start,
                keyword,
            );
        } else {
            unreachable!();
        }
    }
    Ok(r)
}
