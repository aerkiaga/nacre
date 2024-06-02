use crate::parser2::AbstractSyntaxTree;
use crate::*;

use std::ops::Range;

fn parse_type_prototype(
    left: AbstractSyntaxTree,
    right: AbstractSyntaxTree,
    filename: String,
    keyword: &str,
) -> Result<AbstractSyntaxTree, ()> {
    let mut params = left.special_app_flatten("type");
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
                            message: "type parameter expected".to_string(),
                            note: Some(
                                "parameters must follow the syntax `<name>: <type>`".to_string(),
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
                    message: "type parameters must be enclosed in parentheses".to_string(),
                    note: Some(
                        "correct syntax is `type (<name>: <type>, ...) ... -> <type>`".to_string(),
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
                let param_group_range = param_group.get_range();
                report::send(Report {
                    is_error: true,
                    filename: filename,
                    offset: param_group_range.start,
                    message: "type parameters must be enclosed in parentheses".to_string(),
                    note: Some(
                        "correct syntax is `type (<name>: <type>, ...) ... -> <type>`".to_string(),
                    ),
                    help: None,
                    labels: vec![(param_group_range, "not enclosed".to_string())],
                });
                return Err(());
            }
        }
        n += 1;
    }
    Ok(r)
}

pub(crate) fn arrow_handler(
    left: AbstractSyntaxTree,
    right: AbstractSyntaxTree,
    filename: String,
    _range: Range<usize>,
) -> Result<AbstractSyntaxTree, ()> {
    if let AbstractSyntaxTree::SpecialApp(_, _, ref keyword, _) = left {
        if *keyword != "type" {
            panic!();
        }
        let keyword_clone = keyword.clone();
        parse_type_prototype(left, right, filename, &keyword_clone)
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
