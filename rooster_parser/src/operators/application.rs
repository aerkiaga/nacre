use crate::operators::prototype;
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
                    "fn" | "type" | "impl" => true,
                    _ => false,
                } {
                    let name = components[0].clone();
                    if let AbstractSyntaxTree::Enclosed(_, ch, enclosed_range) = &right {
                        if *ch == '{' {
                            match &*name {
                                "fn" => {
                                    report::send(Report {
                                        is_error: true,
                                        filename: filename,
                                        offset: left_range.start,
                                        message: "Function definition is missing at least one parameter".to_string(),
                                        note: None,
                                        help: Some("You may use a type carrying no information, like `std::types::Unit`".to_string()), // TODO: rename if standard library plan changes or a shorthand is introduced
                                        labels: vec![(left_range.start..enclosed_range.end, "Definition with no parameters".to_string())],
                                    });
                                }
                                "impl" => {
                                    report::send(Report {
                                        is_error: true,
                                        filename: filename,
                                        offset: left_range.start,
                                        message: "`impl` block is missing a namespace".to_string(),
                                        note: Some(
                                            "correct syntax is `impl <namespace> {...}`"
                                                .to_string(),
                                        ),
                                        help: None,
                                        labels: vec![(
                                            left_range.start..enclosed_range.end,
                                            "`impl` block here".to_string(),
                                        )],
                                    });
                                }
                                "type" => unreachable!(),
                                _ => unreachable!(),
                            }
                            return Err(());
                        }
                    }
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
                if *ch == '(' && keyword != "impl" {
                    let key = keyword.clone();
                    return Ok(AbstractSyntaxTree::SpecialApp(
                        Box::new(left),
                        Box::new(right),
                        key,
                        left_start..right_end,
                    ));
                } else {
                    match &**keyword {
                        "fn" => match *ch {
                            '{' => {}
                            '[' => {
                                report::send(Report {
                                    is_error: true,
                                    filename: filename,
                                    offset: right_range.start,
                                    message: "function definition body must be enclosed in braces"
                                        .to_string(),
                                    note: None,
                                    help: None,
                                    labels: vec![(
                                        right_range.clone(),
                                        "enclosed in brackets".to_string(),
                                    )],
                                });
                                return Err(());
                            }
                            _ => unreachable!(),
                        },
                        "impl" => match *ch {
                            '{' => {}
                            '[' => {
                                report::send(Report {
                                    is_error: true,
                                    filename: filename,
                                    offset: right_range.start,
                                    message: "`impl` block must be enclosed in braces".to_string(),
                                    note: None,
                                    help: None,
                                    labels: vec![(
                                        right_range.clone(),
                                        "enclosed in brackets".to_string(),
                                    )],
                                });
                                return Err(());
                            }
                            '(' => {
                                report::send(Report {
                                    is_error: true,
                                    filename: filename,
                                    offset: right_range.start,
                                    message: "`impl` block must be enclosed in braces".to_string(),
                                    note: Some("`impl` blocks do not take parameters".to_string()),
                                    help: None,
                                    labels: vec![(
                                        right_range.clone(),
                                        "enclosed in parentheses".to_string(),
                                    )],
                                });
                                return Err(());
                            }
                            _ => unreachable!(),
                        },
                        "type" => {
                            report::send(Report {
                                is_error: true,
                                filename: filename,
                                offset: right_range.start,
                                message: "type definition return type must be preceded by `->`"
                                    .to_string(),
                                note: None,
                                help: None,
                                labels: vec![(right_range.clone(), "return type here".to_string())],
                            });
                            return Err(());
                        }
                        _ => unreachable!(),
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
                            message: "function body must be enclosed in braces".to_string(),
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
                            message: "type definition return type must be preceded by `->`"
                                .to_string(),
                            note: None,
                            help: None,
                            labels: vec![(right_range, "return type here".to_string())],
                        });
                    }
                    "impl" => {
                        report::send(Report {
                            is_error: true,
                            filename: filename,
                            offset: right_range.start,
                            message: "impl block must be enclosed in braces".to_string(),
                            note: None,
                            help: None,
                            labels: vec![(right_range, "not enclosed".to_string())],
                        });
                    }
                    _ => unreachable!(),
                };
                return Err(());
            }
            prototype::parse_prototype(&left, right, filename, keyword)
        }
        _ => {
            // TODO: check both, then return error if appropriate
            left.must_be_expression(&filename)?;
            right.must_be_expression(&filename)?;
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
