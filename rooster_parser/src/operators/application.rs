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
                    if !match &**keyword {
                        "fn" | "impl" => *ch == '{',
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
                    "impl" => {
                        panic!();
                    }
                    _ => panic!(),
                };
                return Err(());
            }
            prototype::parse_prototype(&left, right, filename, keyword)
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
