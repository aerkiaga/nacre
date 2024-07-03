use crate::operators::prototype;
use crate::*;

use rooster_types::report;
use rooster_types::report::Report;
use std::ops::Range;

pub(crate) fn arrow_handler(
    left: AbstractSyntaxTree,
    right: AbstractSyntaxTree,
    filename: String,
    _range: Range<usize>,
) -> Result<AbstractSyntaxTree, ()> {
    match left {
        AbstractSyntaxTree::SpecialApp(_, _, ref keyword, _) => {
            match &**keyword {
                "type" => {}
                "fn" => {
                    let right_range = right.get_range();
                    report::send(Report {
                        is_error: true,
                        filename,
                        offset: right_range.start,
                        message: "function return type must be specified after function body"
                            .to_string(),
                        note: Some(
                            "the syntax for return types is `fn <name> <params> {...} -> <type>`"
                                .to_string(),
                        ),
                        help: None,
                        labels: vec![(right_range, "return type here".to_string())],
                    });
                    return Err(());
                }
                _ => unreachable!(),
            }
            prototype::parse_prototype(&left, right, filename, keyword)
        }
        AbstractSyntaxTree::Assignment(
            left_left,
            left_right,
            is_definition,
            _def_type,
            left_range,
        ) => {
            if let AbstractSyntaxTree::Lambda(_, _, _, _) = &*left_right {
                let right_range = right.get_range();
                let new_def_type = left_right.lambda_typify(right);
                Ok(AbstractSyntaxTree::Assignment(
                    left_left,
                    left_right,
                    is_definition,
                    Some(Box::new(new_def_type)),
                    left_range.start..right_range.end,
                ))
            } else {
                unreachable!();
            }
        }
        _ => {
            left.must_be_expression(&filename)
                .and(right.must_be_expression(&filename))?;
            if let AbstractSyntaxTree::Lambda(_, _, _, ref left_range) = left {
                let right_range = right.get_range();
                report::send(Report {
                    is_error: false,
                    filename: filename.clone(),
                    offset: left_range.start,
                    message: "type definition using `->` operator likely not intended".to_string(),
                    note: Some(
                        "anonymous functions cannot have an explicit return type".to_string(),
                    ),
                    help: None,
                    labels: vec![(
                        left_range.start..right_range.end,
                        "interpreted as `... -> ...`".to_string(),
                    )],
                });
            }
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
}
