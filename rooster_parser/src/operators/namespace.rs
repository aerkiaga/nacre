use crate::parser2::AbstractSyntaxTree;
use crate::*;

use std::ops::Range;

pub(crate) fn namespace_handler(
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
            let right_range = right.get_range();
            report::send(Report {
                is_error: true,
                offset: right_range.start,
                message: "qualified name must be composed of identifiers".to_string(),
                note: None,
                help: None,
                labels: vec![(right_range, "not a valid identifier".to_string())],
            });
            Err(())
        }
    } else {
        let left_range = left.get_range();
        report::send(Report {
            is_error: true,
            offset: left_range.start,
            message: "qualified name must be composed of identifiers".to_string(),
            note: None,
            help: None,
            labels: vec![(left_range, "not a valid identifier".to_string())],
        });
        Err(())
    }
}
