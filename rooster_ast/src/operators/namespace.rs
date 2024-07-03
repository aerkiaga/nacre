use crate::*;

use rooster_types::report;
use rooster_types::report::Report;
use std::ops::Range;

pub(crate) fn namespace_handler(
    left: AbstractSyntaxTree,
    right: AbstractSyntaxTree,
    filename: String,
    _range: Range<usize>,
) -> Result<AbstractSyntaxTree, ()> {
    if let AbstractSyntaxTree::Identifier(mut left_components, left_range) = left {
        if let AbstractSyntaxTree::Identifier(mut right_components, right_range) = right {
            left_components.append(&mut right_components);
            Ok(AbstractSyntaxTree::Identifier(
                left_components,
                left_range.start..right_range.end,
            ))
        } else {
            let mut right_range = right.get_range();
            if right_range == (0..0) {
                right_range = left_range.end + 2..left_range.end + 2;
            }
            report::send(Report {
                is_error: true,
                filename,
                offset: right_range.start,
                message: "qualified name must be composed of identifiers".to_string(),
                note: None,
                help: None,
                labels: vec![(right_range, "not a valid identifier".to_string())],
            });
            Err(())
        }
    } else {
        let mut left_range = left.get_range();
        if left_range == (0..0) {
            let right_range = right.get_range();
            left_range = right_range.start - 2..right_range.start - 2;
        }
        report::send(Report {
            is_error: true,
            filename,
            offset: left_range.start,
            message: "qualified name must be composed of identifiers".to_string(),
            note: None,
            help: None,
            labels: vec![(left_range, "not a valid identifier".to_string())],
        });
        Err(())
    }
}
