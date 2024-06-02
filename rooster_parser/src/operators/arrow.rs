use crate::operators::prototype;
use crate::parser2::AbstractSyntaxTree;
use crate::*;

use std::ops::Range;

pub(crate) fn arrow_handler(
    left: AbstractSyntaxTree,
    right: AbstractSyntaxTree,
    filename: String,
    _range: Range<usize>,
) -> Result<AbstractSyntaxTree, ()> {
    match left {
        AbstractSyntaxTree::SpecialApp(_, _, ref keyword, _) => {
            if keyword != "type" {
                panic!();
            }
            prototype::parse_prototype(&left, right, filename, &keyword)
        }
        AbstractSyntaxTree::Assignment(
            left_left,
            left_right,
            is_definition,
            def_type,
            left_range,
        ) => {
            if let AbstractSyntaxTree::Lambda(_, _, _, _) = &*left_right {
                if !is_definition {
                    panic!();
                }
                if let Some(_) = def_type {
                    panic!();
                }
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
                panic!();
            }
        }
        // TODO: be more selective
        _ => {
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
