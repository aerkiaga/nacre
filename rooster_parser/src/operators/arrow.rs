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
    if let AbstractSyntaxTree::SpecialApp(_, _, ref keyword, _) = left {
        if keyword != "type" {
            println!("{:?}", left); //D
            panic!();
        }
        prototype::parse_prototype(&left, right, filename, &keyword)
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
