use crate::parser2::AbstractSyntaxTree;
use crate::*;

use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::ops::Range;

// TODO: recover from more errors

mod namespace;
use namespace::*;

mod application;
use application::*;

mod arrow;
use arrow::*;

fn colon_handler(
    left: AbstractSyntaxTree,
    right: AbstractSyntaxTree,
    _range: Range<usize>,
) -> Result<AbstractSyntaxTree, ()> {
    let left_start = left.get_range().start;
    let right_end = right.get_range().end;
    Ok(AbstractSyntaxTree::Typed(
        Box::new(left),
        Box::new(right),
        left_start..right_end,
    ))
}

// TODO: accept trailing comma
fn comma_handler(
    left: AbstractSyntaxTree,
    right: AbstractSyntaxTree,
    _range: Range<usize>,
) -> Result<AbstractSyntaxTree, ()> {
    if let AbstractSyntaxTree::List(mut left_statements, left_range) = left {
        let right_range = right.get_range();
        if let AbstractSyntaxTree::List(mut right_statements, _) = right {
            left_statements.append(&mut right_statements);
        } else {
            left_statements.push(Box::new(right));
        }
        Ok(AbstractSyntaxTree::List(
            left_statements,
            left_range.start..right_range.end,
        ))
    } else if let parser2::AbstractSyntaxTree::List(mut right_statements, right_range) = right {
        let left_range = left.get_range();
        right_statements.insert(0, Box::new(left));
        Ok(AbstractSyntaxTree::List(
            right_statements,
            left_range.start..right_range.end,
        ))
    } else {
        let left_start = left.get_range().start;
        let right_end = right.get_range().end;
        Ok(AbstractSyntaxTree::List(
            vec![Box::new(left), Box::new(right)],
            left_start..right_end,
        ))
    }
}

fn equals_handler(
    left: AbstractSyntaxTree,
    right: AbstractSyntaxTree,
    _range: Range<usize>,
) -> Result<AbstractSyntaxTree, ()> {
    let left_start = left.get_range().start;
    let right_end = right.get_range().end;
    match left {
        AbstractSyntaxTree::Identifier(_, _) => Ok(AbstractSyntaxTree::Assignment(
            Box::new(left),
            Box::new(right),
            false,
            left_start..right_end,
        )),
        AbstractSyntaxTree::Application(app_left, app_right, _) => {
            if let AbstractSyntaxTree::Identifier(components, _) = *app_left {
                if components.len() == 1 && components[0] == "let" {
                    Ok(AbstractSyntaxTree::Assignment(
                        Box::new(*app_right),
                        Box::new(right),
                        true,
                        left_start..right_end,
                    ))
                } else {
                    panic!();
                }
            } else {
                panic!();
            }
        }
        _ => {
            panic!();
        }
    }
}

// TODO: accept trailing semicolon
// TODO: check if they are actually statements
// TODO: ignore parser2::AbstractSyntaxTree::Empty if present
fn semicolon_handler(
    left: AbstractSyntaxTree,
    right: AbstractSyntaxTree,
    _range: Range<usize>,
) -> Result<AbstractSyntaxTree, ()> {
    if let AbstractSyntaxTree::Block(mut left_statements, left_range) = left {
        let right_range = right.get_range();
        if let AbstractSyntaxTree::Block(mut right_statements, _) = right {
            left_statements.append(&mut right_statements);
        } else {
            left_statements.push(Box::new(right));
        }
        Ok(AbstractSyntaxTree::Block(
            left_statements,
            left_range.start..right_range.end,
        ))
    } else if let parser2::AbstractSyntaxTree::Block(mut right_statements, right_range) = right {
        let left_range = left.get_range();
        right_statements.insert(0, Box::new(left));
        Ok(AbstractSyntaxTree::Block(
            right_statements,
            left_range.start..right_range.end,
        ))
    } else {
        let left_start = left.get_range().start;
        let right_end = right.get_range().end;
        Ok(AbstractSyntaxTree::Block(
            vec![Box::new(left), Box::new(right)],
            left_start..right_end,
        ))
    }
}

// TODO: make some operators left-associative
// TODO: implement macro operator
// TODO: use string interning for this
pub(crate) static OPERATOR_TABLE: Lazy<
    HashMap<
        String,
        (
            f32,
            bool,
            fn(
                AbstractSyntaxTree,
                AbstractSyntaxTree,
                Range<usize>,
            ) -> Result<AbstractSyntaxTree, ()>,
        ),
    >,
> = Lazy::new(|| {
    // TODO: implement actual handlers
    let mut r = HashMap::new();
    //name, (precedence, right associative)
    r.insert("::".to_string(), (6., true, namespace_handler as _)); // Namespace
    r.insert("".to_string(), (5., false, application_handler as _)); // Application
    r.insert("->".to_string(), (4., true, arrow_handler as _)); // If/function type
    r.insert(":".to_string(), (3., false, colon_handler as _)); // Type
    r.insert(",".to_string(), (2., true, comma_handler as _)); // Comma
    r.insert("=".to_string(), (1., false, equals_handler as _)); // Assignment
    r.insert(";".to_string(), (0., true, semicolon_handler as _)); // Statement
    r
});
