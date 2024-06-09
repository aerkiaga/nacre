use crate::parser2::AbstractSyntaxTree;
use crate::*;

use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::ops::Range;

// TODO: recover from more errors

mod prototype;

mod namespace;
use namespace::*;

mod application;
use application::*;

mod arrow;
use arrow::*;

fn colon_handler(
    left: AbstractSyntaxTree,
    right: AbstractSyntaxTree,
    filename: String,
    _range: Range<usize>,
) -> Result<AbstractSyntaxTree, ()> {
    right.must_be_expression(&filename)?;
    let left_start = left.get_range().start;
    let right_range = right.get_range();
    match left {
        AbstractSyntaxTree::Assignment(identifier, value, is_let, def_type, _) => {
            if let Some(_) = def_type {
                report::send(Report {
                    is_error: true,
                    filename: filename.to_string(),
                    offset: right_range.start,
                    message: "duplicate type specification in definition".to_string(),
                    note: None,
                    help: Some("remove extra specification".to_string()),
                    labels: vec![(right_range, "duplicate type".to_string())],
                });
                return Err(());
            }
            Ok(AbstractSyntaxTree::Assignment(
                identifier,
                value,
                is_let,
                Some(Box::new(right)),
                left_start..right_range.end,
            ))
        }
        _ => Ok(AbstractSyntaxTree::Typed(
            Box::new(left),
            Box::new(right),
            left_start..right_range.end,
        )),
    }
}

fn comma_handler(
    left: AbstractSyntaxTree,
    right: AbstractSyntaxTree,
    _filename: String,
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
    filename: String,
    _range: Range<usize>,
) -> Result<AbstractSyntaxTree, ()> {
    right.must_be_expression(&filename)?;
    let left_start = left.get_range().start;
    let right_end = right.get_range().end;
    let lrg = left.get_range();
    match left {
        AbstractSyntaxTree::Identifier(_, _) => Ok(AbstractSyntaxTree::Assignment(
            Box::new(left),
            Box::new(right),
            false,
            None,
            left_start..right_end,
        )),
        AbstractSyntaxTree::Application(app_left, app_right, _) => {
            if let AbstractSyntaxTree::Identifier(components, _) = *app_left {
                if components.len() == 1 && components[0] == "let" {
                    if let AbstractSyntaxTree::Identifier(_, _) = *app_right {
                        Ok(AbstractSyntaxTree::Assignment(
                            Box::new(*app_right),
                            Box::new(right),
                            true,
                            None,
                            left_start..right_end,
                        ))
                    } else {
                        let arrg = app_right.get_range();
                        report::send(Report {
                            is_error: true,
                            filename: filename.to_string(),
                            offset: arrg.start,
                            message: "expected valid identifier in definition".to_string(),
                            note: None,
                            help: None,
                            labels: vec![(arrg, "not a valid identifier".to_string())],
                        });
                        Err(())
                    }
                } else {
                    report::send(Report {
                        is_error: true,
                        filename: filename.to_string(),
                        offset: lrg.start,
                        message: "expected lvalue in assignment".to_string(),
                        note: None,
                        help: Some("you might have meant to write `let ...`".to_string()),
                        labels: vec![(lrg, "not an lvalue".to_string())],
                    });
                    Err(())
                }
            } else {
                report::send(Report {
                    is_error: true,
                    filename: filename.to_string(),
                    offset: lrg.start,
                    message: "expected lvalue in assignment".to_string(),
                    note: None,
                    help: None,
                    labels: vec![(lrg, "not an lvalue".to_string())],
                });
                Err(())
            }
        }
        AbstractSyntaxTree::Empty => Err(()),
        _ => {
            report::send(Report {
                is_error: true,
                filename: filename.to_string(),
                offset: lrg.start,
                message: "expected lvalue in assignment".to_string(),
                note: None,
                help: None,
                labels: vec![(lrg, "not an lvalue".to_string())],
            });
            Err(())
        }
    }
}

// TODO: check if they are actually statements
// TODO: ignore parser2::AbstractSyntaxTree::Empty if present
fn semicolon_handler(
    left: AbstractSyntaxTree,
    right: AbstractSyntaxTree,
    _filename: String,
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
                String,
                Range<usize>,
            ) -> Result<AbstractSyntaxTree, ()>,
        ),
    >,
> = Lazy::new(|| {
    let mut r = HashMap::new();
    //name, (precedence, right associative)
    r.insert("::".to_string(), (6., true, namespace_handler as _)); // Namespace
    r.insert("".to_string(), (5., false, application_handler as _)); // Application
    r.insert("->".to_string(), (4., true, arrow_handler as _)); // If/function type
    r.insert("=".to_string(), (3., false, equals_handler as _)); // Assignment
    r.insert(":".to_string(), (2., false, colon_handler as _)); // Type
    r.insert(",".to_string(), (1., true, comma_handler as _)); // Comma
    r.insert(";".to_string(), (0., true, semicolon_handler as _)); // Statement
    r
});
