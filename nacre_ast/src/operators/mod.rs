use crate::ast::InternalAST;
use crate::*;

use nacre_types::report;
use nacre_types::report::Report;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::ops::Range;

mod prototype;

mod namespace;
use namespace::*;

mod application;
use application::*;

mod arrow;
use arrow::*;

fn stop_handler(
    left: InternalAST,
    right: InternalAST,
    filename: String,
    _range: Range<usize>,
) -> Result<InternalAST, ()> {
    let left_range = left.get_range();
    let right_range = right.get_range();
    let left_ast = left.get_inner(&filename)?;
    let right_ast = right.get_inner(&filename)?;
    Ok(InternalAST::Ast(AbstractSyntaxTree::Operator(
        ".".to_string(),
        Box::new(left_ast),
        Box::new(right_ast),
        left_range.start..right_range.end,
    )))
}

fn colon_handler(
    left: InternalAST,
    right: InternalAST,
    filename: String,
    _range: Range<usize>,
) -> Result<InternalAST, ()> {
    right.must_be_expression(&filename)?;
    let left_start = left.get_range().start;
    let right_range = right.get_range();
    if let InternalAST::Ast(left_ast) = left {
        if let InternalAST::Ast(right_ast) = right {
            match left_ast {
                AbstractSyntaxTree::Assignment(identifier, value, is_let, def_type, nparams, _) => {
                    if def_type.is_some() {
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
                    Ok(InternalAST::Ast(AbstractSyntaxTree::Assignment(
                        identifier,
                        value,
                        is_let,
                        Some(Box::new(right_ast)),
                        nparams,
                        left_start..right_range.end,
                    )))
                }
                _ => Ok(InternalAST::Typed(
                    Box::new(left_ast),
                    Box::new(right_ast),
                    left_start..right_range.end,
                )),
            }
        } else {
            panic!();
        }
    } else if let InternalAST::Empty = left {
        Ok(InternalAST::Empty)
    } else {
        panic!();
    }
}

fn comma_handler(
    left: InternalAST,
    right: InternalAST,
    _filename: String,
    _range: Range<usize>,
) -> Result<InternalAST, ()> {
    if let InternalAST::List(mut left_statements, left_range) = left {
        let right_range = right.get_range();
        if let InternalAST::List(mut right_statements, _) = right {
            left_statements.append(&mut right_statements);
        } else {
            left_statements.push(right);
        }
        Ok(InternalAST::List(
            left_statements,
            left_range.start..right_range.end,
        ))
    } else if let InternalAST::List(mut right_statements, right_range) = right {
        let left_range = left.get_range();
        right_statements.insert(0, left);
        Ok(InternalAST::List(
            right_statements,
            left_range.start..right_range.end,
        ))
    } else {
        let left_start = left.get_range().start;
        let right_end = right.get_range().end;
        Ok(InternalAST::List(vec![left, right], left_start..right_end))
    }
}

fn equals_handler(
    left: InternalAST,
    right: InternalAST,
    filename: String,
    _range: Range<usize>,
) -> Result<InternalAST, ()> {
    right.must_be_expression(&filename)?;
    let left_start = left.get_range().start;
    let right_end = right.get_range().end;
    let lrg = left.get_range();
    if let InternalAST::Ast(left_ast) = left {
        if let InternalAST::Ast(right_ast) = right {
            match left_ast {
                AbstractSyntaxTree::Identifier(_, _) => {
                    Ok(InternalAST::Ast(AbstractSyntaxTree::Assignment(
                        Box::new(left_ast),
                        Box::new(right_ast),
                        false,
                        None,
                        0,
                        left_start..right_end,
                    )))
                }
                AbstractSyntaxTree::Application(app_left, app_right, _) => {
                    if let AbstractSyntaxTree::Identifier(components, _) = *app_left {
                        if components.len() == 1 && components[0] == "let" {
                            if let AbstractSyntaxTree::Identifier(_, _) = *app_right {
                                Ok(InternalAST::Ast(AbstractSyntaxTree::Assignment(
                                    Box::new(*app_right),
                                    Box::new(right_ast),
                                    true,
                                    None,
                                    0,
                                    left_start..right_end,
                                )))
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
        } else {
            panic!();
        }
    } else if let InternalAST::Empty = left {
        Err(())
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

fn semicolon_handler(
    left: InternalAST,
    right: InternalAST,
    _filename: String,
    _range: Range<usize>,
) -> Result<InternalAST, ()> {
    if let InternalAST::Ast(left_ast) = left {
        if let InternalAST::Ast(right_ast) = right {
            if let AbstractSyntaxTree::Block(mut left_statements, left_range) = left_ast {
                let right_range = right_ast.get_range();
                if let AbstractSyntaxTree::Block(mut right_statements, _) = right_ast {
                    left_statements.append(&mut right_statements);
                } else {
                    left_statements.push(right_ast.clone());
                }
                Ok(InternalAST::Ast(AbstractSyntaxTree::Block(
                    left_statements.clone(),
                    left_range.start..right_range.end,
                )))
            } else if let AbstractSyntaxTree::Block(mut right_statements, right_range) = right_ast {
                let left_range = left_ast.get_range();
                right_statements.insert(0, left_ast.clone());
                Ok(InternalAST::Ast(AbstractSyntaxTree::Block(
                    right_statements.clone(),
                    left_range.start..right_range.end,
                )))
            } else {
                let left_start = left_ast.get_range().start;
                let right_end = right_ast.get_range().end;
                Ok(InternalAST::Ast(AbstractSyntaxTree::Block(
                    vec![left_ast.clone(), right_ast.clone()],
                    left_start..right_end,
                )))
            }
        } else {
            panic!();
        }
    } else if let InternalAST::Empty = left {
        Ok(InternalAST::Empty)
    } else {
        panic!();
    }
}

type OperatorDefinition = (
    f32,
    bool,
    fn(InternalAST, InternalAST, String, Range<usize>) -> Result<InternalAST, ()>,
);

// TODO: make some operators left-associative
// TODO: implement macro operator
// TODO: use string interning for this
pub(crate) static OPERATOR_TABLE: Lazy<HashMap<String, OperatorDefinition>> = Lazy::new(|| {
    let mut r = HashMap::new();
    //name, (precedence, right associative)
    r.insert("::".to_string(), (7., true, namespace_handler as _)); // Namespace
    r.insert(".".to_string(), (6., true, stop_handler as _)); // Method
    r.insert("".to_string(), (5., false, application_handler as _)); // Application
    r.insert("->".to_string(), (4., true, arrow_handler as _)); // If/function type
    r.insert("=".to_string(), (3., false, equals_handler as _)); // Assignment
    r.insert(":".to_string(), (2., false, colon_handler as _)); // Type
    r.insert(",".to_string(), (1., true, comma_handler as _)); // Comma
    r.insert(";".to_string(), (0., true, semicolon_handler as _)); // Statement
    r
});
