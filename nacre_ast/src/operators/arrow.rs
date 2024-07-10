use crate::ast::InternalAST;
use crate::operators::prototype;
use crate::*;

use nacre_types::report;
use nacre_types::report::Report;
use std::ops::Range;

fn arrow_handler_ast(
    left_ast: AbstractSyntaxTree,
    right: InternalAST,
    filename: String,
) -> Result<InternalAST, ()> {
    match left_ast.clone() {
        AbstractSyntaxTree::Assignment(
            left_left,
            left_right,
            is_definition,
            _def_type,
            nparams,
            left_range,
        ) => {
            if let AbstractSyntaxTree::Lambda(_, _, _, _) = &*left_right {
                let right_range = right.get_range();
                if let InternalAST::Ast(right_ast) = right {
                    let new_def_type = left_right.lambda_typify(right_ast, nparams);
                    Ok(InternalAST::Ast(AbstractSyntaxTree::Assignment(
                        left_left,
                        left_right,
                        is_definition,
                        Some(Box::new(new_def_type)),
                        nparams,
                        left_range.start..right_range.end,
                    )))
                } else {
                    panic!();
                }
            } else {
                unreachable!();
            }
        }
        _ => {
            left_ast
                .must_be_expression(&filename)
                .and(right.must_be_expression(&filename))?;
            if let AbstractSyntaxTree::Lambda(_, _, _, ref left_range) = left_ast {
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
            let left_start = left_ast.get_range().start;
            let right_end = right.get_range().end;
            if let InternalAST::Ast(right_ast) = right {
                Ok(InternalAST::Ast(AbstractSyntaxTree::Forall(
                    None,
                    Box::new(left_ast),
                    Box::new(right_ast),
                    left_start..right_end,
                )))
            } else {
                panic!();
            }
        }
    }
}

pub(crate) fn arrow_handler(
    left: InternalAST,
    right: InternalAST,
    filename: String,
    _range: Range<usize>,
) -> Result<InternalAST, ()> {
    match left {
        InternalAST::SpecialApp(_, _, ref keyword, _) => {
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
        InternalAST::Ast(left_ast) => arrow_handler_ast(left_ast, right, filename),
        InternalAST::Empty => Ok(InternalAST::Empty),
        _ => arrow_handler_ast(left.get_inner(&filename)?, right, filename),
    }
}
