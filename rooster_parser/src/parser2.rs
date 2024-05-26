use crate::*;

use std::ops::Range;
use tokio::sync::mpsc;

pub(crate) enum AbstractSyntaxTree {
    Block(Vec<Box<AbstractSyntaxTree>>, Range<usize>),
    List(Vec<Box<AbstractSyntaxTree>>, Range<usize>),
    Enclosed(Box<AbstractSyntaxTree>, char, Range<usize>),
    Identifier(Vec<String>, Range<usize>), // TODO: invalid in final AST?
    Assignment(
        Box<AbstractSyntaxTree>,
        Box<AbstractSyntaxTree>,
        bool,
        Range<usize>,
    ),
    Application(
        Box<AbstractSyntaxTree>,
        Box<AbstractSyntaxTree>,
        Range<usize>,
    ),
    Forall(
        Option<String>,
        Box<AbstractSyntaxTree>,
        Box<AbstractSyntaxTree>,
        Range<usize>,
    ),
    Lambda(
        String,
        Box<AbstractSyntaxTree>,
        Box<AbstractSyntaxTree>,
        Range<usize>,
    ),
    // invalid in final AST:
    Empty,
    Typed(
        Box<AbstractSyntaxTree>,
        Box<AbstractSyntaxTree>,
        Range<usize>,
    ),
    TypeApp(
        Box<AbstractSyntaxTree>,
        Box<AbstractSyntaxTree>,
        Range<usize>,
    ),
    FnApp(
        Box<AbstractSyntaxTree>,
        Box<AbstractSyntaxTree>,
        Range<usize>,
    ),
}

impl AbstractSyntaxTree {
    pub(crate) fn get_range(&self) -> Range<usize> {
        match self {
            AbstractSyntaxTree::Block(_, range) => range,
            AbstractSyntaxTree::List(_, range) => range,
            AbstractSyntaxTree::Enclosed(_, _, range) => range,
            AbstractSyntaxTree::Identifier(_, range) => range,
            AbstractSyntaxTree::Assignment(_, _, _, range) => range,
            AbstractSyntaxTree::Application(_, _, range) => range,
            AbstractSyntaxTree::Forall(_, _, _, range) => range,
            AbstractSyntaxTree::Lambda(_, _, _, range) => range,
            AbstractSyntaxTree::Empty => panic!(),
            AbstractSyntaxTree::Typed(_, _, range) => range,
            AbstractSyntaxTree::TypeApp(_, _, range) => range,
            AbstractSyntaxTree::FnApp(_, _, range) => range,
        }
        .clone()
    }

    fn fmt_rec(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        levels: &mut Vec<bool>,
        last: bool,
    ) -> std::fmt::Result {
        let l = levels.len();
        for n in 0..l.max(1) - 1 {
            if levels[n] {
                write!(f, "│ ");
            } else {
                write!(f, "  ");
            }
        }
        if l > 0 {
            if last {
                write!(f, "└ ");
                levels[l - 1] = false;
            } else {
                write!(f, "├ ");
            }
        }
        match self {
            AbstractSyntaxTree::Block(statements, _) => {
                write!(f, "Block\n");
                levels.push(true);
                for n in 0..statements.len() {
                    statements[n].fmt_rec(f, levels, n == statements.len() - 1)?
                }
                levels.pop();
            }
            AbstractSyntaxTree::List(statements, _) => {
                write!(f, "Block\n");
                levels.push(true);
                for n in 0..statements.len() {
                    statements[n].fmt_rec(f, levels, n == statements.len() - 1)?
                }
                levels.pop();
            }
            AbstractSyntaxTree::Enclosed(ast, ch, _) => {
                write!(f, "Enclosed {}\n", ch);
                levels.push(true);
                ast.fmt_rec(f, levels, true);
                levels.pop();
            }
            AbstractSyntaxTree::Identifier(components, _) => {
                write!(f, "Identifier ");
                for n in 0..components.len() {
                    if n != 0 {
                        write!(f, "::");
                    }
                    write!(f, "{}", components[n]);
                }
                write!(f, "\n");
            }
            AbstractSyntaxTree::Assignment(identifier, value, is_let, _) => {
                write!(f, "Assignment {}\n", is_let);
                levels.push(true);
                identifier.fmt_rec(f, levels, false);
                value.fmt_rec(f, levels, true);
                levels.pop();
            }
            AbstractSyntaxTree::Application(left, right, _) => {
                write!(f, "Application\n");
                levels.push(true);
                left.fmt_rec(f, levels, false);
                right.fmt_rec(f, levels, true);
                levels.pop();
            }
            AbstractSyntaxTree::Forall(identifier, var_type, term, _) => {
                match identifier {
                    Some(name) => write!(f, "Forall {}\n", name),
                    None => write!(f, "Forall\n"),
                };
                levels.push(true);
                var_type.fmt_rec(f, levels, false);
                term.fmt_rec(f, levels, true);
                levels.pop();
            }
            AbstractSyntaxTree::Lambda(identifier, var_type, term, _) => {
                write!(f, "Lambda {}\n", identifier);
                levels.push(true);
                var_type.fmt_rec(f, levels, false);
                term.fmt_rec(f, levels, true);
                levels.pop();
            }
            AbstractSyntaxTree::Empty => {
                write!(f, "Empty\n");
            }
            AbstractSyntaxTree::Typed(left, right, _) => {
                write!(f, "Typed\n");
                levels.push(true);
                left.fmt_rec(f, levels, false);
                right.fmt_rec(f, levels, true);
                levels.pop();
            }
            AbstractSyntaxTree::TypeApp(left, right, _) => {
                write!(f, "TypeApp\n");
                levels.push(true);
                left.fmt_rec(f, levels, false);
                right.fmt_rec(f, levels, true);
                levels.pop();
            }
            AbstractSyntaxTree::FnApp(left, right, _) => {
                write!(f, "FnApp\n");
                levels.push(true);
                left.fmt_rec(f, levels, false);
                right.fmt_rec(f, levels, true);
                levels.pop();
            }
        }
        Ok(())
    }

    pub(crate) fn type_app_flatten(self) -> Vec<AbstractSyntaxTree> {
        match self {
            AbstractSyntaxTree::TypeApp(left, right, _) => {
                let mut r = left.type_app_flatten();
                r.push(*right);
                r
            }
            _ => vec![self],
        }
    }

    pub(crate) fn fn_app_flatten(self) -> Vec<AbstractSyntaxTree> {
        match self {
            AbstractSyntaxTree::FnApp(left, right, _) => {
                let mut r = left.fn_app_flatten();
                r.push(*right);
                r
            }
            _ => vec![self],
        }
    }

    pub(crate) fn into_list(self) -> Vec<Box<AbstractSyntaxTree>> {
        match self {
            AbstractSyntaxTree::List(elements, _) => elements,
            _ => vec![Box::new(self)],
        }
    }
}

impl std::fmt::Debug for AbstractSyntaxTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_rec(f, &mut vec![], true)
    }
}

async fn perform_macro_call(logical_path: String, s: String) -> Result<AbstractSyntaxTree, ()> {
    panic!("Macros are yet unimplemented");
}

async fn parse_terminal(token: lexer::Token) -> Result<AbstractSyntaxTree, ()> {
    match token {
        lexer::Token::SingleQuotes(s, _) => {
            // desugars into #char::_parse (...)
            perform_macro_call("char::_parse".to_string(), s).await
        }
        lexer::Token::DoubleQuotes(s, _) => {
            // desugars into #String::_parse (...)
            perform_macro_call("String::_parse".to_string(), s).await
        }
        lexer::Token::Parentheses(s, range) => Ok(AbstractSyntaxTree::Enclosed(
            Box::new(preprocess::preprocess_chunk(s, 0).await?),
            '(',
            range,
        )),
        lexer::Token::Brackets(s, range) => Ok(AbstractSyntaxTree::Enclosed(
            Box::new(preprocess::preprocess_chunk(s, 0).await?),
            '[',
            range,
        )),
        lexer::Token::Braces(s, range) => Ok(AbstractSyntaxTree::Enclosed(
            Box::new(preprocess::preprocess_chunk(s, 0).await?),
            '{',
            range,
        )),
        lexer::Token::Other(s, range) => Ok(AbstractSyntaxTree::Identifier(vec![s], range)),
    }
}

pub(crate) async fn build_tree(
    mut receiver: mpsc::UnboundedReceiver<parser::ParserToken>,
) -> Result<AbstractSyntaxTree, ()> {
    let mut ast_stack = vec![];
    loop {
        let token = match receiver.recv().await {
            Some(token) => token,
            None => break,
        };
        match token {
            parser::ParserToken::Terminal(opt, _) => {
                ast_stack.push(match opt {
                    Some(x) => parse_terminal(x).await?,
                    None => AbstractSyntaxTree::Empty,
                });
            }
            parser::ParserToken::Operator(s, range) => {
                let definition = operators::OPERATOR_TABLE.get(&s).unwrap();
                let handler = definition.2;
                let right = ast_stack.pop().unwrap();
                let left = ast_stack.pop().unwrap();
                ast_stack.push(handler(left, right, range)?);
            }
        }
    }
    Ok(ast_stack.pop().unwrap())
}
