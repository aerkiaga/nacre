use crate::*;

use std::ops::Range;
use tokio::sync::mpsc;

/// An AST for parsed Rooster code.
///
/// All tuple variants contain a `Range` field
/// that indicates where in the source code the
/// element occurs.
#[derive(Clone)]
pub enum AbstractSyntaxTree {
    /// A sequence of definitions or statements sperated by semicolons.
    Block(Vec<Box<AbstractSyntaxTree>>, Range<usize>),
    /// A sequence of expressions separated by commas.
    List(Vec<Box<AbstractSyntaxTree>>, Range<usize>),
    /// Some code enclosed in a particular (opening) character.
    /// This may be `'('`, `'['` or `'{'`.
    Enclosed(Box<AbstractSyntaxTree>, char, Range<usize>),
    /// An identifier made of one or more path elements separated by `::`.
    Identifier(Vec<String>, Range<usize>), // TODO: invalid in final AST?
    /// An assignment or definition.
    Assignment(
        /// Left-hand expression.
        Box<AbstractSyntaxTree>,
        /// Right-hand expression.
        Box<AbstractSyntaxTree>,
        /// Whether the assignment is a definition (starts with `let`).
        bool,
        /// If a definition, its type.
        Option<Box<AbstractSyntaxTree>>,
        Range<usize>,
    ),
    /// A function application.
    Application(
        /// Function to be called.
        Box<AbstractSyntaxTree>,
        /// Parameter.
        Box<AbstractSyntaxTree>,
        Range<usize>,
    ),
    /// A type definition or forall expression.
    Forall(
        /// Optional parameter name.
        Option<String>,
        /// Parameter type.
        Box<AbstractSyntaxTree>,
        /// Return type.
        Box<AbstractSyntaxTree>,
        Range<usize>,
    ),
    /// A function definition.
    Lambda(
        /// Parameter name.
        String,
        /// Parameter type.
        Box<AbstractSyntaxTree>,
        /// Return type.
        Box<AbstractSyntaxTree>,
        Range<usize>,
    ),
    /// Empty AST, should only occur internally.
    Empty,
    /// Typed parameter, should only occur internally.
    Typed(
        Box<AbstractSyntaxTree>,
        Box<AbstractSyntaxTree>,
        Range<usize>,
    ),
    /// Should only occur internally.
    SpecialApp(
        Box<AbstractSyntaxTree>,
        Box<AbstractSyntaxTree>,
        String,
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
            AbstractSyntaxTree::Assignment(_, _, _, _, range) => range,
            AbstractSyntaxTree::Application(_, _, range) => range,
            AbstractSyntaxTree::Forall(_, _, _, range) => range,
            AbstractSyntaxTree::Lambda(_, _, _, range) => range,
            AbstractSyntaxTree::Empty => panic!(),
            AbstractSyntaxTree::Typed(_, _, range) => range,
            AbstractSyntaxTree::SpecialApp(_, _, _, range) => range,
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
                write!(f, "│ ")?;
            } else {
                write!(f, "  ")?;
            }
        }
        if l > 0 {
            if last {
                write!(f, "└ ")?;
                levels[l - 1] = false;
            } else {
                write!(f, "├ ")?;
            }
        }
        match self {
            AbstractSyntaxTree::Block(statements, _) => {
                write!(f, "Block\n")?;
                levels.push(true);
                for n in 0..statements.len() {
                    statements[n].fmt_rec(f, levels, n == statements.len() - 1)?
                }
                levels.pop();
            }
            AbstractSyntaxTree::List(statements, _) => {
                write!(f, "Block\n")?;
                levels.push(true);
                for n in 0..statements.len() {
                    statements[n].fmt_rec(f, levels, n == statements.len() - 1)?
                }
                levels.pop();
            }
            AbstractSyntaxTree::Enclosed(ast, ch, _) => {
                write!(f, "Enclosed {}\n", ch)?;
                levels.push(true);
                ast.fmt_rec(f, levels, true)?;
                levels.pop();
            }
            AbstractSyntaxTree::Identifier(components, _) => {
                write!(f, "Identifier ")?;
                for n in 0..components.len() {
                    if n != 0 {
                        write!(f, "::")?;
                    }
                    write!(f, "{}", components[n])?;
                }
                write!(f, "\n")?;
            }
            AbstractSyntaxTree::Assignment(identifier, value, is_let, def_type, _) => {
                write!(f, "Assignment {} {}\n", is_let, def_type.is_some())?;
                levels.push(true);
                if let Some(dt) = def_type {
                    dt.fmt_rec(f, levels, false)?;
                }
                identifier.fmt_rec(f, levels, false)?;
                value.fmt_rec(f, levels, true)?;
                levels.pop();
            }
            AbstractSyntaxTree::Application(left, right, _) => {
                write!(f, "Application\n")?;
                levels.push(true);
                left.fmt_rec(f, levels, false)?;
                right.fmt_rec(f, levels, true)?;
                levels.pop();
            }
            AbstractSyntaxTree::Forall(identifier, var_type, term, _) => {
                match identifier {
                    Some(name) => write!(f, "Forall {}\n", name)?,
                    None => write!(f, "Forall\n")?,
                };
                levels.push(true);
                var_type.fmt_rec(f, levels, false)?;
                term.fmt_rec(f, levels, true)?;
                levels.pop();
            }
            AbstractSyntaxTree::Lambda(identifier, var_type, term, _) => {
                write!(f, "Lambda {}\n", identifier)?;
                levels.push(true);
                var_type.fmt_rec(f, levels, false)?;
                term.fmt_rec(f, levels, true)?;
                levels.pop();
            }
            AbstractSyntaxTree::Empty => {
                write!(f, "Empty\n")?;
            }
            AbstractSyntaxTree::Typed(left, right, _) => {
                write!(f, "Typed\n")?;
                levels.push(true);
                left.fmt_rec(f, levels, false)?;
                right.fmt_rec(f, levels, true)?;
                levels.pop();
            }
            AbstractSyntaxTree::SpecialApp(left, right, keyword, _) => {
                write!(f, "SpecialApp {}\n", keyword)?;
                levels.push(true);
                left.fmt_rec(f, levels, false)?;
                right.fmt_rec(f, levels, true)?;
                levels.pop();
            }
        }
        Ok(())
    }

    pub(crate) fn special_app_flatten(self, keyword: &str) -> Vec<AbstractSyntaxTree> {
        if let AbstractSyntaxTree::SpecialApp(left, right, keyword2, range) = self {
            if keyword2 == keyword {
                let mut r = left.special_app_flatten(keyword);
                r.push(*right);
                r
            } else {
                vec![AbstractSyntaxTree::SpecialApp(left, right, keyword2, range)]
            }
        } else {
            vec![self]
        }
    }

    pub(crate) fn lambda_typify(&self, return_type: AbstractSyntaxTree) -> AbstractSyntaxTree {
        if let AbstractSyntaxTree::Lambda(identifier, var_type, term, range) = self {
            AbstractSyntaxTree::Forall(
                Some(identifier.clone()),
                var_type.clone(),
                Box::new(term.lambda_typify(return_type)),
                range.clone(),
            )
        } else {
            return_type
        }
    }

    pub(crate) fn into_list(self) -> Vec<Box<AbstractSyntaxTree>> {
        match self {
            AbstractSyntaxTree::List(elements, _) => elements,
            _ => vec![Box::new(self)],
        }
    }

    pub(crate) fn get_definition(
        &self,
        name: &str,
    ) -> Result<(&AbstractSyntaxTree, Option<&AbstractSyntaxTree>), ()> {
        match self {
            AbstractSyntaxTree::Block(statements, _) => {
                for statement in statements {
                    match statement.get_definition(name) {
                        Ok(definition) => return Ok(definition),
                        Err(_) => {}
                    }
                }
                Err(())
            }
            AbstractSyntaxTree::Assignment(def_name, value, is_def, def_type, _) => {
                if *is_def {
                    if let AbstractSyntaxTree::Identifier(components, _) = &**def_name {
                        if components.join("::") == name {
                            Ok((value, def_type.as_deref()))
                        } else {
                            Err(())
                        }
                    } else {
                        Err(())
                    }
                } else {
                    Err(())
                }
            }
            _ => Err(()),
        }
    }

    pub(crate) fn do_namespace(
        self,
        components: &Vec<String>,
        filename: &str,
    ) -> AbstractSyntaxTree {
        match self {
            AbstractSyntaxTree::Block(statements, range) => AbstractSyntaxTree::Block(
                statements
                    .into_iter()
                    .map(|statement| Box::new(statement.do_namespace(components, filename)))
                    .collect(),
                range,
            ),
            AbstractSyntaxTree::Assignment(def_name, value, is_def, def_type, range) => {
                if is_def {
                    if let AbstractSyntaxTree::Identifier(components2, identifier_range) = *def_name
                    {
                        let mut full_name = components.clone();
                        full_name.extend_from_slice(&components2);
                        let new_identifier =
                            AbstractSyntaxTree::Identifier(full_name, identifier_range);
                        AbstractSyntaxTree::Assignment(
                            Box::new(new_identifier),
                            value,
                            true,
                            def_type,
                            range,
                        )
                    } else {
                        panic!();
                    }
                } else {
                    panic!();
                }
            }
            _ => {
                let range = self.get_range();
                report::send(Report {
                    is_error: true,
                    filename: filename.to_string(),
                    offset: range.start,
                    message: "expected definition within `impl` block".to_string(),
                    note: None,
                    help: None,
                    labels: vec![(range, "not a definition".to_string())],
                });
                // TODO: AbstractSyntaxTree::Ignore or similar
                AbstractSyntaxTree::Empty
            }
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

async fn parse_terminal(token: lexer::Token, filename: String) -> Result<AbstractSyntaxTree, ()> {
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
            Box::new(preprocess::preprocess_chunk(s, filename, range.start + 1).await?),
            '(',
            range,
        )),
        lexer::Token::Brackets(s, range) => Ok(AbstractSyntaxTree::Enclosed(
            Box::new(preprocess::preprocess_chunk(s, filename, range.start + 1).await?),
            '[',
            range,
        )),
        lexer::Token::Braces(s, range) => Ok(AbstractSyntaxTree::Enclosed(
            Box::new(preprocess::preprocess_chunk(s, filename, range.start + 1).await?),
            '{',
            range,
        )),
        lexer::Token::Other(s, range) => Ok(AbstractSyntaxTree::Identifier(vec![s], range)),
    }
}

pub(crate) async fn build_tree(
    mut receiver: mpsc::UnboundedReceiver<parser::ParserToken>,
    filename: String,
) -> Result<AbstractSyntaxTree, ()> {
    let mut ast_stack = vec![];
    loop {
        let token = match receiver.recv().await {
            Some(token) => token,
            None => break,
        };
        match token {
            parser::ParserToken::Terminal(opt) => {
                ast_stack.push(match opt {
                    Some(x) => parse_terminal(x, filename.clone()).await?,
                    None => AbstractSyntaxTree::Empty,
                });
            }
            parser::ParserToken::Operator(s, range) => {
                if ast_stack.len() >= 2 {
                    let definition = operators::OPERATOR_TABLE.get(&s).unwrap();
                    let handler = definition.2;
                    let right = ast_stack.pop().unwrap();
                    let left = ast_stack.pop().unwrap();
                    ast_stack.push(handler(left, right, filename.clone(), range)?);
                } else {
                    if s != ";" && s != "," {
                        report::send(Report {
                            is_error: true,
                            filename: filename,
                            offset: range.start,
                            message: "binary operator is missing right operand".to_string(),
                            note: None,
                            help: None,
                            labels: vec![(
                                range,
                                format!("operator `{}` requires two operands", s),
                            )],
                        });
                        return Err(());
                    }
                }
            }
        }
    }
    if ast_stack.len() < 1 {
        return Err(());
    }
    Ok(ast_stack.pop().unwrap())
}
