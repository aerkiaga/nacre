use crate::*;

use nacre_types::report;
use nacre_types::report::Report;
use std::ops::Range;
use tokio::sync::mpsc;

/// An AST for parsed Nacre code.
///
/// All tuple variants contain a `Range` field
/// that indicates where in the source code the
/// element occurs.
#[derive(Clone)]
pub enum AbstractSyntaxTree {
    /// A sequence of definitions or statements sperated by semicolons.
    Block(Vec<AbstractSyntaxTree>, Range<usize>),
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
        /// Top-level parameter count
        usize,
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
    /// A generic binary operator
    Operator(
        /// Operator.
        String,
        /// Left operand.
        Box<AbstractSyntaxTree>,
        /// Right operand.
        Box<AbstractSyntaxTree>,
        Range<usize>,
    ),
    /// An import statement through the `use` keyword.
    Import(
        /// Imported path
        Vec<String>,
        Range<usize>,
    ),
}

pub enum GetDefinitionError {
    InvalidAST,
    NotFound,
    Other,
}

impl AbstractSyntaxTree {
    /// Get the range spanned by this node in the source code.
    pub fn get_range(&self) -> Range<usize> {
        match self {
            AbstractSyntaxTree::Block(_, range) => range,
            AbstractSyntaxTree::Identifier(_, range) => range,
            AbstractSyntaxTree::Assignment(_, _, _, _, _, range) => range,
            AbstractSyntaxTree::Application(_, _, range) => range,
            AbstractSyntaxTree::Forall(_, _, _, range) => range,
            AbstractSyntaxTree::Lambda(_, _, _, range) => range,
            AbstractSyntaxTree::Operator(_, _, _, range) => range,
            AbstractSyntaxTree::Import(_, range) => range,
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
        for item in levels.iter().take(l.max(1) - 1) {
            if *item {
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
                writeln!(f, "Block")?;
                levels.push(true);
                for n in 0..statements.len() {
                    statements[n].fmt_rec(f, levels, n == statements.len() - 1)?
                }
                levels.pop();
            }
            AbstractSyntaxTree::Identifier(components, _) => {
                write!(f, "Identifier ")?;
                #[allow(clippy::needless_range_loop)]
                for n in 0..components.len() {
                    if n != 0 {
                        write!(f, "::")?;
                    }
                    write!(f, "{}", components[n])?;
                }
                writeln!(f)?;
            }
            AbstractSyntaxTree::Assignment(identifier, value, is_let, def_type, nparams, _) => {
                writeln!(
                    f,
                    "Assignment {} {} {}",
                    is_let,
                    def_type.is_some(),
                    nparams
                )?;
                levels.push(true);
                if let Some(dt) = def_type {
                    dt.fmt_rec(f, levels, false)?;
                }
                identifier.fmt_rec(f, levels, false)?;
                value.fmt_rec(f, levels, true)?;
                levels.pop();
            }
            AbstractSyntaxTree::Application(left, right, _) => {
                writeln!(f, "Application")?;
                levels.push(true);
                left.fmt_rec(f, levels, false)?;
                right.fmt_rec(f, levels, true)?;
                levels.pop();
            }
            AbstractSyntaxTree::Forall(identifier, var_type, term, _) => {
                match identifier {
                    Some(name) => writeln!(f, "Forall {}", name)?,
                    None => writeln!(f, "Forall")?,
                };
                levels.push(true);
                var_type.fmt_rec(f, levels, false)?;
                term.fmt_rec(f, levels, true)?;
                levels.pop();
            }
            AbstractSyntaxTree::Lambda(identifier, var_type, term, _) => {
                writeln!(f, "Lambda {}", identifier)?;
                levels.push(true);
                var_type.fmt_rec(f, levels, false)?;
                term.fmt_rec(f, levels, true)?;
                levels.pop();
            }
            AbstractSyntaxTree::Operator(op, left, right, _) => {
                writeln!(f, "Operator {}", op)?;
                levels.push(true);
                left.fmt_rec(f, levels, false)?;
                right.fmt_rec(f, levels, true)?;
                levels.pop();
            }
            AbstractSyntaxTree::Import(components, _) => {
                write!(f, "Import ")?;
                #[allow(clippy::needless_range_loop)]
                for n in 0..components.len() {
                    if n != 0 {
                        write!(f, "::")?;
                    }
                    write!(f, "{}", components[n])?;
                }
                writeln!(f)?;
            }
        }
        Ok(())
    }

    pub(crate) fn lambda_typify(
        &self,
        return_type: AbstractSyntaxTree,
        nparams: usize,
    ) -> AbstractSyntaxTree {
        if nparams == 0 {
            return return_type;
        }
        if let AbstractSyntaxTree::Lambda(identifier, var_type, term, _range) = self {
            let rg = return_type.get_range().clone();
            AbstractSyntaxTree::Forall(
                Some(identifier.clone()),
                var_type.clone(),
                Box::new(term.lambda_typify(return_type, nparams - 1)),
                //_range.clone(),
                rg,
            )
        } else {
            return_type
        }
    }

    /// If this [AbstractSyntaxTree] corresponds to a file, obtains a top-level definition from it.
    ///
    /// Must be given the local name of the symbol to retrieve.
    pub fn get_definition(
        &self,
        name: &str,
    ) -> Result<(&AbstractSyntaxTree, Option<&AbstractSyntaxTree>), GetDefinitionError> {
        match self {
            AbstractSyntaxTree::Block(statements, _) => {
                for statement in statements {
                    if let Ok(definition) = statement.get_definition(name) {
                        return Ok(definition);
                    }
                }
                Err(GetDefinitionError::NotFound)
            }
            AbstractSyntaxTree::Assignment(def_name, value, is_def, def_type, _, _) => {
                if *is_def {
                    if let AbstractSyntaxTree::Identifier(components, _) = &**def_name {
                        if components.join("::") == name {
                            Ok((value, def_type.as_deref()))
                        } else {
                            Err(GetDefinitionError::NotFound)
                        }
                    } else {
                        Err(GetDefinitionError::Other)
                    }
                } else {
                    Err(GetDefinitionError::Other)
                }
            }
            _ => Err(GetDefinitionError::InvalidAST),
        }
    }

    pub(crate) fn do_namespace(
        self,
        components: &Vec<String>,
        filename: &str,
    ) -> Result<AbstractSyntaxTree, ()> {
        match self {
            AbstractSyntaxTree::Block(statements, range) => Ok(AbstractSyntaxTree::Block(
                statements
                    .into_iter()
                    .map(|statement| {
                        statement
                            .clone()
                            .do_namespace(components, filename)
                            .unwrap_or(statement)
                    })
                    .collect(),
                range,
            )),
            AbstractSyntaxTree::Assignment(def_name, value, is_def, def_type, nparams, range) => {
                if is_def {
                    if let AbstractSyntaxTree::Identifier(components2, identifier_range) = *def_name
                    {
                        let mut full_name = components.clone();
                        full_name.extend_from_slice(&components2);
                        let new_identifier =
                            AbstractSyntaxTree::Identifier(full_name, identifier_range);
                        Ok(AbstractSyntaxTree::Assignment(
                            Box::new(new_identifier),
                            value,
                            true,
                            def_type,
                            nparams,
                            range,
                        ))
                    } else {
                        unreachable!();
                    }
                } else {
                    report::send(Report {
                        is_error: true,
                        filename: filename.to_string(),
                        offset: range.start,
                        message: "expected definition within `impl` block".to_string(),
                        note: None,
                        help: Some(
                            "use keyword `let` to make assignment into a definition".to_string(),
                        ),
                        labels: vec![(range, "not a definition".to_string())],
                    });
                    Err(())
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
                Err(())
            }
        }
    }

    // only checks top level
    pub(crate) fn must_be_expression(&self, filename: &str) -> Result<(), ()> {
        match self {
            AbstractSyntaxTree::Block(_, _) => Ok(()),
            AbstractSyntaxTree::Identifier(_, _) => Ok(()),
            AbstractSyntaxTree::Assignment(_, _, is_let, _, _, range) => {
                if *is_let {
                    report::send(Report {
                        is_error: true,
                        filename: filename.to_string(),
                        offset: range.start,
                        message: "Expected valid expression, found definition".to_string(),
                        note: None,
                        help: None,
                        labels: vec![(range.clone(), "definition statement".to_string())],
                    });
                } else {
                    report::send(Report {
                        is_error: true,
                        filename: filename.to_string(),
                        offset: range.start,
                        message: "Expected valid expression, found assignment".to_string(),
                        note: None,
                        help: Some(
                            "use `==` for boolean equality, `===` for equality proposition"
                                .to_string(),
                        ),
                        labels: vec![(range.clone(), "assignment statement".to_string())],
                    });
                }
                Err(())
            }
            AbstractSyntaxTree::Application(_, _, _) => Ok(()),
            AbstractSyntaxTree::Forall(_, _, _, _) => Ok(()),
            AbstractSyntaxTree::Lambda(_, _, _, _) => Ok(()),
            AbstractSyntaxTree::Operator(op, _, _, _) => match &**op {
                "." => Ok(()),
                _ => panic!(),
            },
            AbstractSyntaxTree::Import(_, range) => {
                report::send(Report {
                    is_error: true,
                    filename: filename.to_string(),
                    offset: range.start,
                    message: "Expected valid expression, found use statement".to_string(),
                    note: None,
                    help: None,
                    labels: vec![(range.clone(), "use statement".to_string())],
                });
                Err(())
            }
        }
    }
}

impl std::fmt::Debug for AbstractSyntaxTree {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_rec(f, &mut vec![], true)
    }
}

#[derive(Clone)]
pub(crate) enum InternalAST {
    /// Full AST
    Ast(AbstractSyntaxTree),
    /// Empty AST, indicates a parser error.
    Empty,
    /// A sequence of expressions separated by commas.
    List(Vec<InternalAST>, Range<usize>),
    /// Some code enclosed in a particular (opening) character.
    /// This may be `'('`, `'['` or `'{'`.
    Enclosed(Box<InternalAST>, char, Range<usize>),
    /// Typed parameter, should only occur internally.
    Typed(
        Box<AbstractSyntaxTree>,
        Box<AbstractSyntaxTree>,
        Range<usize>,
    ),
    /// Should only occur internally.
    SpecialApp(Box<InternalAST>, Box<InternalAST>, String, Range<usize>),
}

impl InternalAST {
    pub(crate) fn get_range(&self) -> Range<usize> {
        match self {
            InternalAST::Ast(ast) => ast.get_range(),
            InternalAST::Empty => 0..0,
            InternalAST::List(_, range) => range.clone(),
            InternalAST::Enclosed(_, _, range) => range.clone(),
            InternalAST::Typed(_, _, range) => range.clone(),
            InternalAST::SpecialApp(_, _, _, range) => range.clone(),
        }
    }

    fn fmt_rec(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        levels: &mut Vec<bool>,
        last: bool,
    ) -> std::fmt::Result {
        let l = levels.len();
        for item in levels.iter().take(l.max(1) - 1) {
            if *item {
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
            InternalAST::Ast(ast) => {
                writeln!(f, "AST")?;
                levels.push(true);
                ast.fmt_rec(f, levels, true)?;
                levels.pop();
            }
            InternalAST::Empty => {
                writeln!(f, "Empty")?;
            }
            InternalAST::List(definitions, _) => {
                writeln!(f, "List")?;
                levels.push(true);
                for n in 0..definitions.len() {
                    definitions[n].fmt_rec(f, levels, n == definitions.len() - 1)?
                }
                levels.pop();
            }
            InternalAST::Enclosed(inner, s, _) => {
                writeln!(f, "Enclosed {}", s)?;
                levels.push(true);
                inner.fmt_rec(f, levels, true)?;
                levels.pop();
            }
            InternalAST::Typed(left, right, _) => {
                writeln!(f, "Typed")?;
                levels.push(true);
                left.fmt_rec(f, levels, false)?;
                right.fmt_rec(f, levels, true)?;
                levels.pop();
            }
            InternalAST::SpecialApp(left, right, s, _) => {
                writeln!(f, "SpecialApp {}", s)?;
                levels.push(true);
                left.fmt_rec(f, levels, false)?;
                right.fmt_rec(f, levels, true)?;
                levels.pop();
            }
        }
        Ok(())
    }

    pub(crate) fn special_app_flatten(self, keyword: &str) -> Vec<InternalAST> {
        if let InternalAST::SpecialApp(left, right, keyword2, range) = self {
            if keyword2 == keyword {
                let mut r = (*left).special_app_flatten(keyword);
                r.push(*right);
                r
            } else {
                vec![InternalAST::SpecialApp(left, right, keyword2, range)]
            }
        } else {
            vec![self]
        }
    }

    pub(crate) fn into_list(self) -> Vec<InternalAST> {
        match self {
            InternalAST::List(elements, _) => elements,
            _ => vec![self],
        }
    }

    // only checks top level
    pub(crate) fn must_be_expression(&self, filename: &str) -> Result<(), ()> {
        match self {
            InternalAST::Ast(ast) => ast.must_be_expression(filename),
            InternalAST::Empty => Err(()),
            InternalAST::List(_, range) => {
                report::send(Report {
                    is_error: true,
                    filename: filename.to_string(),
                    offset: range.start,
                    message: "Expected expression, found comma-separated list".to_string(),
                    note: None,
                    help: None,
                    labels: vec![(range.clone(), "not an expression".to_string())],
                });
                Err(())
            }
            InternalAST::Enclosed(ast, ch, range) => match ch {
                '(' | '{' => ast.must_be_expression(filename),
                _ => {
                    report::send(Report {
                        is_error: true,
                        filename: filename.to_string(),
                        offset: range.start,
                        message: "Expected expression, found bracket-delimited code".to_string(),
                        note: Some("Bracket notation is reserved for future use".to_string()),
                        help: None,
                        labels: vec![(range.clone(), "enclosed in brackets".to_string())],
                    });
                    Err(())
                }
            },
            InternalAST::Typed(_, _, range) => {
                report::send(Report {
                    is_error: true,
                    filename: filename.to_string(),
                    offset: range.start,
                    message: "Explicit types are only allowed for definitions".to_string(),
                    note: None,
                    help: Some("remove trailing `: <type>` from expression".to_string()),
                    labels: vec![(range.clone(), "has explicit type".to_string())],
                });
                Err(())
            }
            InternalAST::SpecialApp(_, _, keyword, range) => {
                report::send(Report {
                    is_error: true,
                    filename: filename.to_string(),
                    offset: range.start,
                    message: format!(
                        "Incomplete {} in expression",
                        match &**keyword {
                            "fn" => "function declaration",
                            "type" => "type declaration",
                            "impl" => "`impl` block",
                            _ => unreachable!(),
                        }
                    ),
                    note: None,
                    help: None,
                    labels: vec![(range.clone(), "missing body".to_string())],
                });
                Err(())
            }
        }
    }

    pub(crate) fn get_inner(self, filename: &str) -> Result<AbstractSyntaxTree, ()> {
        match self {
            InternalAST::Ast(ast) => Ok(ast),
            InternalAST::Enclosed(ast, _, _) => ast.get_inner(filename),
            _ => {
                let range = self.get_range();
                report::send(Report {
                    is_error: true,
                    filename: filename.to_string(),
                    offset: range.start,
                    message: "Incomplete expression found".to_string(),
                    note: None,
                    help: None,
                    labels: vec![(range.clone(), "incomplete".to_string())],
                });
                Err(())
            }
        }
    }
}

impl std::fmt::Debug for InternalAST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_rec(f, &mut vec![], true)
    }
}

async fn perform_macro_call(_logical_path: String, _s: String) -> Result<AbstractSyntaxTree, ()> {
    panic!("Macros are yet unimplemented");
}

async fn parse_terminal(token: lexer::Token, filename: String) -> Result<InternalAST, ()> {
    match token {
        lexer::Token::SingleQuotes(s, _) => {
            // desugars into #char::_parse (...)
            Ok(InternalAST::Ast(
                perform_macro_call("char::_parse".to_string(), s).await?,
            ))
        }
        lexer::Token::DoubleQuotes(s, _) => {
            // desugars into #String::_parse (...)
            Ok(InternalAST::Ast(
                perform_macro_call("String::_parse".to_string(), s).await?,
            ))
        }
        lexer::Token::Parentheses(s, range) => Ok(InternalAST::Enclosed(
            Box::new(preprocess::preprocess_chunk(s, filename, range.start + 1).await?),
            '(',
            range,
        )),
        lexer::Token::Brackets(s, range) => Ok(InternalAST::Enclosed(
            Box::new(preprocess::preprocess_chunk(s, filename, range.start + 1).await?),
            '[',
            range,
        )),
        lexer::Token::Braces(s, range) => Ok(InternalAST::Enclosed(
            Box::new(preprocess::preprocess_chunk(s, filename, range.start + 1).await?),
            '{',
            range,
        )),
        lexer::Token::Other(s, range) => Ok(InternalAST::Ast(AbstractSyntaxTree::Identifier(
            vec![s],
            range,
        ))),
    }
}

pub(crate) async fn build_tree(
    mut receiver: mpsc::UnboundedReceiver<parser::ParserToken>,
    filename: String,
) -> Result<InternalAST, ()> {
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
                    None => InternalAST::Empty,
                });
            }
            parser::ParserToken::Operator(s, range) => {
                if ast_stack.len() >= 2 {
                    let definition = operators::OPERATOR_TABLE.get(&s).unwrap();
                    let handler = definition.2;
                    let right = ast_stack.pop().unwrap();
                    let left = ast_stack.pop().unwrap();
                    ast_stack.push(match handler(left, right, filename.clone(), range) {
                        Ok(x) => x,
                        Err(_) => InternalAST::Empty,
                    });
                } else if s != ";" && s != "," {
                    report::send(Report {
                        is_error: true,
                        filename,
                        offset: range.start,
                        message: "binary operator is missing right operand".to_string(),
                        note: None,
                        help: None,
                        labels: vec![(range, format!("operator `{}` requires two operands", s))],
                    });
                    return Err(());
                }
            }
        }
    }
    if ast_stack.is_empty() {
        return Err(());
    }
    Ok(ast_stack.pop().unwrap())
}
