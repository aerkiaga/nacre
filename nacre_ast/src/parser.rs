use crate::*;

use std::ops::Range;
use tokio::sync::mpsc;

pub(crate) enum ParserToken {
    Terminal(Option<lexer::Token>),
    Operator(String, Range<usize>),
}

impl std::fmt::Debug for ParserToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserToken::Terminal(opt) => match opt {
                Some(token) => write!(f, "Terminal {:?}", token),
                None => write!(f, "Empty Terminal"),
            },
            ParserToken::Operator(s, _) => write!(f, "Operator {}", s),
        }
    }
}

// A modified Dijkstra-type parser
// producing a reverse Polish notation stream from tokens
pub(crate) async fn parse_stream(
    mut receiver: mpsc::UnboundedReceiver<lexer::Token>,
    sender: mpsc::UnboundedSender<ParserToken>,
) {
    let mut next_is_operator = false;
    let mut operator_stack = vec![];
    loop {
        let token = match receiver.recv().await {
            Some(token) => token,
            None => break,
        };
        let mut s_clone = None;
        // Check if we're dealing with an operator
        let maybe_operator = if let lexer::Token::Other(s, _) = &token {
            s_clone = Some(s.clone());
            operators::OPERATOR_TABLE.get(s)
        } else {
            None
        };
        let range = token.get_range();
        // Convert each actual token into a sequence of pseudo-tokens
        // to remove empty and unary operators
        let tokens = match maybe_operator {
            Some(_) => {
                if next_is_operator {
                    // Binary operator
                    next_is_operator = false;
                    vec![ParserToken::Operator(s_clone.unwrap(), range)]
                } else {
                    // Unary operator
                    next_is_operator = false;
                    vec![
                        ParserToken::Terminal(None),
                        ParserToken::Operator(s_clone.unwrap(), range),
                    ]
                }
            }
            None => {
                if next_is_operator {
                    // Empty operator + terminal
                    next_is_operator = true;
                    vec![
                        ParserToken::Operator("".to_string(), range.clone()),
                        ParserToken::Terminal(Some(token.clone())),
                    ]
                } else {
                    // Regular terminal
                    next_is_operator = true;
                    vec![ParserToken::Terminal(Some(token.clone()))]
                }
            }
        };
        // Shunting yard algorithm
        for token in tokens {
            match &token {
                ParserToken::Terminal(_) => {
                    sender.send(token).unwrap();
                }
                ParserToken::Operator(s, _) => {
                    let definition = operators::OPERATOR_TABLE.get(s).unwrap();
                    loop {
                        if operator_stack.is_empty() {
                            break;
                        }
                        let stack_operator = &operator_stack[operator_stack.len() - 1];
                        let stack_definition = match stack_operator {
                            ParserToken::Terminal(_) => unreachable!(),
                            ParserToken::Operator(s, _) => {
                                operators::OPERATOR_TABLE.get(s).unwrap()
                            }
                        };
                        if stack_definition.0 < definition.0
                            || stack_definition.0 == definition.0 && definition.1
                        {
                            break;
                        }
                        sender.send(operator_stack.pop().unwrap()).unwrap();
                    }
                    operator_stack.push(token);
                }
            }
        }
    }
    // Flush remaining stack
    loop {
        sender
            .send(match operator_stack.pop() {
                Some(token) => token,
                None => break,
            })
            .unwrap();
    }
}
