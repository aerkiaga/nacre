use nacre_types::report;
use nacre_types::report::Report;
use std::ops::Range;
use tokio::sync::mpsc;

#[derive(Clone)]
pub(crate) enum Token {
    SingleQuotes(String, Range<usize>),
    DoubleQuotes(String, Range<usize>),
    Parentheses(String, Range<usize>),
    Brackets(String, Range<usize>),
    Braces(String, Range<usize>),
    Other(String, Range<usize>), // TODO: use string interning for this one
}

impl Token {
    pub(crate) fn get_range(&self) -> Range<usize> {
        match self {
            Token::SingleQuotes(_, range) => range,
            Token::DoubleQuotes(_, range) => range,
            Token::Parentheses(_, range) => range,
            Token::Brackets(_, range) => range,
            Token::Braces(_, range) => range,
            Token::Other(_, range) => range,
        }
        .clone()
    }
}

impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::SingleQuotes(s, _) => write!(f, "\'{}\'", s),
            Token::DoubleQuotes(s, _) => write!(f, "\"{}\"", s),
            Token::Parentheses(s, _) => write!(f, "({})", s),
            Token::Brackets(s, _) => write!(f, "[{}]", s),
            Token::Braces(s, _) => write!(f, "{{{}}}", s),
            Token::Other(s, _) => write!(f, "{}", s),
        }
    }
}

#[derive(PartialEq)]
enum CharType {
    Identifier,
    Operator,
    SingleQuote,
    DoubleQuote,
    OpenDelimiter,
    CloseDelimiter,
    Space,
}

fn classify_char(ch: char) -> CharType {
    if ch.is_alphanumeric() || ch == '_' {
        CharType::Identifier
    } else if ch.is_whitespace() {
        CharType::Space
    } else if ch == '\'' {
        CharType::SingleQuote
    } else if ch == '\"' {
        CharType::DoubleQuote
    } else if ch == '(' || ch == '[' || ch == '{' {
        CharType::OpenDelimiter
    } else if ch == ')' || ch == ']' || ch == '}' {
        CharType::CloseDelimiter
    } else {
        CharType::Operator
    }
}

fn get_opening_char(ch: char) -> char {
    if ch == ')' {
        '('
    } else if ch == ']' {
        '['
    } else if ch == '}' {
        '{'
    } else {
        panic!();
    }
}

pub(crate) fn get_closing_char(ch: char) -> char {
    if ch == '(' {
        ')'
    } else if ch == '[' {
        ']'
    } else if ch == '{' {
        '}'
    } else {
        panic!();
    }
}

#[derive(Clone)]
enum TokenizerState {
    Normal,
    ShortComment(Box<TokenizerState>),
    LongComment(bool, usize, Box<TokenizerState>),
    SingleQuotes(bool, usize),
    DoubleQuotes(bool, usize),
    Delimiters(Vec<(char, usize)>),
}

fn tokenize_normal(
    filename: &String,
    offset: usize,
    sender: &mpsc::UnboundedSender<Token>,
    ch_info: (char, CharType),
    fsm: (
        &mut TokenizerState,
        &mut Vec<char>,
        &mut Option<usize>,
        &mut usize,
    ),
) -> Result<(), ()> {
    let (ch, ch_type) = ch_info;
    let (state, char_stack, initial_offset, token_count) = fsm;
    if !char_stack.is_empty() && ch_type != classify_char(char_stack[char_stack.len() - 1]) {
        sender
            .send(Token::Other(
                char_stack.clone().into_iter().collect::<String>(),
                initial_offset.unwrap()..offset,
            ))
            .unwrap();
        *token_count += 1;
        char_stack.clear();
        *initial_offset = None;
    }

    match ch_type {
        CharType::Identifier => {
            char_stack.push(ch);
            if initial_offset.is_none() {
                *initial_offset = Some(offset);
            }
        }
        CharType::Operator => {
            if !char_stack.is_empty() && char_stack[char_stack.len() - 1] == '/' {
                if ch == '/' {
                    char_stack.pop();
                    *state = TokenizerState::ShortComment(state.clone().into());
                } else if ch == '*' {
                    char_stack.pop();
                    *state = TokenizerState::LongComment(false, offset - 1, state.clone().into());
                } else {
                    char_stack.push(ch);
                    if initial_offset.is_none() {
                        *initial_offset = Some(offset);
                    }
                }
            } else {
                char_stack.push(ch);
                if initial_offset.is_none() {
                    *initial_offset = Some(offset);
                }
            }
        }
        CharType::SingleQuote => {
            *initial_offset = Some(offset);
            *state = TokenizerState::SingleQuotes(false, offset);
        }
        CharType::DoubleQuote => {
            *initial_offset = Some(offset);
            *state = TokenizerState::DoubleQuotes(false, offset);
        }
        CharType::OpenDelimiter => {
            *initial_offset = Some(offset);
            *state = TokenizerState::Delimiters(vec![(ch, offset)]);
        }
        CharType::CloseDelimiter => {
            report::send(Report {
                is_error: true,
                filename: filename.to_string(),
                offset,
                message: "unmatched closing delimiter".to_string(),
                note: None,
                help: None,
                labels: vec![(
                    offset..offset + 1,
                    format!("no matching `{}`", get_opening_char(ch)),
                )],
            });
            return Err(());
        }
        CharType::Space => {}
    }
    Ok(())
}

fn tokenize_quotes(
    quote_info: (char, bool, usize),
    offset: usize,
    sender: &mpsc::UnboundedSender<Token>,
    ch: char,
    fsm: (
        &mut TokenizerState,
        &mut Vec<char>,
        &mut Option<usize>,
        &mut usize,
    ),
) -> Result<(), ()> {
    let (quote, escape, start_offset) = quote_info;
    let (state, char_stack, initial_offset, token_count) = fsm;
    let compute_state = |x| match quote {
        '\'' => TokenizerState::SingleQuotes(x, start_offset),
        '\"' => TokenizerState::DoubleQuotes(x, start_offset),
        _ => unreachable!(),
    };
    if ch == quote {
        if escape {
            char_stack.push(ch);
            *state = compute_state(false);
        } else {
            sender
                .send(match quote {
                    '\'' => Token::SingleQuotes(
                        char_stack.clone().into_iter().collect::<String>(),
                        start_offset..offset + 1,
                    ),
                    '\"' => Token::DoubleQuotes(
                        char_stack.clone().into_iter().collect::<String>(),
                        start_offset..offset + 1,
                    ),
                    _ => unreachable!(),
                })
                .unwrap();
            *token_count += 1;
            char_stack.clear();
            *initial_offset = None;
            *state = TokenizerState::Normal;
        }
    } else if ch == '\\' {
        if escape {
            *state = compute_state(false);
        } else {
            *state = compute_state(true);
        }
        char_stack.push(ch);
    } else if !char_stack.is_empty() && char_stack[char_stack.len() - 1] == '/' {
        if ch == '/' {
            char_stack.pop();
            *state = TokenizerState::ShortComment(compute_state(false).into());
        } else if ch == '*' {
            char_stack.pop();
            *state = TokenizerState::LongComment(false, offset - 1, compute_state(false).into());
        } else {
            *state = compute_state(escape);
        }
    } else {
        char_stack.push(ch);
        *state = compute_state(false);
    }
    Ok(())
}

fn tokenize_delimiters(
    mut stack: Vec<(char, usize)>,
    filename: String,
    offset: usize,
    sender: &mpsc::UnboundedSender<Token>,
    ch_info: (char, CharType),
    fsm: (
        &mut TokenizerState,
        &mut Vec<char>,
        &mut Option<usize>,
        &mut usize,
    ),
) -> Result<(), ()> {
    let (ch, ch_type) = ch_info;
    let (state, char_stack, initial_offset, token_count) = fsm;
    match ch_type {
        CharType::Operator => {
            if !char_stack.is_empty() && char_stack[char_stack.len() - 1] == '/' {
                if ch == '/' {
                    char_stack.push(ch);
                    *state = TokenizerState::ShortComment(TokenizerState::Delimiters(stack).into());
                } else if ch == '*' {
                    char_stack.push(ch);
                    *state = TokenizerState::LongComment(
                        false,
                        offset - 1,
                        TokenizerState::Delimiters(stack).into(),
                    );
                } else {
                    *state = TokenizerState::Delimiters(stack);
                }
            } else {
                char_stack.push(ch);
                *state = TokenizerState::Delimiters(stack);
            }
        }
        CharType::OpenDelimiter => {
            char_stack.push(ch);
            stack.push((ch, offset));
            *state = TokenizerState::Delimiters(stack);
        }
        CharType::CloseDelimiter => {
            if stack.is_empty() {
                panic!();
            }
            let (opening, opening_offset) = stack.pop().unwrap();
            if opening != get_opening_char(ch) {
                report::send(Report {
                    is_error: true,
                    filename,
                    offset,
                    message: "mismatched delimiters".to_string(),
                    note: None,
                    help: None,
                    labels: vec![
                        (
                            opening_offset..opening_offset + 1,
                            "opening delimiter".to_string(),
                        ),
                        (offset..offset + 1, "closing delimiter".to_string()),
                    ],
                });
                return Err(());
            }
            if stack.is_empty() {
                let s = char_stack.clone().into_iter().collect::<String>();
                sender
                    .send(match ch {
                        ')' => Token::Parentheses(s, initial_offset.unwrap()..offset + 1),
                        ']' => Token::Brackets(s, initial_offset.unwrap()..offset + 1),
                        '}' => Token::Braces(s, initial_offset.unwrap()..offset + 1),
                        _ => panic!(),
                    })
                    .unwrap();
                *token_count += 1;
                char_stack.clear();
                *initial_offset = None;
                *state = TokenizerState::Normal;
            } else {
                char_stack.push(ch);
                *state = TokenizerState::Delimiters(stack);
            }
        }
        _ => {
            char_stack.push(ch);
            *state = TokenizerState::Delimiters(stack);
        }
    }
    Ok(())
}

fn check_tokenizer_end(
    state: TokenizerState,
    first_offset: usize,
    token_count: usize,
    chunk_len: usize,
    filename: String,
    offset: usize,
) -> Result<(), ()> {
    if token_count < 1 {
        if first_offset == 0 {
            report::send(Report {
                is_error: true,
                filename: filename.clone(),
                offset: first_offset,
                message: format!("file {} is empty", filename),
                note: None,
                help: None,
                labels: vec![],
            });
        } else {
            report::send(Report {
                is_error: true,
                filename,
                offset,
                message: "empty delimited scope".to_string(),
                note: None,
                help: None,
                labels: vec![(
                    first_offset - 1..offset,
                    "no tokens between these delimiters".to_string(),
                )],
            });
        }
        return Err(());
    }
    match state {
        TokenizerState::Normal => Ok(()),
        TokenizerState::ShortComment(_) => Ok(()),
        TokenizerState::LongComment(_, start_offset, _) => {
            report::send(Report {
                is_error: true,
                filename,
                offset,
                message: "unterminated comment".to_string(),
                note: None,
                help: None,
                labels: vec![(
                    start_offset..chunk_len,
                    "comment reaches end of input".to_string(),
                )],
            });
            Err(())
        }
        TokenizerState::SingleQuotes(_, start_offset) => {
            report::send(Report {
                is_error: true,
                filename,
                offset,
                message: "unterminated single quotes".to_string(),
                note: None,
                help: None,
                labels: vec![(
                    start_offset..chunk_len,
                    "quoted text reaches end of input".to_string(),
                )],
            });
            Err(())
        }
        TokenizerState::DoubleQuotes(_, start_offset) => {
            report::send(Report {
                is_error: true,
                filename,
                offset,
                message: "unterminated double quotes".to_string(),
                note: None,
                help: None,
                labels: vec![(
                    start_offset..chunk_len,
                    "quoted text reaches end of input".to_string(),
                )],
            });
            Err(())
        }
        TokenizerState::Delimiters(stack) => {
            for (ch, offset) in stack {
                report::send(Report {
                    is_error: true,
                    filename: filename.clone(),
                    offset,
                    message: "unmatched opening delimiter".to_string(),
                    note: None,
                    help: None,
                    labels: vec![(
                        offset..offset + 1,
                        format!("no matching `{}`", get_closing_char(ch)),
                    )],
                });
            }
            Err(())
        }
    }
}

// A finite state machine-based tokenizer
// Only handles the top-level of the input,
// parenthesized expressions are produced as single tokens
pub(crate) async fn tokenize_chunk(
    chunk: &str,
    filename: String,
    mut offset: usize,
    sender: mpsc::UnboundedSender<Token>,
) -> Result<(), ()> {
    let mut state = TokenizerState::Normal;
    let mut char_stack = vec![];
    let first_offset = offset;
    let mut initial_offset = None;
    let mut token_count = 0;
    for ch in chunk.chars().chain(" ".chars()) {
        let ch_type = classify_char(ch);
        match state {
            TokenizerState::Normal => {
                tokenize_normal(
                    &filename,
                    offset,
                    &sender,
                    (ch, ch_type),
                    (
                        &mut state,
                        &mut char_stack,
                        &mut initial_offset,
                        &mut token_count,
                    ),
                )?;
            }
            TokenizerState::ShortComment(prev_state) => {
                if let TokenizerState::Delimiters(_) = *prev_state {
                    char_stack.push(ch);
                }
                if ch == '\n' {
                    state = *prev_state;
                } else {
                    state = TokenizerState::ShortComment(prev_state);
                }
            }
            TokenizerState::LongComment(tentative_end, start_offset, prev_state) => {
                if let TokenizerState::Delimiters(_) = *prev_state {
                    char_stack.push(ch);
                }
                if ch == '*' {
                    state = TokenizerState::LongComment(true, start_offset, prev_state);
                } else if ch == '/' && tentative_end {
                    state = *prev_state;
                } else {
                    state = TokenizerState::LongComment(tentative_end, start_offset, prev_state);
                }
            }
            TokenizerState::SingleQuotes(escape, start_offset) => {
                tokenize_quotes(
                    ('\'', escape, start_offset),
                    offset,
                    &sender,
                    ch,
                    (
                        &mut state,
                        &mut char_stack,
                        &mut initial_offset,
                        &mut token_count,
                    ),
                )?;
            }
            TokenizerState::DoubleQuotes(escape, start_offset) => {
                tokenize_quotes(
                    ('\"', escape, start_offset),
                    offset,
                    &sender,
                    ch,
                    (
                        &mut state,
                        &mut char_stack,
                        &mut initial_offset,
                        &mut token_count,
                    ),
                )?;
            }
            TokenizerState::Delimiters(ref stack) => tokenize_delimiters(
                stack.clone(),
                filename.clone(),
                offset,
                &sender,
                (ch, ch_type),
                (
                    &mut state,
                    &mut char_stack,
                    &mut initial_offset,
                    &mut token_count,
                ),
            )?,
        }
        offset += 1;
    }
    check_tokenizer_end(
        state,
        first_offset,
        token_count,
        chunk.len(),
        filename,
        offset,
    )
}
