mod binding;
mod expression;
mod function;
mod identifier;
mod if_else;
mod literal;
mod object;
mod operator;

pub use binding::BindingType;
pub use operator::POperator;
pub use operator::POperatorKind;

use std::{
    error::Error,
    fmt::{Debug, Display},
    iter::Peekable,
};

use crate::tokenizer::{Token, TokenKind, TokenLocation, Tokenizer};

pub struct Parser<'a> {
    tokenizer: Peekable<Tokenizer<'a>>,
}

type ParseResult<'a, T> = Result<T, ParserError<'a>>;

impl<'a> Parser<'a> {
    pub fn new(tokenizer: Tokenizer<'a>) -> Self {
        Self {
            tokenizer: tokenizer.peekable(),
        }
    }

    pub fn parse(mut self) -> ParseResult<'a, (ParseTree<'a>, Vec<ParserError<'a>>)> {
        let (root, errors) = self.parse_root()?;
        Ok((ParseTree { root }, errors))
    }

    fn parse_root(&mut self) -> ParseResult<'a, (ParseTreeRoot<'a>, Vec<ParserError<'a>>)> {
        let (statements, errors) = self.parse_block_statements(false)?;
        Ok((ParseTreeRoot { statements }, errors))
    }

    /// Parses statements in a block and consumes the tokens including the closing brace
    /// It has no knowledge about opening brace so the caller should consume it
    fn parse_block_statements(
        &mut self,
        is_block: bool,
    ) -> ParseResult<'a, (Vec<PStatement<'a>>, Vec<ParserError<'a>>)> {
        let mut statements = vec![];
        let mut errors = vec![];
        loop {
            if is_block
                && self
                    .tokenizer
                    .peek()
                    .ok_or(ParserError::UnexpectedEof)?
                    .token_type()
                    == &TokenKind::BraceClose
            {
                self.tokenizer.next();
                break;
            }
            match self.parse_statement() {
                Ok((statement, mut statement_errors)) => {
                    statements.push(statement);
                    errors.append(&mut statement_errors);
                }
                Err(ParserError::UnexpectedEof) => {
                    if statements.is_empty() {
                        errors.push(ParserError::UnexpectedEof);
                    }
                    break;
                }
                Err(e) => {
                    errors.push(e);
                    // since the statement wasn't parsed properly, we should move ahead i guess -
                    // maybe i'll consume the next token for now - maybe later i'll see if we can
                    // move to next statement?
                    self.tokenizer.next();
                }
            }
        }
        Ok((statements, errors))
    }

    fn parse_statement(&mut self) -> ParseResult<'a, (PStatement<'a>, Vec<ParserError<'a>>)> {
        self.consume_comments();
        let (statement, errors) = if let Some(next_token) = self.tokenizer.peek() {
            if let Ok(binding_type) = next_token.token_type().clone().try_into() {
                (self.parse_binding(binding_type)?, vec![])
            } else if next_token.token_type() == &TokenKind::BraceOpen {
                // we're in a block hmmm
                self.tokenizer.next(); // consume brace
                let (statements, errors) = self.parse_block_statements(true)?;
                (PStatement::Block { statements }, errors)
            } else if next_token.token_type() == &TokenKind::If {
                self.tokenizer.next();
                let (statement, errors) = self.parse_if_else()?;
                (PStatement::If { statement }, errors)
            } else {
                // okay let's maybe parse expression directly here
                let expression = self.parse_expression()?;
                (PStatement::Expression { expression }, vec![])
            }
        } else {
            return Err(ParserError::UnexpectedEof);
        };
        self.optional_semicolon();
        Ok((statement, errors))
    }

    /// Consume comments if any
    fn consume_comments(&mut self) {
        while let Ok(_) = self.expect_token(TokenKind::Comment) {
            // ignore comment
        }
    }

    /// Check if a token is there
    fn is_next_token(&mut self, token_type: &TokenKind) -> bool {
        self.tokenizer
            .peek()
            .is_some_and(|token| token.token_type() == token_type)
    }

    /// Check if a token is there and consume
    fn expect_token(&mut self, token_type: TokenKind) -> ParseResult<'a, Token<'a>> {
        if let Some(token) = self.tokenizer.peek() {
            if token.token_type() == &token_type {
                let token = self.tokenizer.next().expect("We peeked in the above if");
                Ok(token)
            } else {
                Err(ParserError::ExpectedToken {
                    expected: token_type,
                    got: token.clone(),
                })
            }
        } else {
            Err(ParserError::UnexpectedEof)
        }
    }

    /// See if there's a semicolon next if so consume it
    fn optional_semicolon(&mut self) {
        let next_token = self.tokenizer.peek();
        if let Some(next_token) = next_token {
            if next_token.token_type() == &TokenKind::Semicolon {
                let _ = self.tokenizer.next();
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ParseTree<'a> {
    pub root: ParseTreeRoot<'a>,
}

#[derive(PartialEq)]
pub struct ParseTreeRoot<'a> {
    pub statements: Vec<PStatement<'a>>,
}

#[derive(PartialEq)]
pub enum PStatement<'a> {
    Binding {
        binding_type: BindingType,
        identifier: PIdentifier<'a>,
        value: Option<PExpression<'a>>,
    },
    Block {
        statements: Vec<PStatement<'a>>,
    },
    Expression {
        expression: PExpression<'a>,
    },
    If {
        statement: PIfElseStatement<'a>,
    },
}

impl Debug for ParseTreeRoot<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            f.debug_struct("ParseTreeRoot")
                .field("statements", &format_args!("{:#?}", self.statements))
                .finish()
        } else {
            f.debug_struct("ParseTreeRoot")
                .field("statement", &self.statements)
                .finish()
        }
    }
}

impl<'a> Debug for PStatement<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            match self {
                PStatement::Expression { expression } => write!(f, "expr({expression})"),
                _ => write!(f, "{self:?}"),
            }
        } else {
            match self {
                Self::Binding {
                    binding_type,
                    identifier,
                    value,
                } => write!(f, "{binding_type} {identifier} = {value:?}"),
                Self::Block { statements } => write!(f, "block({statements:?})"),
                Self::Expression { expression } => write!(f, "expr({expression:?})"),
                Self::If { statement } => write!(f, "if({statement:?})"),
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum PAtom<'a> {
    Literal(PLiteralPrimitive<'a>),
    ObjectLiteral(PObject<'a>),
    Identifier(PIdentifier<'a>),
    Function(PFunction<'a>),
}

impl<'a> PAtom<'a> {
    fn one_token(&self) -> &Token<'a> {
        match &self {
            Self::Literal(l) => match l {
                PLiteralPrimitive::Number { value: _, token } => token,
                PLiteralPrimitive::String { start_delim, .. } => start_delim,
            },
            Self::Function(_f) => {
                // should store 'function ' keyword
                todo!()
            }
            Self::Identifier(id) => &id.token,
            Self::ObjectLiteral(obj) => &obj.start_token,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum PLiteralPrimitive<'a> {
    Number {
        value: f32, // to check what value we should keep
        token: Token<'a>,
    },
    String {
        value_token: Option<Token<'a>>,
        value: &'a str,
        start_delim: Token<'a>,
        end_delim: Token<'a>,
    },
}

pub use self::expression::PExpression;

#[cfg(test)]
impl<'a> PLiteralPrimitive<'a> {
    fn string(s: &'a str, row: usize, column: usize, delim: &'a str) -> Self {
        let start_delim = Token::new(
            TokenKind::StringLiteralStart,
            TokenLocation { row, column },
            delim,
        );
        let value_token = Some(Token::new(
            TokenKind::Literal,
            TokenLocation {
                row,
                column: column + 1,
            },
            s,
        ));
        let end_delim = Token::new(
            TokenKind::StringLiteralEnd,
            TokenLocation {
                row,
                column: column + s.len() + 1,
            },
            delim,
        );
        let value = s;
        Self::String {
            value_token,
            value,
            start_delim,
            end_delim,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct PObject<'a> {
    pub entries: Vec<PObjectEntry<'a>>,
    /// Opening brace
    start_token: Token<'a>,
    /// Closing brace
    end_token: Token<'a>,
}

#[derive(Debug, PartialEq)]
pub enum PObjectEntry<'a> {
    KeyValue(PKeyValue<'a>),
    Destructure(PExpression<'a>),
}

#[derive(Debug, PartialEq)]
pub struct PKeyValue<'a> {
    pub key: PObjectKey<'a>,
    pub value: PExpression<'a>,
}

#[derive(Debug, PartialEq)]
pub enum PObjectKey<'a> {
    Identifier(PIdentifier<'a>),
    Expression(PExpression<'a>),
    Literal(PLiteralPrimitive<'a>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct PIdentifier<'a> {
    token: Token<'a>,
}

impl<'a> PIdentifier<'a> {
    pub const fn name(&self) -> &str {
        self.token.lexeme()
    }

    pub fn location(&self) -> &TokenLocation {
        self.token.location()
    }
}

#[derive(Debug, PartialEq)]
struct PIfStatement<'a> {
    condition: PExpression<'a>,
    body: Box<PStatement<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct PIfElseStatement<'a> {
    if_statement: PIfStatement<'a>,
    /// intermediate else ifs in sequence
    else_if_statements: Vec<PIfStatement<'a>>,
    /// final else
    else_body: Option<Box<PStatement<'a>>>,
}

#[derive(Debug, PartialEq)]
pub struct PFunction<'a> {
    identifier: Option<PIdentifier<'a>>,
    arguments: Vec<PIdentifier<'a>>,
    body: Vec<PStatement<'a>>,
}

#[allow(clippy::module_name_repetitions)]
#[derive(Debug, PartialEq, Eq)]
pub enum ParserError<'a> {
    ExpectedLiteral,
    ExpectedToken { expected: TokenKind, got: Token<'a> },
    UnexpectedEof,
    UnexpectedToken(Token<'a>),
}

impl Error for ParserError<'_> {}

impl<'a> Display for PLiteralPrimitive<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PLiteralPrimitive::Number { value, .. } => write!(f, "{value}"),
            PLiteralPrimitive::String { value, .. } => write!(f, "str({value})"),
        }
    }
}

impl<'a> Display for PObject<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for entry in &self.entries {
            write!(f, "{entry},")?;
        }
        Ok(())
    }
}

impl<'a> Display for PObjectEntry<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PObjectEntry::KeyValue(PKeyValue { .. }) => todo!(),
            PObjectEntry::Destructure(exp) => write!(f, "[{exp}]")?,
        }
        Ok(())
    }
}

impl<'a> Display for PIdentifier<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.lexeme())
    }
}

impl<'a> Display for PFunction<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fname = self
            .identifier
            .as_ref()
            .map_or("<anon>", |i| i.token.lexeme());
        write!(f, "function({fname})")?;
        for statement in &self.body {
            write!(f, "{statement}")?;
        }
        write!(f, "functionend({fname})")?;
        Ok(())
    }
}

impl<'a> Display for PStatement<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PStatement::Expression { expression } => writeln!(f, "{expression}"),
            _ => Debug::fmt(&self, f),
        }
    }
}

impl<'a> Display for ParserError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::UnexpectedToken(token) => {
                let location = token.location();
                write!(
                    f,
                    "Unexpected token {:?} at line {}, column {}",
                    token.lexeme(),
                    location.row,
                    location.column
                )
            }
            ParserError::UnexpectedEof => write!(f, "Unexpected end of file"),
            // todo: remove Expected token maybe or maybe have what is the current token too
            ParserError::ExpectedToken { expected, got } => {
                write!(f, "Expected token type {expected:?}, got {got:?}")
            }
            ParserError::ExpectedLiteral => todo!(),
        }
    }
}

#[cfg(test)]
fn parse_code(code: &str) -> ParseResult<'_, (ParseTree<'_>, Vec<ParserError<'_>>)> {
    let tokenizer = Tokenizer::new(code);
    let parser = Parser::new(tokenizer);
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::{Token, TokenKind, TokenLocation, Tokenizer};
    use operator::{POperator, POperatorKind};
    use pretty_assertions::assert_eq;

    #[test]
    fn it_should_parse() {
        let code = "
let x = 30;
let y = 100;
let z = x + y;
        ";
        let tokenizer = Tokenizer::new(code);
        let parser = Parser::new(tokenizer);
        let (tree, errors) = parser.parse().expect("parsing failure");

        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![
                    PStatement::Binding {
                        binding_type: BindingType::Let,
                        identifier: PIdentifier {
                            token: Token::new(
                                TokenKind::Identifier,
                                TokenLocation { row: 2, column: 5 },
                                "x",
                            ),
                        },
                        value: Some(PExpression::Atom(PAtom::Literal(
                            PLiteralPrimitive::Number {
                                value: 30.0,
                                token: Token::new(
                                    TokenKind::Literal,
                                    TokenLocation { row: 2, column: 9 },
                                    "30",
                                ),
                            },
                        ))),
                    },
                    PStatement::Binding {
                        binding_type: BindingType::Let,
                        identifier: PIdentifier {
                            token: Token::new(
                                TokenKind::Identifier,
                                TokenLocation { row: 3, column: 5 },
                                "y",
                            ),
                        },
                        value: Some(PExpression::Atom(PAtom::Literal(
                            PLiteralPrimitive::Number {
                                value: 100.0,
                                token: Token::new(
                                    TokenKind::Literal,
                                    TokenLocation { row: 3, column: 9 },
                                    "100",
                                ),
                            },
                        ))),
                    },
                    PStatement::Binding {
                        binding_type: BindingType::Let,
                        identifier: PIdentifier {
                            token: Token::new(
                                TokenKind::Identifier,
                                TokenLocation { row: 4, column: 5 },
                                "z",
                            ),
                        },
                        value: Some(PExpression::Cons(
                            POperator::new(
                                POperatorKind::BinaryAdd,
                                Token::new(
                                    TokenKind::Plus,
                                    TokenLocation { row: 4, column: 11 },
                                    "+",
                                ),
                            ),
                            vec![
                                PExpression::Atom(PAtom::Identifier(PIdentifier {
                                    token: Token::new(
                                        TokenKind::Identifier,
                                        TokenLocation { row: 4, column: 9 },
                                        "x",
                                    ),
                                })),
                                PExpression::Atom(PAtom::Identifier(PIdentifier {
                                    token: Token::new(
                                        TokenKind::Identifier,
                                        TokenLocation { row: 4, column: 13 },
                                        "y",
                                    ),
                                })),
                            ],
                        )),
                    },
                ],
            },
        };

        assert_eq!(errors, vec![]);
        assert_eq!(tree, expected_tree);
    }

    #[test]
    fn block() {
        let code = "
let x = 1;
{
    let y = 2;
    x + y;
}
y;
        ";
        let tokenizer = Tokenizer::new(code);
        let parser = Parser::new(tokenizer);
        let (tree, errors) = parser.parse().expect("should parse");
        assert_eq!(errors, vec![]);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![
                    PStatement::Binding {
                        binding_type: BindingType::Let,
                        identifier: PIdentifier {
                            token: Token::new(
                                TokenKind::Identifier,
                                TokenLocation { row: 2, column: 5 },
                                "x",
                            ),
                        },
                        value: Some(PExpression::Atom(PAtom::Literal(
                            PLiteralPrimitive::Number {
                                value: 1.0,
                                token: Token::new(
                                    TokenKind::Literal,
                                    TokenLocation { row: 2, column: 9 },
                                    "1",
                                ),
                            },
                        ))),
                    },
                    PStatement::Block {
                        statements: vec![
                            PStatement::Binding {
                                binding_type: BindingType::Let,
                                identifier: PIdentifier {
                                    token: Token::new(
                                        TokenKind::Identifier,
                                        TokenLocation { row: 4, column: 9 },
                                        "y",
                                    ),
                                },
                                value: Some(PExpression::Atom(PAtom::Literal(
                                    PLiteralPrimitive::Number {
                                        value: 2.0,
                                        token: Token::new(
                                            TokenKind::Literal,
                                            TokenLocation { row: 4, column: 13 },
                                            "2",
                                        ),
                                    },
                                ))),
                            },
                            PStatement::Expression {
                                expression: PExpression::Cons(
                                    POperator::new(
                                        POperatorKind::BinaryAdd,
                                        Token::new(
                                            TokenKind::Plus,
                                            TokenLocation { row: 5, column: 7 },
                                            "+",
                                        ),
                                    ),
                                    vec![
                                        PExpression::Atom(PAtom::Identifier(PIdentifier {
                                            token: Token::new(
                                                TokenKind::Identifier,
                                                TokenLocation { row: 5, column: 5 },
                                                "x",
                                            ),
                                        })),
                                        PExpression::Atom(PAtom::Identifier(PIdentifier {
                                            token: Token::new(
                                                TokenKind::Identifier,
                                                TokenLocation { row: 5, column: 9 },
                                                "y",
                                            ),
                                        })),
                                    ],
                                ),
                            },
                        ],
                    },
                    PStatement::Expression {
                        expression: PExpression::Atom(PAtom::Identifier(PIdentifier {
                            token: Token::new(
                                TokenKind::Identifier,
                                TokenLocation { row: 7, column: 1 },
                                "y",
                            ),
                        })),
                    },
                ],
            },
        };
        assert_eq!(expected_tree, tree);
    }

    #[test]
    fn error_expected() {
        let code = "
let x = function ( {};
        ";
        let tokenizer = Tokenizer::new(code);
        let parser = Parser::new(tokenizer);
        let (_tree, errors) = parser.parse().expect("should parse");
        assert_eq!(
            errors[0],
            ParserError::ExpectedToken {
                expected: TokenKind::ParenthesisClose,
                got: Token::new(
                    TokenKind::BraceOpen,
                    TokenLocation { row: 2, column: 20 },
                    "{"
                )
            }
        );
        // } - // editor is acting weird and parsing the above opening brace in string as an
        // opening brace.
    }
}
