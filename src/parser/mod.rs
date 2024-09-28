mod binding;
mod expression;
mod function;
mod identifier;
mod if_else;
mod literal;
mod object;

use std::{error::Error, fmt::Display, iter::Peekable};

use crate::tokenizer::{Token, TokenType, Tokenizer};

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
                    == &TokenType::BraceClose
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
                Err(e) => errors.push(e),
            }
        }
        Ok((statements, errors))
    }

    fn parse_statement(&mut self) -> ParseResult<'a, (PStatement<'a>, Vec<ParserError<'a>>)> {
        let (statement, errors) = if let Some(next_token) = self.tokenizer.peek() {
            if let Ok(binding_type) = next_token.token_type().clone().try_into() {
                (self.parse_binding(binding_type)?, vec![])
            } else if next_token.token_type() == &TokenType::BraceOpen {
                // we're in a block hmmm
                self.tokenizer.next(); // consume brace
                let (statements, errors) = self.parse_block_statements(true)?;
                (PStatement::Block { statements }, errors)
            } else if next_token.token_type() == &TokenType::If {
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

    /// Check if a token is there
    fn is_next_token(&mut self, token_type: &TokenType) -> bool {
        self.tokenizer
            .peek()
            .is_some_and(|token| token.token_type() == token_type)
    }

    /// Check if a token is there and consume
    fn expect_token(&mut self, token_type: TokenType) -> ParseResult<'a, Token<'a>> {
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
            if next_token.token_type() == &TokenType::Semicolon {
                let _ = self.tokenizer.next();
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ParseTree<'a> {
    root: ParseTreeRoot<'a>,
}

#[derive(Debug, PartialEq)]
struct ParseTreeRoot<'a> {
    statements: Vec<PStatement<'a>>,
}

#[derive(Debug, PartialEq)]
enum POperator<'a> {
    BinaryAdd(Token<'a>),
    Negate(Token<'a>),
    Multiply(Token<'a>),
    Subtract(Token<'a>),
}

#[derive(Debug, PartialEq)]
enum PStatement<'a> {
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

#[derive(Debug, PartialEq)]
enum PExpression<'a> {
    Atom(PAtom<'a>),
    Cons(POperator<'a>, Vec<PExpression<'a>>),
}

#[derive(Debug, PartialEq)]
enum PAtom<'a> {
    Literal(PLiteralPrimitive<'a>),
    ObjectLiteral(PObject<'a>),
    Identifier(PIdentifier<'a>),
    Function(PFunction<'a>),
}

#[derive(Debug, PartialEq)]
enum PLiteralPrimitive<'a> {
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

#[cfg(test)]
use crate::tokenizer::TokenLocation;

#[cfg(test)]
impl<'a> PLiteralPrimitive<'a> {
    fn string(s: &'a str, row: usize, column: usize, delim: &'a str) -> Self {
        let start_delim = Token::new(
            TokenType::StringLiteralStart,
            TokenLocation { row, column },
            delim,
        );
        let value_token = Some(Token::new(
            TokenType::Literal,
            TokenLocation {
                row,
                column: column + 1,
            },
            s,
        ));
        let end_delim = Token::new(
            TokenType::StringLiteralEnd,
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
struct PObject<'a> {
    entries: Vec<PObjectEntry<'a>>,
}

#[derive(Debug, PartialEq)]
enum PObjectEntry<'a> {
    KeyValue(PKeyValue<'a>),
    Destructure(PExpression<'a>),
}

#[derive(Debug, PartialEq)]
struct PKeyValue<'a> {
    key: PObjectKey<'a>,
    value: PExpression<'a>,
}

#[derive(Debug, PartialEq)]
enum PObjectKey<'a> {
    Identifier(PIdentifier<'a>),
    Expression(PExpression<'a>),
    Literal(PLiteralPrimitive<'a>),
}

#[derive(Debug, PartialEq)]
struct PIdentifier<'a> {
    token: Token<'a>,
}

#[derive(Debug, PartialEq)]
struct PIfStatement<'a> {
    condition: PExpression<'a>,
    body: Box<PStatement<'a>>,
}

#[derive(Debug, PartialEq)]
struct PIfElseStatement<'a> {
    if_statement: PIfStatement<'a>,
    /// intermediate else ifs in sequence
    else_if_statements: Vec<PIfStatement<'a>>,
    /// final else
    else_body: Option<Box<PStatement<'a>>>,
}

#[derive(Debug, PartialEq)]
struct PFunction<'a> {
    identifier: Option<PIdentifier<'a>>,
    arguments: Vec<PIdentifier<'a>>,
    body: Vec<PStatement<'a>>,
}

#[derive(Debug, PartialEq)]
enum BindingType {
    Let,
    Const,
}

#[allow(clippy::module_name_repetitions)]
#[derive(Debug, PartialEq, Eq)]
pub enum ParserError<'a> {
    ExpectedLiteral,
    ExpectedToken { expected: TokenType, got: Token<'a> },
    UnexpectedEof,
    UnexpectedToken(Token<'a>),
}

impl Error for ParserError<'_> {}

impl<'a> Display for PExpression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PExpression::Atom(atom) => match atom {
                PAtom::Literal(literal) => write!(f, "{literal}"),
                PAtom::ObjectLiteral(object) => write!(f, "{{{object}}}"),
                PAtom::Identifier(identifier) => write!(f, "{identifier}"),
                PAtom::Function(function) => write!(f, "{function}"),
            },
            PExpression::Cons(operator, rest) => {
                write!(f, "{operator} (")?;
                for expr in rest {
                    write!(f, "{expr} ")?;
                }
                write!(f, ")")
            }
        }
    }
}

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
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl<'a> Display for POperator<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            POperator::BinaryAdd(_) => write!(f, "+"),
            POperator::Negate(_) => write!(f, "-"),
            POperator::Multiply(_) => write!(f, "*"),
            POperator::Subtract(_) => write!(f, "-"),
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

impl TryFrom<TokenType> for BindingType {
    type Error = ();

    fn try_from(value: TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::Const => Ok(Self::Const),
            TokenType::Let => Ok(Self::Let),
            _ => Err(()),
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
    use crate::tokenizer::{Token, TokenLocation, TokenType, Tokenizer};
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
                                TokenType::Identifier,
                                TokenLocation { row: 2, column: 5 },
                                "x",
                            ),
                        },
                        value: Some(PExpression::Atom(PAtom::Literal(
                            PLiteralPrimitive::Number {
                                value: 30.0,
                                token: Token::new(
                                    TokenType::Literal,
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
                                TokenType::Identifier,
                                TokenLocation { row: 3, column: 5 },
                                "y",
                            ),
                        },
                        value: Some(PExpression::Atom(PAtom::Literal(
                            PLiteralPrimitive::Number {
                                value: 100.0,
                                token: Token::new(
                                    TokenType::Literal,
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
                                TokenType::Identifier,
                                TokenLocation { row: 4, column: 5 },
                                "z",
                            ),
                        },
                        value: Some(PExpression::Cons(
                            POperator::BinaryAdd(Token::new(
                                TokenType::Plus,
                                TokenLocation { row: 4, column: 11 },
                                "+",
                            )),
                            vec![
                                PExpression::Atom(PAtom::Identifier(PIdentifier {
                                    token: Token::new(
                                        TokenType::Identifier,
                                        TokenLocation { row: 4, column: 9 },
                                        "x",
                                    ),
                                })),
                                PExpression::Atom(PAtom::Identifier(PIdentifier {
                                    token: Token::new(
                                        TokenType::Identifier,
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
                                TokenType::Identifier,
                                TokenLocation { row: 2, column: 5 },
                                "x",
                            ),
                        },
                        value: Some(PExpression::Atom(PAtom::Literal(
                            PLiteralPrimitive::Number {
                                value: 1.0,
                                token: Token::new(
                                    TokenType::Literal,
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
                                        TokenType::Identifier,
                                        TokenLocation { row: 4, column: 9 },
                                        "y",
                                    ),
                                },
                                value: Some(PExpression::Atom(PAtom::Literal(
                                    PLiteralPrimitive::Number {
                                        value: 2.0,
                                        token: Token::new(
                                            TokenType::Literal,
                                            TokenLocation { row: 4, column: 13 },
                                            "2",
                                        ),
                                    },
                                ))),
                            },
                            PStatement::Expression {
                                expression: PExpression::Cons(
                                    POperator::BinaryAdd(Token::new(
                                        TokenType::Plus,
                                        TokenLocation { row: 5, column: 7 },
                                        "+",
                                    )),
                                    vec![
                                        PExpression::Atom(PAtom::Identifier(PIdentifier {
                                            token: Token::new(
                                                TokenType::Identifier,
                                                TokenLocation { row: 5, column: 5 },
                                                "x",
                                            ),
                                        })),
                                        PExpression::Atom(PAtom::Identifier(PIdentifier {
                                            token: Token::new(
                                                TokenType::Identifier,
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
                                TokenType::Identifier,
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
            errors,
            vec![ParserError::ExpectedToken {
                expected: TokenType::ParenthesisClose,
                got: Token::new(
                    TokenType::BraceOpen,
                    TokenLocation { row: 2, column: 20 },
                    "{"
                )
            }]
        );
        // } - // editor is acting weird and parsing the above opening brace in string as an
        // opening brace.
    }
}
