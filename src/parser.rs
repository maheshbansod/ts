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
            if next_token.token_type() == &TokenType::Let {
                // let binding
                (self.parse_binding()?, vec![])
            } else if next_token.token_type() == &TokenType::BraceOpen {
                // we're in a block hmmm
                self.tokenizer.next(); // consume brace
                let (statements, errors) = self.parse_block_statements(true)?;
                (PStatement::Block { statements }, errors)
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

    fn parse_binding(&mut self) -> ParseResult<'a, PStatement<'a>> {
        self.expect_token(TokenType::Let)?;
        let binding_type = BindingType::Let;
        let identifier = self.parse_identifier()?;
        let value = {
            if let Ok(()) = self.expect_token(TokenType::Assign) {
                Some(self.parse_expression()?)
            } else {
                None
            }
        };
        let statement = PStatement::Binding {
            binding_type,
            identifier,
            value,
        };
        Ok(statement)
    }

    fn parse_identifier(&mut self) -> ParseResult<'a, PIdentifier<'a>> {
        if let Some(token) = self.tokenizer.peek() {
            if token.token_type() == &TokenType::Identifier {
                let token = self.tokenizer.next().unwrap();
                Ok(PIdentifier { token })
            } else {
                Err(ParserError::ExpectedToken(TokenType::Identifier))
            }
        } else {
            Err(ParserError::UnexpectedEof)
        }
    }

    fn parse_expression(&mut self) -> ParseResult<'a, PExpression<'a>> {
        self.parse_expression_pratt(0)
    }

    fn parse_expression_pratt(
        &mut self,
        min_binding_power: u8,
    ) -> ParseResult<'a, PExpression<'a>> {
        let token = self.tokenizer.peek().ok_or(ParserError::UnexpectedEof)?;
        let mut lhs = if is_token_prefix_operator(token.token_type()) {
            let token_type = token.token_type();
            if let Some(((), bp)) = prefix_binding_power(token_type) {
                let token = self.tokenizer.next().unwrap();
                let operator = self.token_as_operator(token).unwrap();
                let rhs = self.parse_expression_pratt(bp)?;
                PExpression::Cons(operator, vec![rhs])
            } else {
                let token = self.tokenizer.next().unwrap();
                return Err(ParserError::UnexpectedToken(token));
            }
        } else if let (Some(atom), _errors) = {
            let token = self.tokenizer.next().unwrap();
            self.try_parse_atom(token)
        }? {
            PExpression::Atom(atom)
        } else {
            let token = self.tokenizer.next().unwrap();
            return Err(ParserError::UnexpectedToken(token));
        };

        loop {
            let op_token = self.tokenizer.peek();
            if op_token.is_none() {
                break;
            }
            let op_token = op_token.unwrap();

            if let Some((l_bp, r_bp)) = infix_binding_power(op_token.token_type()) {
                if l_bp < min_binding_power {
                    break;
                }
                let token = self.tokenizer.next().expect("Already peeked ");
                let operator = self
                    .token_as_operator(token)
                    .expect("Already peeked and checked");
                let rhs = self.parse_expression_pratt(r_bp)?;
                lhs = PExpression::Cons(operator, vec![lhs, rhs]);

                continue;
            }
            break;
        }
        Ok(lhs)
    }

    fn try_parse_atom(
        &mut self,
        first_token: Token<'a>,
    ) -> ParseResult<'a, (Option<PAtom<'a>>, Vec<ParserError<'a>>)> {
        let token = first_token;
        match token.token_type() {
            TokenType::Literal => {
                if let Ok(n) = token.lexeme().parse::<f32>() {
                    Ok((
                        Some(PAtom::Literal(PLiteral::Number { value: n, token })),
                        vec![],
                    ))
                } else {
                    Err(ParserError::UnexpectedToken(token))
                }
            }
            TokenType::StringLiteralStart => {
                let start_token = token;
                if let Some(next) = self.tokenizer.next() {
                    let (value, value_token, end_token) = match next.token_type() {
                        TokenType::Literal => Ok((next.lexeme(), Some(next), None)),
                        TokenType::StringLiteralEnd => Ok(("", None, Some(next))),
                        _ => Err(ParserError::UnexpectedToken(next)),
                    }?;

                    let end_token = if end_token.is_none() {
                        if let Some(next) = self.tokenizer.next() {
                            if next.token_type() == &TokenType::StringLiteralEnd {
                                Ok(next)
                            } else {
                                Err(ParserError::UnexpectedToken(next))
                            }
                        } else {
                            Err(ParserError::UnexpectedEof)
                        }
                    } else {
                        Ok(end_token.unwrap())
                    }?;

                    Ok((
                        Some(PAtom::Literal(PLiteral::String {
                            start_delim: start_token,
                            end_delim: end_token,
                            value,
                            value_token,
                        })),
                        vec![],
                    ))
                } else {
                    Err(ParserError::UnexpectedEof)
                }
            }
            TokenType::Function => {
                let identifier = self.parse_identifier().ok();
                self.expect_token(TokenType::ParenthesisOpen)?;
                // todo parse args
                self.expect_token(TokenType::ParenthesisClose)?;
                self.expect_token(TokenType::BraceOpen)?;
                let (statements, errors) = self.parse_block_statements(true)?;
                Ok((
                    Some(PAtom::Function(PFunction {
                        identifier,
                        arguments: vec![],
                        body: statements,
                    })),
                    errors,
                ))
            }
            TokenType::Identifier => Ok((Some(PAtom::Identifier(PIdentifier { token })), vec![])),
            _ => Ok((None, vec![])),
        }
    }

    /// Check if a token is there and consume
    fn expect_token(&mut self, token_type: TokenType) -> ParseResult<'a, ()> {
        if !self
            .tokenizer
            .peek()
            .map(|token| (token.token_type() == &token_type))
            .unwrap_or(false)
        {
            Err(ParserError::ExpectedToken(token_type))
        } else {
            self.tokenizer.next().expect("We peeked in the above if");
            Ok(())
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

    fn token_as_operator(&self, token: Token<'a>) -> ParseResult<'a, POperator<'a>> {
        // if matches!(token.token_type(), TokenType::Plus) {
        //     Ok(POperator::BinaryAdd(token))
        // } else {
        //     Err(ParserError::UnexpectedToken(token))
        // }
        match token.token_type() {
            TokenType::Plus => Ok(POperator::BinaryAdd(token)),
            TokenType::Minus => Ok(POperator::Minus(token)),
            _ => Err(ParserError::UnexpectedToken(token)),
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
    Minus(Token<'a>),
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
}

#[derive(Debug, PartialEq)]
enum PExpression<'a> {
    Atom(PAtom<'a>),
    Cons(POperator<'a>, Vec<PExpression<'a>>),
}

#[derive(Debug, PartialEq)]
enum PAtom<'a> {
    Literal(PLiteral<'a>),
    Identifier(PIdentifier<'a>),
    Function(PFunction<'a>),
}

#[derive(Debug, PartialEq)]
enum PLiteral<'a> {
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

#[derive(Debug, PartialEq)]
struct PIdentifier<'a> {
    token: Token<'a>,
}

#[derive(Debug, PartialEq)]
struct PFunction<'a> {
    identifier: Option<PIdentifier<'a>>,
    /// Args - WIP
    arguments: Vec<()>,
    body: Vec<PStatement<'a>>,
}

#[derive(Debug, PartialEq)]
enum BindingType {
    Let,
}

#[derive(Debug, PartialEq)]
pub enum ParserError<'a> {
    UnexpectedEof,
    UnexpectedToken(Token<'a>),
    ExpectedToken(TokenType),
}

impl Error for ParserError<'_> {}

impl<'a> Display for PExpression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PExpression::Atom(atom) => match atom {
                PAtom::Literal(literal) => write!(f, "{}", literal),
                PAtom::Identifier(identifier) => write!(f, "{}", identifier),
                PAtom::Function(function) => write!(f, "{}", function),
            },
            PExpression::Cons(operator, rest) => {
                write!(f, "{} (", operator)?;
                for expr in rest {
                    write!(f, "{} ", expr)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl<'a> Display for PLiteral<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PLiteral::Number { value, .. } => write!(f, "{}", value),
            PLiteral::String { value, .. } => write!(f, "str({})", value),
        }
    }
}

impl<'a> Display for PIdentifier<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.lexeme())
    }
}

impl<'a> Display for PFunction<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fname = if let Some(i) = &self.identifier {
            i.token.lexeme()
        } else {
            "<anon>"
        };
        write!(f, "function({})", fname)?;
        for statement in &self.body {
            write!(f, "{statement}")?;
        }
        write!(f, "functionend({})", fname)?;
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
            POperator::Minus(_) => write!(f, "-"),
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
            ParserError::ExpectedToken(t) => write!(f, "Expected token type {:?}", t),
        }
    }
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
                        value: Some(PExpression::Atom(PAtom::Literal(PLiteral::Number {
                            value: 30.0,
                            token: Token::new(
                                TokenType::Literal,
                                TokenLocation { row: 2, column: 9 },
                                "30",
                            ),
                        }))),
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
                        value: Some(PExpression::Atom(PAtom::Literal(PLiteral::Number {
                            value: 100.0,
                            token: Token::new(
                                TokenType::Literal,
                                TokenLocation { row: 3, column: 9 },
                                "100",
                            ),
                        }))),
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
    fn it_should_parse_exp_atom() {
        let code = "1";
        let tokenizer = Tokenizer::new(code);
        let mut parser = Parser::new(tokenizer);
        assert_eq!(
            parser.parse_expression().expect("should parse"),
            PExpression::Atom(PAtom::Literal(PLiteral::Number {
                value: 1.0,
                token: Token::new(TokenType::Literal, TokenLocation { row: 1, column: 1 }, "1")
            }))
        );
        let code = "ident";
        let tokenizer = Tokenizer::new(code);
        let mut parser = Parser::new(tokenizer);
        assert_eq!(
            parser.parse_expression().expect("should parse"),
            PExpression::Atom(PAtom::Identifier(PIdentifier {
                token: Token::new(
                    TokenType::Identifier,
                    TokenLocation { row: 1, column: 1 },
                    "ident"
                )
            }))
        );
    }

    #[test]
    fn it_should_parse_string_atom() {
        let code = "'1'";
        let tokenizer = Tokenizer::new(code);
        let mut parser = Parser::new(tokenizer);
        assert_eq!(
            parser.parse_expression().expect("should parse"),
            PExpression::Atom(PAtom::Literal(PLiteral::String {
                value: "1",
                start_delim: Token::new(
                    TokenType::StringLiteralStart,
                    TokenLocation { row: 1, column: 1 },
                    "'"
                ),
                value_token: Some(Token::new(
                    TokenType::Literal,
                    TokenLocation { row: 1, column: 2 },
                    "1"
                )),
                end_delim: Token::new(
                    TokenType::StringLiteralEnd,
                    TokenLocation { row: 1, column: 3 },
                    "'"
                ),
            }))
        );
    }

    #[test]
    fn it_should_parse_empty_string_atom() {
        let code = "''";
        let tokenizer = Tokenizer::new(code);
        let mut parser = Parser::new(tokenizer);
        assert_eq!(
            parser.parse_expression().expect("should parse"),
            PExpression::Atom(PAtom::Literal(PLiteral::String {
                value: "",
                start_delim: Token::new(
                    TokenType::StringLiteralStart,
                    TokenLocation { row: 1, column: 1 },
                    "'"
                ),
                value_token: None,
                end_delim: Token::new(
                    TokenType::StringLiteralEnd,
                    TokenLocation { row: 1, column: 2 },
                    "'"
                ),
            }))
        );
    }

    #[test]
    fn unary_single_exp() {
        let code = "-1";
        let mut parser = Parser::new(Tokenizer::new(code));
        let tree = parser.parse_expression().expect("it should parse");

        assert_eq!(tree.to_string(), "- (1 )")
    }

    #[test]
    fn test_exp() {
        let code = "4 + 3 + -2";
        let mut parser = Parser::new(Tokenizer::new(code));
        let tree = parser.parse_expression().expect("it should parse");

        assert_eq!(tree.to_string(), "+ (+ (4 3 ) - (2 ) )")
    }

    #[test]
    fn it_should_parse_add_exp() {
        let code = "4 + 3";
        let tokenizer = Tokenizer::new(code);
        let mut parser = Parser::new(tokenizer);
        assert_eq!(
            parser.parse_expression().expect("should parse"),
            PExpression::Cons(
                POperator::BinaryAdd(Token::new(
                    TokenType::Plus,
                    TokenLocation { row: 1, column: 3 },
                    "+"
                )),
                vec![
                    PExpression::Atom(PAtom::Literal(PLiteral::Number {
                        value: 4.0,
                        token: Token::new(
                            TokenType::Literal,
                            TokenLocation { row: 1, column: 1 },
                            "4"
                        )
                    })),
                    PExpression::Atom(PAtom::Literal(PLiteral::Number {
                        value: 3.0,
                        token: Token::new(
                            TokenType::Literal,
                            TokenLocation { row: 1, column: 5 },
                            "3"
                        )
                    }))
                ]
            )
        );
        let code = "a + b";
        let tokenizer = Tokenizer::new(code);
        let mut parser = Parser::new(tokenizer);
        assert_eq!(
            parser.parse_expression().expect("should parse"),
            PExpression::Cons(
                POperator::BinaryAdd(Token::new(
                    TokenType::Plus,
                    TokenLocation { row: 1, column: 3 },
                    "+"
                )),
                vec![
                    PExpression::Atom(PAtom::Identifier(PIdentifier {
                        token: Token::new(
                            TokenType::Identifier,
                            TokenLocation { row: 1, column: 1 },
                            "a"
                        )
                    })),
                    PExpression::Atom(PAtom::Identifier(PIdentifier {
                        token: Token::new(
                            TokenType::Identifier,
                            TokenLocation { row: 1, column: 5 },
                            "b"
                        )
                    })),
                ]
            )
        );
    }

    #[test]
    fn expression_statement() {
        let code = "y;";
        let tokenizer = Tokenizer::new(code);
        let parser = Parser::new(tokenizer);
        let (tree, error) = parser.parse().expect("should parse");
        assert_eq!(error, vec![]);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::Expression {
                    expression: PExpression::Atom(PAtom::Identifier(PIdentifier {
                        token: Token::new(
                            TokenType::Identifier,
                            TokenLocation { row: 1, column: 1 },
                            "y",
                        ),
                    })),
                }],
            },
        };
        assert_eq!(expected_tree, tree);
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
                        value: Some(PExpression::Atom(PAtom::Literal(PLiteral::Number {
                            value: 1.0,
                            token: Token::new(
                                TokenType::Literal,
                                TokenLocation { row: 2, column: 9 },
                                "1",
                            ),
                        }))),
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
                                value: Some(PExpression::Atom(PAtom::Literal(PLiteral::Number {
                                    value: 2.0,
                                    token: Token::new(
                                        TokenType::Literal,
                                        TokenLocation { row: 4, column: 13 },
                                        "2",
                                    ),
                                }))),
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
    fn function() {
        let code = "
        function f() {
let y = x+1;
        }
        ";
        let tokenizer = Tokenizer::new(code);
        let parser = Parser::new(tokenizer);
        let (tree, errors) = parser.parse().expect("Should parse");
        assert_eq!(errors, vec![]);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::Expression {
                    expression: PExpression::Atom(PAtom::Function(PFunction {
                        identifier: Some(PIdentifier {
                            token: Token::new(
                                TokenType::Identifier,
                                TokenLocation { row: 2, column: 18 },
                                "f",
                            ),
                        }),
                        arguments: vec![],
                        body: vec![PStatement::Binding {
                            binding_type: BindingType::Let,
                            identifier: PIdentifier {
                                token: Token::new(
                                    TokenType::Identifier,
                                    TokenLocation { row: 3, column: 5 },
                                    "y",
                                ),
                            },
                            value: Some(PExpression::Cons(
                                POperator::BinaryAdd(Token::new(
                                    TokenType::Plus,
                                    TokenLocation { row: 3, column: 10 },
                                    "+",
                                )),
                                vec![
                                    PExpression::Atom(PAtom::Identifier(PIdentifier {
                                        token: Token::new(
                                            TokenType::Identifier,
                                            TokenLocation { row: 3, column: 9 },
                                            "x",
                                        ),
                                    })),
                                    PExpression::Atom(PAtom::Literal(PLiteral::Number {
                                        value: 1.0,
                                        token: Token::new(
                                            TokenType::Literal,
                                            TokenLocation { row: 3, column: 11 },
                                            "1",
                                        ),
                                    })),
                                ],
                            )),
                        }],
                    })),
                }],
            },
        };
        assert_eq!(tree, expected_tree);
    }
}

fn infix_binding_power(token_type: &TokenType) -> Option<(u8, u8)> {
    match token_type {
        TokenType::Plus => Some((1, 2)),
        _ => None,
    }
}
fn prefix_binding_power(token_type: &TokenType) -> Option<((), u8)> {
    match token_type {
        TokenType::Minus => Some(((), 6)),
        TokenType::Plus => Some(((), 4)),
        _ => None,
    }
}

fn is_token_prefix_operator(token_type: &TokenType) -> bool {
    matches!(token_type, TokenType::Plus | TokenType::Minus)
}
