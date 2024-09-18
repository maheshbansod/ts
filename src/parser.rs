use std::{fmt::Display, iter::Peekable};

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
        let mut statements = vec![];
        let mut errors = vec![];
        loop {
            match self.parse_statement() {
                Ok(statement) => statements.push(statement),
                Err(ParserError::UnexpectedEof) => {
                    if statements.len() == 0 {
                        return Err(ParserError::UnexpectedEof);
                    }
                    break;
                }
                Err(e) => errors.push(e),
            }
        }
        Ok((ParseTreeRoot { statements }, errors))
    }

    fn parse_statement(&mut self) -> ParseResult<'a, PStatement<'a>> {
        let statement = if let Some(next_token) = self.tokenizer.peek() {
            if next_token.token_type() == &TokenType::Let {
                // let binding
                self.parse_binding()?
            } else {
                let next_token = self.tokenizer.next().expect("token was already peeked");
                return Err(ParserError::UnexpectedToken(next_token));
            }
        } else {
            return Err(ParserError::UnexpectedEof);
        };
        self.optional_semicolon();
        Ok(statement)
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
        return Ok(statement);
    }

    fn parse_identifier(&mut self) -> ParseResult<'a, PIdentifier<'a>> {
        if let Some(token) = self.tokenizer.next() {
            if token.token_type() == &TokenType::Identifier {
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
        let token = self.tokenizer.next().ok_or(ParserError::UnexpectedEof)?;
        let mut lhs = if is_token_expression_atom(token.token_type()) {
            PExpression::Atom(self.atom_from_token(token).unwrap())
        } else if is_token_prefix_operator(token.token_type()) {
            let token_type = token.token_type();
            if let Some(((), bp)) = prefix_binding_power(&token_type) {
                let operator = self.token_as_operator(token).unwrap();
                let rhs = self.parse_expression_pratt(bp)?;
                PExpression::Cons(operator, vec![rhs])
            } else {
                panic!("expected prefix binding power"); // todo: should this be a ParserResult?
            }
        } else {
            todo!()
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

    fn atom_from_token(&self, token: Token<'a>) -> ParseResult<'a, PAtom<'a>> {
        match token.token_type() {
            TokenType::Literal => Ok(PAtom::Literal(PLiteral::Number {
                value: token.lexeme().parse::<f32>().expect("it to be a number"),
                token,
            })),
            TokenType::Identifier => Ok(PAtom::Identifier(PIdentifier { token })),
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
}

#[derive(Debug, PartialEq)]
enum PLiteral<'a> {
    Number {
        value: f32, // to check what value we should keep
        token: Token<'a>,
    },
}

#[derive(Debug, PartialEq)]
struct PIdentifier<'a> {
    token: Token<'a>,
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

impl<'a> Display for PExpression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PExpression::Atom(atom) => match atom {
                PAtom::Literal(literal) => write!(f, "{}", literal),
                PAtom::Identifier(identifier) => write!(f, "{}", identifier),
            },
            PExpression::Cons(operator, rest) => {
                write!(f, "{} ", operator)?;
                for expr in rest {
                    write!(f, "{} ", expr)?;
                }
                Ok(())
            }
        }
    }
}

impl<'a> Display for PLiteral<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PLiteral::Number { value, .. } => write!(f, "{}", value),
        }
    }
}

impl<'a> Display for PIdentifier<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.lexeme())
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
    fn unary_single_exp() {
        let code = "-1";
        let mut parser = Parser::new(Tokenizer::new(code));
        let tree = parser.parse_expression().expect("it should parse");

        assert_eq!(tree.to_string(), "- 1 ")
    }

    #[test]
    fn test_exp() {
        let code = "4 + 3 + -2";
        let mut parser = Parser::new(Tokenizer::new(code));
        let tree = parser.parse_expression().expect("it should parse");

        assert_eq!(tree.to_string(), "+ + 4 3  - 2  ")
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
    match token_type {
        TokenType::Plus | TokenType::Minus => true,
        _ => false,
    }
}

fn is_token_expression_atom(token_type: &TokenType) -> bool {
    match token_type {
        TokenType::Identifier | TokenType::Literal => true,
        _ => false,
    }
}
