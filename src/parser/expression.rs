use std::fmt::Display;

use crate::tokenizer::{Token, TokenType};

use super::{PAtom, PIdentifier, ParseResult, Parser, ParserError};

#[derive(Debug, PartialEq)]
pub(super) enum POperator<'a> {
    BinaryAdd(Token<'a>),
    Divide(Token<'a>),
    PostIncrement, // todo: include tokens or something maybe
    Multiply(Token<'a>),
    Negate(Token<'a>),
    Subtract(Token<'a>),
}

#[derive(Debug, PartialEq)]
pub(super) enum PExpression<'a> {
    Atom(PAtom<'a>),
    Cons(POperator<'a>, Vec<PExpression<'a>>),
}

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

impl<'a> Display for POperator<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            POperator::BinaryAdd(_) => write!(f, "+"),
            POperator::Divide(_) => write!(f, "/"),
            POperator::PostIncrement => write!(f, "++"),
            POperator::Multiply(_) => write!(f, "*"),
            POperator::Negate(_) => write!(f, "-"),
            POperator::Subtract(_) => write!(f, "-"),
        }
    }
}

impl<'a> Parser<'a> {
    pub(super) fn parse_expression(&mut self) -> ParseResult<'a, PExpression<'a>> {
        self.parse_expression_pratt(0)
    }

    fn parse_expression_pratt(
        &mut self,
        min_binding_power: u8,
    ) -> ParseResult<'a, PExpression<'a>> {
        let token = self.tokenizer.peek().ok_or(ParserError::UnexpectedEof)?;
        let mut lhs = if is_token_prefix_operator(token.token_type()) {
            let token_type = token.token_type().clone();
            let token = self.tokenizer.next().unwrap();
            if let Some(((), bp)) = prefix_binding_power(&token_type) {
                let operator = Parser::prefix_token_as_operator(token).unwrap();
                let rhs = self.parse_expression_pratt(bp)?;
                PExpression::Cons(operator, vec![rhs])
            } else {
                return Err(ParserError::UnexpectedToken(token));
            }
        } else if let (Some(atom), _errors) = self.try_parse_atom()? {
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
                let operator =
                    Parser::infix_token_as_operator(token).expect("Already peeked and checked");
                let rhs = self.parse_expression_pratt(r_bp)?;
                lhs = PExpression::Cons(operator, vec![lhs, rhs]);

                continue;
            }
            if let Some((l_bp, ())) = postfix_binding_power(op_token.token_type()) {
                if l_bp < min_binding_power {
                    break;
                }
                let token = self.tokenizer.next().expect("Already peeked ");
                let operator =
                    Parser::postfix_token_as_operator(token).expect("Already peeked and checked");
                lhs = PExpression::Cons(operator, vec![lhs]);

                continue;
            }
            break;
        }
        Ok(lhs)
    }

    fn try_parse_atom(&mut self) -> ParseResult<'a, (Option<PAtom<'a>>, Vec<ParserError<'a>>)> {
        if let Ok(literal) = self.expect_literal_primitive() {
            Ok((Some(PAtom::Literal(literal)), vec![]))
        } else if let Some(token) = self.tokenizer.peek() {
            match token.token_type() {
                TokenType::Function => {
                    self.tokenizer.next();
                    let (function, errors) = self.parse_function()?;
                    Ok((Some(PAtom::Function(function)), errors))
                }
                TokenType::Identifier => {
                    let token = self.tokenizer.next().unwrap();
                    Ok((Some(PAtom::Identifier(PIdentifier { token })), vec![]))
                }
                TokenType::BraceOpen => {
                    self.tokenizer.next();
                    let object = self.parse_object()?;
                    Ok((Some(PAtom::ObjectLiteral(object)), vec![]))
                }
                _ => Ok((None, vec![])),
            }
        } else {
            Err(ParserError::UnexpectedEof)
        }
    }

    const fn prefix_token_as_operator(token: Token<'a>) -> ParseResult<'a, POperator<'a>> {
        match token.token_type() {
            TokenType::Minus => Ok(POperator::Negate(token)),
            _ => Err(ParserError::UnexpectedToken(token)),
        }
    }

    const fn infix_token_as_operator(token: Token<'a>) -> ParseResult<'a, POperator<'a>> {
        match token.token_type() {
            TokenType::Minus => Ok(POperator::Subtract(token)),
            TokenType::Plus => Ok(POperator::BinaryAdd(token)),
            TokenType::Slash => Ok(POperator::Divide(token)),
            TokenType::Star => Ok(POperator::Multiply(token)),
            _ => Err(ParserError::UnexpectedToken(token)),
        }
    }

    const fn postfix_token_as_operator(token: Token<'a>) -> ParseResult<'a, POperator<'a>> {
        match token.token_type() {
            TokenType::Increment => Ok(POperator::PostIncrement),
            _ => Err(ParserError::UnexpectedToken(token)),
        }
    }
}

const fn infix_binding_power(token_type: &TokenType) -> Option<(u8, u8)> {
    match token_type {
        TokenType::Minus => Some((1, 2)),
        TokenType::Plus => Some((1, 2)),

        TokenType::Slash => Some((5, 6)),
        TokenType::Star => Some((5, 6)),
        _ => None,
    }
}
const fn prefix_binding_power(token_type: &TokenType) -> Option<((), u8)> {
    match token_type {
        TokenType::Minus => Some(((), 8)),
        TokenType::Plus => Some(((), 8)),
        _ => None,
    }
}
const fn postfix_binding_power(token_type: &TokenType) -> Option<(u8, ())> {
    match token_type {
        TokenType::Increment => Some((10, ())),
        _ => None,
    }
}

const fn is_token_prefix_operator(token_type: &TokenType) -> bool {
    matches!(token_type, TokenType::Plus | TokenType::Minus)
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::{
            expression::{PExpression, POperator},
            parse_code, PAtom, PIdentifier, PLiteralPrimitive, PStatement, ParseResult, ParseTree,
            ParseTreeRoot, Parser,
        },
        tokenizer::{Token, TokenLocation, TokenType, Tokenizer},
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn it_should_parse_exp_atom() {
        let code = "1";
        let tokenizer = Tokenizer::new(code);
        let mut parser = Parser::new(tokenizer);
        assert_eq!(
            parser.parse_expression().expect("should parse"),
            PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
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

        assert_eq!(tree.to_string(), "- (1 )");
    }

    #[test]
    fn test_exp() {
        let code = "4 + 3 + -2";
        let mut parser = Parser::new(Tokenizer::new(code));
        let tree = parser.parse_expression().expect("it should parse");

        assert_eq!(tree.to_string(), "+ (+ (4 3 ) - (2 ) )");
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
                    PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                        value: 4.0,
                        token: Token::new(
                            TokenType::Literal,
                            TokenLocation { row: 1, column: 1 },
                            "4"
                        )
                    })),
                    PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
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
    fn subtraction<'a>() -> ParseResult<'a, ()> {
        let (tree, errors) = parse_code("3 - 2")?;
        assert_eq!(errors, vec![]);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::Expression {
                    expression: PExpression::Cons(
                        POperator::Subtract(Token::new(
                            TokenType::Minus,
                            TokenLocation { row: 1, column: 3 },
                            "-",
                        )),
                        vec![
                            PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                                value: 3.0,
                                token: Token::new(
                                    TokenType::Literal,
                                    TokenLocation { row: 1, column: 1 },
                                    "3",
                                ),
                            })),
                            PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                                value: 2.0,
                                token: Token::new(
                                    TokenType::Literal,
                                    TokenLocation { row: 1, column: 5 },
                                    "2",
                                ),
                            })),
                        ],
                    ),
                }],
            },
        };
        assert_eq!(tree, expected_tree);
        Ok(())
    }

    #[test]
    fn multiplication<'a>() -> ParseResult<'a, ()> {
        let (tree, errors) = parse_code("3 * 2")?;
        assert_eq!(errors, vec![]);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::Expression {
                    expression: PExpression::Cons(
                        POperator::Multiply(Token::new(
                            TokenType::Star,
                            TokenLocation { row: 1, column: 3 },
                            "*",
                        )),
                        vec![
                            PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                                value: 3.0,
                                token: Token::new(
                                    TokenType::Literal,
                                    TokenLocation { row: 1, column: 1 },
                                    "3",
                                ),
                            })),
                            PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                                value: 2.0,
                                token: Token::new(
                                    TokenType::Literal,
                                    TokenLocation { row: 1, column: 5 },
                                    "2",
                                ),
                            })),
                        ],
                    ),
                }],
            },
        };
        assert_eq!(tree, expected_tree);
        Ok(())
    }

    #[test]
    fn division<'a>() -> ParseResult<'a, ()> {
        let (tree, errors) = parse_code("3 / 2")?;
        assert_eq!(errors, vec![]);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::Expression {
                    expression: PExpression::Cons(
                        POperator::Divide(Token::new(
                            TokenType::Slash,
                            TokenLocation { row: 1, column: 3 },
                            "/",
                        )),
                        vec![
                            PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                                value: 3.0,
                                token: Token::new(
                                    TokenType::Literal,
                                    TokenLocation { row: 1, column: 1 },
                                    "3",
                                ),
                            })),
                            PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                                value: 2.0,
                                token: Token::new(
                                    TokenType::Literal,
                                    TokenLocation { row: 1, column: 5 },
                                    "2",
                                ),
                            })),
                        ],
                    ),
                }],
            },
        };
        assert_eq!(tree, expected_tree);
        Ok(())
    }
    #[test]
    fn add_associativity<'a>() -> ParseResult<'a, ()> {
        let (tree, errors) = parse_code("a + b + c")?;
        assert_eq!(errors, vec![]);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::Expression {
                    expression: PExpression::Cons(
                        POperator::BinaryAdd(Token::new(
                            TokenType::Plus,
                            TokenLocation { row: 1, column: 7 },
                            "+",
                        )),
                        vec![
                            PExpression::Cons(
                                POperator::BinaryAdd(Token::new(
                                    TokenType::Plus,
                                    TokenLocation { row: 1, column: 3 },
                                    "+",
                                )),
                                vec![
                                    PExpression::Atom(PAtom::Identifier(PIdentifier {
                                        token: Token::new(
                                            TokenType::Identifier,
                                            TokenLocation { row: 1, column: 1 },
                                            "a",
                                        ),
                                    })),
                                    PExpression::Atom(PAtom::Identifier(PIdentifier {
                                        token: Token::new(
                                            TokenType::Identifier,
                                            TokenLocation { row: 1, column: 5 },
                                            "b",
                                        ),
                                    })),
                                ],
                            ),
                            PExpression::Atom(PAtom::Identifier(PIdentifier {
                                token: Token::new(
                                    TokenType::Identifier,
                                    TokenLocation { row: 1, column: 9 },
                                    "c",
                                ),
                            })),
                        ],
                    ),
                }],
            },
        };
        assert_eq!(expected_tree, tree);
        Ok(())
    }
    #[test]
    fn post_increment<'a>() -> ParseResult<'a, ()> {
        let (tree, errors) = parse_code("3 + a++ + 2")?;
        assert_eq!(errors, vec![]);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::Expression {
                    expression: PExpression::Cons(
                        POperator::BinaryAdd(Token::new(
                            TokenType::Plus,
                            TokenLocation { row: 1, column: 9 },
                            "+",
                        )),
                        vec![
                            PExpression::Cons(
                                POperator::BinaryAdd(Token::new(
                                    TokenType::Plus,
                                    TokenLocation { row: 1, column: 3 },
                                    "+",
                                )),
                                vec![
                                    PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                                        value: 3.0,
                                        token: Token::new(
                                            TokenType::Literal,
                                            TokenLocation { row: 1, column: 1 },
                                            "3",
                                        ),
                                    })),
                                    PExpression::Cons(
                                        POperator::PostIncrement,
                                        vec![PExpression::Atom(PAtom::Identifier(PIdentifier {
                                            token: Token::new(
                                                TokenType::Identifier,
                                                TokenLocation { row: 1, column: 5 },
                                                "a",
                                            ),
                                        }))],
                                    ),
                                ],
                            ),
                            PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                                value: 2.0,
                                token: Token::new(
                                    TokenType::Literal,
                                    TokenLocation { row: 1, column: 11 },
                                    "2",
                                ),
                            })),
                        ],
                    ),
                }],
            },
        };
        assert_eq!(tree, expected_tree);
        Ok(())
    }
}
