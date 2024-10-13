use std::fmt::Display;

use crate::tokenizer::{Token, TokenKind};

use super::{operator::POperator, PAtom, PIdentifier, ParseResult, Parser, ParserError};

#[derive(Debug, PartialEq)]
pub enum PExpression<'a> {
    Atom(PAtom<'a>),
    Cons(POperator<'a>, Vec<PExpression<'a>>),
}

impl<'a> PExpression<'a> {
    pub fn one_token(&self) -> &Token<'a> {
        match self {
            Self::Atom(atom) => atom.one_token(),
            Self::Cons(operator, _args) => &operator.token,
        }
    }
}

impl<'a> Display for PExpression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PExpression::Atom(atom) => write!(f, "{atom}"),
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

impl<'a> Display for PAtom<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PAtom::Literal(literal) => write!(f, "{literal}"),
            PAtom::ObjectLiteral(object) => write!(f, "{{{object}}}"),
            PAtom::Identifier(identifier) => write!(f, "{identifier}"),
            PAtom::Function(function) => write!(f, "{function}"),
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
        self.consume_comments();
        let mut lhs = if let Some((operator, ((), r_bp))) =
            self.try_parse_prefix_operator(min_binding_power)
        {
            let rhs = self.parse_expression_pratt(r_bp)?;
            PExpression::Cons(operator, vec![rhs])
        } else if let (Some(atom), _errors) = self.try_parse_atom()? {
            PExpression::Atom(atom)
        } else if self.expect_token(TokenKind::ParenthesisOpen).is_ok() {
            let expression = self.parse_expression()?;
            self.expect_token(TokenKind::ParenthesisClose)?;
            expression
        } else {
            let token = self.tokenizer.peek().unwrap().clone();
            return Err(ParserError::UnexpectedToken(token));
        };

        loop {
            let op_token = self.tokenizer.peek();
            if op_token.is_none() {
                break;
            }

            if let Some((operator, (_, r_bp))) = self.try_parse_infix_operator(min_binding_power) {
                if operator.token_type() == &TokenKind::ParenthesisOpen {
                    // function call
                    let mut args = vec![];
                    // parse args
                    loop {
                        if let Ok(expression) = self.parse_expression() {
                            args.push(expression);
                            if self.expect_token(TokenKind::Comma).is_err() {
                                self.expect_token(TokenKind::ParenthesisClose)?;

                                break;
                            }
                        } else {
                            self.expect_token(TokenKind::ParenthesisClose)?;
                            break;
                        }
                    }
                    let mut call_args = vec![lhs];
                    call_args.extend(args);
                    lhs = PExpression::Cons(operator, call_args);
                } else if operator.token_type() == &TokenKind::QuestionMark {
                    let mhs = self.parse_expression_pratt(r_bp)?;
                    self.expect_token(TokenKind::Colon)?;
                    let rhs = self.parse_expression_pratt(r_bp)?;
                    lhs = PExpression::Cons(operator, vec![lhs, mhs, rhs]);
                } else {
                    let rhs = self.parse_expression_pratt(r_bp)?;
                    lhs = PExpression::Cons(operator, vec![lhs, rhs]);
                }

                continue;
            }
            if let Some((operator, (_l_bp, _r_bp))) =
                self.try_parse_postfix_operator(min_binding_power)
            {
                if operator.token_type() == &TokenKind::SquareBracketOpen {
                    // subscript operator []
                    let expression = self.parse_expression()?;
                    self.expect_token(TokenKind::SquareBracketClose)?;
                    lhs = PExpression::Cons(operator, vec![lhs, expression]);
                } else {
                    lhs = PExpression::Cons(operator, vec![lhs]);
                }

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
                TokenKind::Function => {
                    self.tokenizer.next();
                    let (function, errors) = self.parse_function()?;
                    Ok((Some(PAtom::Function(function)), errors))
                }
                TokenKind::Identifier => {
                    let token = self.tokenizer.next().unwrap();
                    Ok((Some(PAtom::Identifier(PIdentifier { token })), vec![]))
                }
                TokenKind::BraceOpen => {
                    let open_brace = self.tokenizer.next().unwrap();
                    let object = self.parse_object(open_brace)?;
                    Ok((Some(PAtom::ObjectLiteral(object)), vec![]))
                }
                _ => Ok((None, vec![])),
            }
        } else {
            Err(ParserError::UnexpectedEof)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::{
            expression::{PExpression, POperator},
            operator::POperatorKind,
            parse_code, PAtom, PIdentifier, PLiteralPrimitive, PStatement, ParseResult, ParseTree,
            ParseTreeRoot, Parser,
        },
        tokenizer::{Token, TokenKind, TokenLocation, Tokenizer},
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
                token: Token::new(TokenKind::Literal, TokenLocation { row: 1, column: 1 }, "1")
            }))
        );
        let code = "ident";
        let tokenizer = Tokenizer::new(code);
        let mut parser = Parser::new(tokenizer);
        assert_eq!(
            parser.parse_expression().expect("should parse"),
            PExpression::Atom(PAtom::Identifier(PIdentifier {
                token: Token::new(
                    TokenKind::Identifier,
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
    fn subscript<'a>() -> ParseResult<'a, ()> {
        let code = "4 * (3 - 2) * a[x] + 5";
        let mut parser = Parser::new(Tokenizer::new(code));
        let tree = parser.parse_expression()?;
        assert_eq!(tree.to_string(), "+ (* (* (4 - (3 2 ) ) [] (a x ) ) 5 )");
        Ok(())
    }

    #[test]
    fn brackets<'a>() -> ParseResult<'a, ()> {
        // with brackets
        let code = "4 * (3 - 2) * 1 + 5";
        let mut parser = Parser::new(Tokenizer::new(code));
        let tree = parser.parse_expression()?;
        assert_eq!(tree.to_string(), "+ (* (* (4 - (3 2 ) ) 1 ) 5 )");

        // without brackets
        let code = "4 * 3 - 2 * 1 + 5";
        let mut parser = Parser::new(Tokenizer::new(code));
        let tree = parser.parse_expression()?;
        assert_eq!(tree.to_string(), "+ (- (* (4 3 ) * (2 1 ) ) 5 )");
        Ok(())
    }

    #[test]
    fn it_should_parse_add_exp() {
        let code = "4 + 3";
        let tokenizer = Tokenizer::new(code);
        let mut parser = Parser::new(tokenizer);
        assert_eq!(
            parser.parse_expression().expect("should parse").to_string(),
            "+ (4 3 )"
        );
        let code = "a + b";
        let tokenizer = Tokenizer::new(code);
        let mut parser = Parser::new(tokenizer);
        assert_eq!(
            parser.parse_expression().expect("should parse").to_string(),
            "+ (a b )"
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
                            TokenKind::Identifier,
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
                        POperator {
                            kind: POperatorKind::Subtract,
                            token: Token::new(
                                TokenKind::Minus,
                                TokenLocation { row: 1, column: 3 },
                                "-",
                            ),
                        },
                        vec![
                            PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                                value: 3.0,
                                token: Token::new(
                                    TokenKind::Literal,
                                    TokenLocation { row: 1, column: 1 },
                                    "3",
                                ),
                            })),
                            PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                                value: 2.0,
                                token: Token::new(
                                    TokenKind::Literal,
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
                        POperator {
                            kind: POperatorKind::Multiply,
                            token: Token::new(
                                TokenKind::Star,
                                TokenLocation { row: 1, column: 3 },
                                "*",
                            ),
                        },
                        vec![
                            PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                                value: 3.0,
                                token: Token::new(
                                    TokenKind::Literal,
                                    TokenLocation { row: 1, column: 1 },
                                    "3",
                                ),
                            })),
                            PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                                value: 2.0,
                                token: Token::new(
                                    TokenKind::Literal,
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
                        POperator {
                            kind: POperatorKind::Divide,
                            token: (Token::new(
                                TokenKind::Slash,
                                TokenLocation { row: 1, column: 3 },
                                "/",
                            )),
                        },
                        vec![
                            PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                                value: 3.0,
                                token: Token::new(
                                    TokenKind::Literal,
                                    TokenLocation { row: 1, column: 1 },
                                    "3",
                                ),
                            })),
                            PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                                value: 2.0,
                                token: Token::new(
                                    TokenKind::Literal,
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
        let code = "a + b + c";
        let mut parser = Parser::new(Tokenizer::new(code));
        let tree = parser.parse_expression()?;
        assert_eq!(tree.to_string(), "+ (+ (a b ) c )");
        Ok(())
    }
    #[test]
    fn increment<'a>() -> ParseResult<'a, ()> {
        let (tree, errors) = parse_code("3 + a++ + ++b")?;
        assert_eq!(errors, vec![]);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::Expression {
                    expression: PExpression::Cons(
                        POperator {
                            kind: POperatorKind::BinaryAdd,
                            token: Token::new(
                                TokenKind::Plus,
                                TokenLocation { row: 1, column: 9 },
                                "+",
                            ),
                        },
                        vec![
                            PExpression::Cons(
                                POperator {
                                    kind: POperatorKind::BinaryAdd,
                                    token: Token::new(
                                        TokenKind::Plus,
                                        TokenLocation { row: 1, column: 3 },
                                        "+",
                                    ),
                                },
                                vec![
                                    PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                                        value: 3.0,
                                        token: Token::new(
                                            TokenKind::Literal,
                                            TokenLocation { row: 1, column: 1 },
                                            "3",
                                        ),
                                    })),
                                    PExpression::Cons(
                                        POperator {
                                            kind: POperatorKind::PostIncrement,
                                            token: Token::new(
                                                TokenKind::Increment,
                                                TokenLocation { row: 1, column: 6 },
                                                "++",
                                            ),
                                        },
                                        vec![PExpression::Atom(PAtom::Identifier(PIdentifier {
                                            token: Token::new(
                                                TokenKind::Identifier,
                                                TokenLocation { row: 1, column: 5 },
                                                "a",
                                            ),
                                        }))],
                                    ),
                                ],
                            ),
                            PExpression::Cons(
                                POperator {
                                    kind: POperatorKind::PreIncrement,
                                    token: Token::new(
                                        TokenKind::Increment,
                                        TokenLocation { row: 1, column: 11 },
                                        "++",
                                    ),
                                },
                                vec![PExpression::Atom(PAtom::Identifier(PIdentifier {
                                    token: Token::new(
                                        TokenKind::Identifier,
                                        TokenLocation { row: 1, column: 13 },
                                        "b",
                                    ),
                                }))],
                            ),
                        ],
                    ),
                }],
            },
        };
        assert_eq!(tree, expected_tree);
        Ok(())
    }

    #[test]
    fn function_call_multi_args<'a>() -> ParseResult<'a, ()> {
        let (tree, errors) = parse_code("foo(a, b)")?;
        assert_eq!(errors, vec![]);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::Expression {
                    expression: PExpression::Cons(
                        POperator {
                            kind: POperatorKind::FunctionCall,
                            token: Token::new(
                                TokenKind::ParenthesisOpen,
                                TokenLocation { row: 1, column: 4 },
                                "(",
                            ),
                        },
                        vec![
                            PExpression::Atom(PAtom::Identifier(PIdentifier {
                                token: Token::new(
                                    TokenKind::Identifier,
                                    TokenLocation { row: 1, column: 1 },
                                    "foo",
                                ),
                            })),
                            PExpression::Atom(PAtom::Identifier(PIdentifier {
                                token: Token::new(
                                    TokenKind::Identifier,
                                    TokenLocation { row: 1, column: 5 },
                                    "a",
                                ),
                            })),
                            PExpression::Atom(PAtom::Identifier(PIdentifier {
                                token: Token::new(
                                    TokenKind::Identifier,
                                    TokenLocation { row: 1, column: 8 },
                                    "b",
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
    fn function_call_no_args<'a>() -> ParseResult<'a, ()> {
        let code = "foo()";
        let mut parser = Parser::new(Tokenizer::new(code));
        let tree = parser.parse_expression()?;
        assert_eq!(tree.to_string(), "CALL (foo )");
        Ok(())
    }

    #[test]
    fn function_call<'a>() -> ParseResult<'a, ()> {
        let code = "foo(a)";
        let mut parser = Parser::new(Tokenizer::new(code));
        let tree = parser.parse_expression()?;
        assert_eq!(tree.to_string(), "CALL (foo a )");
        Ok(())
    }

    #[test]
    fn conditional_ternary<'a>() -> ParseResult<'a, ()> {
        let code = "a ? b : c";
        let mut parser = Parser::new(Tokenizer::new(code));
        let tree = parser.parse_expression()?;
        assert_eq!(tree.to_string(), "?: (a b c )");
        Ok(())
    }

    #[test]
    fn conditional_ternary2<'a>() -> ParseResult<'a, ()> {
        let code = "4 + 3 ? 3 - 2 * 4 : c";
        let mut parser = Parser::new(Tokenizer::new(code));
        let tree = parser.parse_expression()?;
        assert_eq!(tree.to_string(), "?: (+ (4 3 ) - (3 * (2 4 ) ) c )");
        Ok(())
    }

    #[test]
    fn equals<'a>() -> ParseResult<'a, ()> {
        let code = "4 == 3 ? true : false";
        let mut parser = Parser::new(Tokenizer::new(code));
        let tree = parser.parse_expression()?;
        assert_eq!(tree.to_string(), "?: (== (4 3 ) true false )");
        Ok(())
    }

    #[test]
    fn not_equals<'a>() -> ParseResult<'a, ()> {
        let code = "4 != 3 ? true : false";
        let mut parser = Parser::new(Tokenizer::new(code));
        let tree = parser.parse_expression()?;
        assert_eq!(tree.to_string(), "?: (!= (4 3 ) true false )");
        Ok(())
    }

    #[test]
    fn not_operator<'a>() -> ParseResult<'a, ()> {
        let code = "!0 ? 1 : 0";
        let mut parser = Parser::new(Tokenizer::new(code));
        let tree = parser.parse_expression()?;
        assert_eq!(tree.to_string(), "?: (! (0 ) 1 0 )");
        Ok(())
    }

    #[test]
    fn member_access<'a>() -> ParseResult<'a, ()> {
        let code = "4 + ++this.x - 1";
        let mut parser = Parser::new(Tokenizer::new(code));
        let tree = parser.parse_expression()?;
        assert_eq!(tree.to_string(), "+ (4 - (++ (-> (this x ) ) 1 ) )");
        Ok(())
    }

    #[test]
    fn member_access_multiple<'a>() -> ParseResult<'a, ()> {
        let code = "a.b.c";
        let mut parser = Parser::new(Tokenizer::new(code));
        let tree = parser.parse_expression()?;
        assert_eq!(tree.to_string(), "-> (-> (a b ) c )");
        Ok(())
    }

    #[test]
    fn method_calling<'a>() -> ParseResult<'a, ()> {
        let code = "console.log('hello')";
        let mut parser = Parser::new(Tokenizer::new(code));
        let tree = parser.parse_expression()?;
        assert_eq!(tree.to_string(), "CALL (-> (console log ) str(hello) )");
        Ok(())
    }

    #[test]
    fn assign<'a>() -> ParseResult<'a, ()> {
        let code = "a = b";
        let mut parser = Parser::new(Tokenizer::new(code));
        let tree = parser.parse_expression()?;
        assert_eq!(tree.to_string(), "= (a b )");
        Ok(())
    }

    #[test]
    fn assign_assoc<'a>() -> ParseResult<'a, ()> {
        let code = "a = b = c";
        let mut parser = Parser::new(Tokenizer::new(code));
        let tree = parser.parse_expression()?;
        assert_eq!(tree.to_string(), "= (a = (b c ) )");
        Ok(())
    }
}
