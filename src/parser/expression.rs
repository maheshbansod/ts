use crate::tokenizer::{Token, TokenType};

use super::{PAtom, PExpression, PIdentifier, PLiteralPrimitive, ParseResult, Parser, ParserError};

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

#[cfg(test)]
mod tests {
    use crate::{
        parser::{
            PAtom, PExpression, PIdentifier, PLiteralPrimitive, POperator, PStatement, ParseTree,
            ParseTreeRoot, Parser,
        },
        tokenizer::{Token, TokenLocation, TokenType, Tokenizer},
    };

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
}
