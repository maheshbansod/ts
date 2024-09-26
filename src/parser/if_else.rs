use crate::tokenizer::TokenType;

use super::{PIfElseStatement, PIfStatement, ParseResult, Parser, ParserError};

impl<'a> Parser<'a> {
    fn parse_if(&mut self) -> ParseResult<'a, (PIfStatement<'a>, Vec<ParserError<'a>>)> {
        let mut errors = vec![];
        self.expect_token(TokenType::ParenthesisOpen)?;
        let condition = self.parse_expression()?;
        self.expect_token(TokenType::ParenthesisClose)?;
        let (body, mut body_errors) = self.parse_statement()?;
        errors.append(&mut body_errors);
        let statement = PIfStatement {
            condition,
            body: Box::new(body),
        };
        Ok((statement, errors))
    }

    /// Assuming the IF token is already parsed here
    pub(super) fn parse_if_else(
        &mut self,
    ) -> ParseResult<'a, (PIfElseStatement<'a>, Vec<ParserError<'a>>)> {
        let (if_statement, mut errors) = self.parse_if()?;
        let mut else_if_statements = vec![];
        let mut else_body = None;

        while self.expect_token(TokenType::Else).is_ok() {
            if self.expect_token(TokenType::If).is_ok() {
                let (if_statement, mut else_if_errors) = self.parse_if()?;
                else_if_statements.push(if_statement);
                errors.append(&mut else_if_errors);
            } else {
                let (body, mut body_errors) = self.parse_statement()?;
                errors.append(&mut body_errors);
                else_body = Some(Box::new(body));
                break;
            }
        }
        Ok((
            PIfElseStatement {
                if_statement,
                else_if_statements,
                else_body,
            },
            errors,
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::{
            parse_code, PAtom, PExpression, PIdentifier, PIfElseStatement, PIfStatement,
            PLiteralPrimitive, PStatement, ParseResult, ParseTree, ParseTreeRoot, Parser,
        },
        tokenizer::{Token, TokenLocation, TokenType, Tokenizer},
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn if_basic() {
        let code = "
if (1) {
x
}";
        let tokenizer = Tokenizer::new(code);
        let parser = Parser::new(tokenizer);
        let (tree, errors) = parser.parse().expect("should parse");
        assert_eq!(errors, vec![]);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::If {
                    statement: PIfElseStatement {
                        if_statement: PIfStatement {
                            condition: PExpression::Atom(PAtom::Literal(
                                PLiteralPrimitive::Number {
                                    value: 1.0,
                                    token: Token::new(
                                        TokenType::Literal,
                                        TokenLocation { row: 2, column: 5 },
                                        "1",
                                    ),
                                },
                            )),
                            body: Box::new(PStatement::Block {
                                statements: vec![PStatement::Expression {
                                    expression: PExpression::Atom(PAtom::Identifier(PIdentifier {
                                        token: Token::new(
                                            TokenType::Identifier,
                                            TokenLocation { row: 3, column: 1 },
                                            "x",
                                        ),
                                    })),
                                }],
                            }),
                        },
                        else_if_statements: vec![],
                        else_body: None,
                    },
                }],
            },
        };
        assert_eq!(tree, expected_tree);
    }

    #[test]
    fn if_else_basic() {
        let code = "
if (1) {
x
} else if (0) {
} else {
y
}
";
        let tokenizer = Tokenizer::new(code);
        let parser = Parser::new(tokenizer);
        let (tree, errors) = parser.parse().expect("should parse");
        assert_eq!(errors, vec![]);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::If {
                    statement: PIfElseStatement {
                        if_statement: PIfStatement {
                            condition: PExpression::Atom(PAtom::Literal(
                                PLiteralPrimitive::Number {
                                    value: 1.0,
                                    token: Token::new(
                                        TokenType::Literal,
                                        TokenLocation { row: 2, column: 5 },
                                        "1",
                                    ),
                                },
                            )),
                            body: Box::new(PStatement::Block {
                                statements: vec![PStatement::Expression {
                                    expression: PExpression::Atom(PAtom::Identifier(PIdentifier {
                                        token: Token::new(
                                            TokenType::Identifier,
                                            TokenLocation { row: 3, column: 1 },
                                            "x",
                                        ),
                                    })),
                                }],
                            }),
                        },
                        else_if_statements: vec![PIfStatement {
                            condition: PExpression::Atom(PAtom::Literal(
                                PLiteralPrimitive::Number {
                                    value: 0.0,
                                    token: Token::new(
                                        TokenType::Literal,
                                        TokenLocation { row: 4, column: 12 },
                                        "0",
                                    ),
                                },
                            )),
                            body: Box::new(PStatement::Block { statements: vec![] }),
                        }],
                        else_body: Some(Box::new(PStatement::Block {
                            statements: vec![PStatement::Expression {
                                expression: PExpression::Atom(PAtom::Identifier(PIdentifier {
                                    token: Token::new(
                                        TokenType::Identifier,
                                        TokenLocation { row: 6, column: 1 },
                                        "y",
                                    ),
                                })),
                            }],
                        })),
                    },
                }],
            },
        };
        assert_eq!(tree, expected_tree);
    }

    #[test]
    fn single_line<'a>() -> ParseResult<'a, ()> {
        let code = "if (true) 1 else 0";
        let (tree, errors) = parse_code(code)?;
        assert_eq!(errors, vec![]);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::If {
                    statement: PIfElseStatement {
                        if_statement: PIfStatement {
                            condition: PExpression::Atom(PAtom::Identifier(PIdentifier {
                                token: Token::new(
                                    TokenType::Identifier,
                                    TokenLocation { row: 1, column: 5 },
                                    "true",
                                ),
                            })),
                            body: Box::new(PStatement::Expression {
                                expression: PExpression::Atom(PAtom::Literal(
                                    PLiteralPrimitive::Number {
                                        value: 1.0,
                                        token: Token::new(
                                            TokenType::Literal,
                                            TokenLocation { row: 1, column: 11 },
                                            "1",
                                        ),
                                    },
                                )),
                            }),
                        },
                        else_if_statements: vec![],
                        else_body: Some(Box::new(PStatement::Expression {
                            expression: PExpression::Atom(PAtom::Literal(
                                PLiteralPrimitive::Number {
                                    value: 0.0,
                                    token: Token::new(
                                        TokenType::Literal,
                                        TokenLocation { row: 1, column: 18 },
                                        "0",
                                    ),
                                },
                            )),
                        })),
                    },
                }],
            },
        };
        assert_eq!(expected_tree, tree);
        Ok(())
    }
}
