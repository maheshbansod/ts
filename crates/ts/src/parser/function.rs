use crate::tokenizer::TokenKind;

use super::{PFunction, ParseResult, Parser, ParserError};

impl<'a> Parser<'a> {
    pub(super) fn parse_function(
        &mut self,
    ) -> ParseResult<'a, (PFunction<'a>, Vec<ParserError<'a>>)> {
        // todo - we need to make identifier compulsary for function statements i think.
        let identifier = self.parse_identifier().ok();
        self.expect_token(TokenKind::ParenthesisOpen)?;
        let mut args = vec![];
        loop {
            match self.parse_identifier() {
                Ok(identifier) => {
                    args.push(identifier);
                }
                Err(_e) => {
                    self.expect_token(TokenKind::ParenthesisClose)?;
                    break;
                }
            }
            let token = self
                .expect_token(TokenKind::Comma)
                .or_else(|_| self.expect_token(TokenKind::ParenthesisClose))?;
            if token.token_type() == &TokenKind::ParenthesisClose {
                break;
            }
        }
        self.expect_token(TokenKind::BraceOpen)?;
        let (statements, errors) = self.parse_block_statements(true)?;
        Ok((
            PFunction {
                identifier,
                arguments: args,
                body: statements,
            },
            errors,
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::{
            operator::{POperator, POperatorKind},
            BindingType, PAtom, PExpression, PFunction, PIdentifier, PJsExpression,
            PLiteralPrimitive, PStatement, ParseTree, ParseTreeRoot, Parser,
        },
        tokenizer::{Token, TokenKind, TokenLocation, Tokenizer},
    };

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
                    expression: PExpression::Js(PJsExpression::Atom(PAtom::Function(PFunction {
                        identifier: Some(PIdentifier {
                            token: Token::new(
                                TokenKind::Identifier,
                                TokenLocation { row: 2, column: 18 },
                                "f",
                            ),
                        }),
                        arguments: vec![],
                        body: vec![PStatement::Binding {
                            binding_type: BindingType::Let,
                            identifier: PIdentifier {
                                token: Token::new(
                                    TokenKind::Identifier,
                                    TokenLocation { row: 3, column: 5 },
                                    "y",
                                ),
                            },
                            #[cfg(feature = "ts")]
                            ts_type: None,
                            value: Some(PExpression::Js(PJsExpression::Cons(
                                POperator::new(
                                    POperatorKind::BinaryAdd,
                                    Token::new(
                                        TokenKind::Plus,
                                        TokenLocation { row: 3, column: 10 },
                                        "+",
                                    ),
                                ),
                                vec![
                                    PExpression::Js(PJsExpression::Atom(PAtom::Identifier(
                                        PIdentifier {
                                            token: Token::new(
                                                TokenKind::Identifier,
                                                TokenLocation { row: 3, column: 9 },
                                                "x",
                                            ),
                                        },
                                    ))),
                                    PExpression::Js(PJsExpression::Atom(PAtom::Literal(
                                        PLiteralPrimitive::Number {
                                            value: 1.0,
                                            token: Token::new(
                                                TokenKind::Literal,
                                                TokenLocation { row: 3, column: 11 },
                                                "1",
                                            ),
                                        },
                                    ))),
                                ],
                            ))),
                        }],
                    }))),
                }],
            },
        };
        assert_eq!(tree, expected_tree);
    }

    #[test]
    fn function_expression_minimal() {
        let code = "
let x = function () {};
        ";
        let tokenizer = Tokenizer::new(code);
        let parser = Parser::new(tokenizer);
        let (tree, errors) = parser.parse().expect("should parse");
        assert_eq!(errors, vec![]);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::Binding {
                    binding_type: BindingType::Let,
                    identifier: PIdentifier {
                        token: Token::new(
                            TokenKind::Identifier,
                            TokenLocation { row: 2, column: 5 },
                            "x",
                        ),
                    },
                    #[cfg(feature = "ts")]
                    ts_type: None,
                    value: Some(PExpression::Js(PJsExpression::Atom(PAtom::Function(
                        PFunction {
                            identifier: None,
                            arguments: vec![],
                            body: vec![],
                        },
                    )))),
                }],
            },
        };
        assert_eq!(expected_tree, tree);
    }

    #[test]
    fn function_with_args() {
        let code = "
function foo(arg1, arg2) {}
        ";
        let tokenizer = Tokenizer::new(code);
        let parser = Parser::new(tokenizer);
        let (tree, errors) = parser.parse().expect("should parse");
        assert_eq!(errors, vec![]);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::Expression {
                    expression: PExpression::Js(PJsExpression::Atom(PAtom::Function(PFunction {
                        identifier: Some(PIdentifier {
                            token: Token::new(
                                TokenKind::Identifier,
                                TokenLocation { row: 2, column: 10 },
                                "foo",
                            ),
                        }),
                        arguments: vec![
                            PIdentifier {
                                token: Token::new(
                                    TokenKind::Identifier,
                                    TokenLocation { row: 2, column: 14 },
                                    "arg1",
                                ),
                            },
                            PIdentifier {
                                token: Token::new(
                                    TokenKind::Identifier,
                                    TokenLocation { row: 2, column: 20 },
                                    "arg2",
                                ),
                            },
                        ],
                        body: vec![],
                    }))),
                }],
            },
        };
        assert_eq!(expected_tree, tree);
    }
}
