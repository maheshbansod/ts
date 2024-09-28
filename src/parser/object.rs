use crate::tokenizer::TokenType;

use super::{PKeyValue, PObject, PObjectEntry, PObjectKey, ParseResult, Parser};

impl<'a> Parser<'a> {
    pub(super) fn parse_object(&mut self) -> ParseResult<'a, PObject<'a>> {
        let entries = self.object_entries()?;
        self.expect_token(TokenType::BraceClose)?;
        let _ = self.expect_token(TokenType::Comma);
        let object = PObject { entries };
        Ok(object)
    }

    fn object_entries(&mut self) -> ParseResult<'a, Vec<PObjectEntry<'a>>> {
        let mut object_entries = vec![];
        loop {
            if self.expect_token(TokenType::Destructure).is_ok() {
                let expression = self.parse_expression()?;
                let expression = PObjectEntry::Destructure(expression);
                object_entries.push(expression)
            } else {
                let key_value = self.key_value()?;
                let key_value = PObjectEntry::KeyValue(key_value);
                object_entries.push(key_value);
            }
            if self.expect_token(TokenType::Comma).is_err()
                || self.is_next_token(TokenType::BraceClose)
            {
                break;
            }
        }
        Ok(object_entries)
    }

    fn key_value(&mut self) -> ParseResult<'a, PKeyValue<'a>> {
        let key = if self.expect_token(TokenType::SquareBracketOpen).is_ok() {
            let expression = self.parse_expression()?;
            self.expect_token(TokenType::SquareBracketClose)?;
            PObjectKey::Expression(expression)
        } else if let Ok(literal) = self.expect_literal_primitive() {
            PObjectKey::Literal(literal)
        } else {
            let identifier = self.parse_identifier()?;
            PObjectKey::Identifier(identifier)
        };
        self.expect_token(TokenType::Colon)?;
        let expression = self.parse_expression()?;
        Ok(PKeyValue {
            key,
            value: expression,
        })
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::{
            parse_code, BindingType, PAtom, PExpression, PIdentifier, PKeyValue, PLiteralPrimitive,
            PObject, PObjectEntry, PObjectKey, PStatement, ParseResult, ParseTree, ParseTreeRoot,
        },
        tokenizer::{Token, TokenLocation, TokenType},
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn simple<'a>() -> ParseResult<'a, ()> {
        let (tree, errors) = parse_code(
            "
let code = {
    a: 1
}
",
        )?;
        assert_eq!(errors, vec![]);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::Binding {
                    binding_type: BindingType::Let,
                    identifier: PIdentifier {
                        token: Token::new(
                            TokenType::Identifier,
                            TokenLocation { row: 2, column: 5 },
                            "code",
                        ),
                    },
                    value: Some(PExpression::Atom(PAtom::ObjectLiteral(PObject {
                        entries: vec![PObjectEntry::KeyValue(PKeyValue {
                            key: PObjectKey::Identifier(PIdentifier {
                                token: Token::new(
                                    TokenType::Identifier,
                                    TokenLocation { row: 3, column: 5 },
                                    "a",
                                ),
                            }),
                            value: PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                                value: 1.0,
                                token: Token::new(
                                    TokenType::Literal,
                                    TokenLocation { row: 3, column: 8 },
                                    "1",
                                ),
                            })),
                        })],
                    }))),
                }],
            },
        };
        assert_eq!(expected_tree, tree);

        Ok(())
    }

    #[test]
    fn multiple_keys<'a>() -> ParseResult<'a, ()> {
        let (tree, errors) = parse_code(
            "
let code = {
    a: 1,
    b: 3
}
",
        )?;
        assert_eq!(errors, vec![]);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::Binding {
                    binding_type: BindingType::Let,
                    identifier: PIdentifier {
                        token: Token::new(
                            TokenType::Identifier,
                            TokenLocation { row: 2, column: 5 },
                            "code",
                        ),
                    },
                    value: Some(PExpression::Atom(PAtom::ObjectLiteral(PObject {
                        entries: vec![
                            PObjectEntry::KeyValue(PKeyValue {
                                key: PObjectKey::Identifier(PIdentifier {
                                    token: Token::new(
                                        TokenType::Identifier,
                                        TokenLocation { row: 3, column: 5 },
                                        "a",
                                    ),
                                }),
                                value: PExpression::Atom(PAtom::Literal(
                                    PLiteralPrimitive::Number {
                                        value: 1.0,
                                        token: Token::new(
                                            TokenType::Literal,
                                            TokenLocation { row: 3, column: 8 },
                                            "1",
                                        ),
                                    },
                                )),
                            }),
                            PObjectEntry::KeyValue(PKeyValue {
                                key: PObjectKey::Identifier(PIdentifier {
                                    token: Token::new(
                                        TokenType::Identifier,
                                        TokenLocation { row: 4, column: 5 },
                                        "b",
                                    ),
                                }),
                                value: PExpression::Atom(PAtom::Literal(
                                    PLiteralPrimitive::Number {
                                        value: 3.0,
                                        token: Token::new(
                                            TokenType::Literal,
                                            TokenLocation { row: 4, column: 8 },
                                            "3",
                                        ),
                                    },
                                )),
                            }),
                        ],
                    }))),
                }],
            },
        };
        assert_eq!(expected_tree, tree);

        Ok(())
    }

    #[test]
    fn comma_after_last_entry<'a>() -> ParseResult<'a, ()> {
        let (tree, errors) = parse_code(
            "
let code = {
    a: 1,
}
",
        )?;
        assert_eq!(errors, vec![]);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::Binding {
                    binding_type: BindingType::Let,
                    identifier: PIdentifier {
                        token: Token::new(
                            TokenType::Identifier,
                            TokenLocation { row: 2, column: 5 },
                            "code",
                        ),
                    },
                    value: Some(PExpression::Atom(PAtom::ObjectLiteral(PObject {
                        entries: vec![PObjectEntry::KeyValue(PKeyValue {
                            key: PObjectKey::Identifier(PIdentifier {
                                token: Token::new(
                                    TokenType::Identifier,
                                    TokenLocation { row: 3, column: 5 },
                                    "a",
                                ),
                            }),
                            value: PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                                value: 1.0,
                                token: Token::new(
                                    TokenType::Literal,
                                    TokenLocation { row: 3, column: 8 },
                                    "1",
                                ),
                            })),
                        })],
                    }))),
                }],
            },
        };
        assert_eq!(expected_tree, tree);

        Ok(())
    }

    #[test]
    fn destructure_identifier<'a>() -> ParseResult<'a, ()> {
        let (tree, errors) = parse_code(
            "
let code = {
    ...test,
}
",
        )?;
        assert_eq!(errors, vec![]);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::Binding {
                    binding_type: BindingType::Let,
                    identifier: PIdentifier {
                        token: Token::new(
                            TokenType::Identifier,
                            TokenLocation { row: 2, column: 5 },
                            "code",
                        ),
                    },
                    value: Some(PExpression::Atom(PAtom::ObjectLiteral(PObject {
                        entries: vec![PObjectEntry::Destructure(PExpression::Atom(
                            PAtom::Identifier(PIdentifier {
                                token: Token::new(
                                    TokenType::Identifier,
                                    TokenLocation { row: 3, column: 8 },
                                    "test",
                                ),
                            }),
                        ))],
                    }))),
                }],
            },
        };
        assert_eq!(expected_tree, tree);

        Ok(())
    }

    #[test]
    fn expression_key<'a>() -> ParseResult<'a, ()> {
        let (tree, errors) = parse_code(
            "
let code = {
    ['key']: 1
}
",
        )?;
        assert_eq!(errors, vec![]);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::Binding {
                    binding_type: BindingType::Let,
                    identifier: PIdentifier {
                        token: Token::new(
                            TokenType::Identifier,
                            TokenLocation { row: 2, column: 5 },
                            "code",
                        ),
                    },
                    value: Some(PExpression::Atom(PAtom::ObjectLiteral(PObject {
                        entries: vec![PObjectEntry::KeyValue(PKeyValue {
                            key: PObjectKey::Expression(PExpression::Atom(PAtom::Literal(
                                PLiteralPrimitive::String {
                                    value_token: Some(Token::new(
                                        TokenType::Literal,
                                        TokenLocation { row: 3, column: 7 },
                                        "key",
                                    )),
                                    value: "key",
                                    start_delim: Token::new(
                                        TokenType::StringLiteralStart,
                                        TokenLocation { row: 3, column: 6 },
                                        "'",
                                    ),
                                    end_delim: Token::new(
                                        TokenType::StringLiteralEnd,
                                        TokenLocation { row: 3, column: 10 },
                                        "'",
                                    ),
                                },
                            ))),
                            value: PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                                value: 1.0,
                                token: Token::new(
                                    TokenType::Literal,
                                    TokenLocation { row: 3, column: 14 },
                                    "1",
                                ),
                            })),
                        })],
                    }))),
                }],
            },
        };
        assert_eq!(expected_tree, tree);

        Ok(())
    }

    #[test]
    fn literal_key<'a>() -> ParseResult<'a, ()> {
        let (tree, errors) = parse_code(
            "
let code = {
    'key': 1
}
    ",
        )?;
        assert_eq!(errors, vec![]);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::Binding {
                    binding_type: BindingType::Let,
                    identifier: PIdentifier {
                        token: Token::new(
                            TokenType::Identifier,
                            TokenLocation { row: 2, column: 5 },
                            "code",
                        ),
                    },
                    value: Some(PExpression::Atom(PAtom::ObjectLiteral(PObject {
                        entries: vec![PObjectEntry::KeyValue(PKeyValue {
                            key: PObjectKey::Literal(PLiteralPrimitive::string("key", 3, 5, "'")),
                            value: PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                                value: 1.0,
                                token: Token::new(
                                    TokenType::Literal,
                                    TokenLocation { row: 3, column: 12 },
                                    "1",
                                ),
                            })),
                        })],
                    }))),
                }],
            },
        };
        assert_eq!(expected_tree, tree);

        Ok(())
    }
}