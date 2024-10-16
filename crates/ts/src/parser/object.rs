use crate::tokenizer::{Token, TokenKind};

use super::{PKeyValue, PObject, PObjectEntry, PObjectKey, ParseResult, Parser};

impl<'a> Parser<'a> {
    pub(super) fn parse_object(&mut self, start_token: Token<'a>) -> ParseResult<'a, PObject<'a>> {
        let entries = self.object_entries()?;
        let end_delim = self.expect_token(TokenKind::BraceClose)?;
        let _ = self.expect_token(TokenKind::Comma);
        let object = PObject {
            entries,
            start_token,
            end_token: end_delim,
        };
        Ok(object)
    }

    fn object_entries(&mut self) -> ParseResult<'a, Vec<PObjectEntry<'a>>> {
        let mut object_entries = vec![];
        loop {
            if self.expect_token(TokenKind::Destructure).is_ok() {
                let expression = self.parse_expression()?;
                let expression = PObjectEntry::Destructure(expression);
                object_entries.push(expression);
            } else if !self.is_next_token(&TokenKind::BraceClose) {
                let key_value = self.key_value()?;
                let key_value = PObjectEntry::KeyValue(key_value);
                object_entries.push(key_value);
            }
            if self.expect_token(TokenKind::Comma).is_err()
                || self.is_next_token(&TokenKind::BraceClose)
            {
                break;
            }
        }
        Ok(object_entries)
    }

    fn key_value(&mut self) -> ParseResult<'a, PKeyValue<'a>> {
        let key = if self.expect_token(TokenKind::SquareBracketOpen).is_ok() {
            let expression = self.parse_expression()?;
            self.expect_token(TokenKind::SquareBracketClose)?;
            PObjectKey::Expression(expression)
        } else if let Ok(literal) = self.expect_literal_primitive() {
            PObjectKey::Literal(literal)
        } else {
            let identifier = self.parse_identifier()?;
            PObjectKey::Identifier(identifier)
        };
        self.expect_token(TokenKind::Colon)?;
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
        tokenizer::{Token, TokenKind, TokenLocation},
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
                            TokenKind::Identifier,
                            TokenLocation { row: 2, column: 5 },
                            "code",
                        ),
                    },
                    #[cfg(feature = "ts")]
                    ts_type: None,
                    value: Some(PExpression::Atom(PAtom::ObjectLiteral(PObject {
                        start_token: Token::new(
                            TokenKind::BraceOpen,
                            TokenLocation { row: 2, column: 12 },
                            "{",
                        ),
                        end_token: Token::new(
                            TokenKind::BraceClose,
                            TokenLocation { row: 4, column: 1 },
                            "}",
                        ),
                        entries: vec![PObjectEntry::KeyValue(PKeyValue {
                            key: PObjectKey::Identifier(PIdentifier {
                                token: Token::new(
                                    TokenKind::Identifier,
                                    TokenLocation { row: 3, column: 5 },
                                    "a",
                                ),
                            }),
                            value: PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                                value: 1.0,
                                token: Token::new(
                                    TokenKind::Literal,
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
                            TokenKind::Identifier,
                            TokenLocation { row: 2, column: 5 },
                            "code",
                        ),
                    },
                    #[cfg(feature = "ts")]
                    ts_type: None,
                    value: Some(PExpression::Atom(PAtom::ObjectLiteral(PObject {
                        start_token: Token::new(
                            TokenKind::BraceOpen,
                            TokenLocation { row: 2, column: 12 },
                            "{",
                        ),
                        end_token: Token::new(
                            TokenKind::BraceClose,
                            TokenLocation { row: 5, column: 1 },
                            "}",
                        ),
                        entries: vec![
                            PObjectEntry::KeyValue(PKeyValue {
                                key: PObjectKey::Identifier(PIdentifier {
                                    token: Token::new(
                                        TokenKind::Identifier,
                                        TokenLocation { row: 3, column: 5 },
                                        "a",
                                    ),
                                }),
                                value: PExpression::Atom(PAtom::Literal(
                                    PLiteralPrimitive::Number {
                                        value: 1.0,
                                        token: Token::new(
                                            TokenKind::Literal,
                                            TokenLocation { row: 3, column: 8 },
                                            "1",
                                        ),
                                    },
                                )),
                            }),
                            PObjectEntry::KeyValue(PKeyValue {
                                key: PObjectKey::Identifier(PIdentifier {
                                    token: Token::new(
                                        TokenKind::Identifier,
                                        TokenLocation { row: 4, column: 5 },
                                        "b",
                                    ),
                                }),
                                value: PExpression::Atom(PAtom::Literal(
                                    PLiteralPrimitive::Number {
                                        value: 3.0,
                                        token: Token::new(
                                            TokenKind::Literal,
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
                            TokenKind::Identifier,
                            TokenLocation { row: 2, column: 5 },
                            "code",
                        ),
                    },
                    #[cfg(feature = "ts")]
                    ts_type: None,
                    value: Some(PExpression::Atom(PAtom::ObjectLiteral(PObject {
                        start_token: Token::new(
                            TokenKind::BraceOpen,
                            TokenLocation { row: 2, column: 12 },
                            "{",
                        ),
                        end_token: Token::new(
                            TokenKind::BraceClose,
                            TokenLocation { row: 4, column: 1 },
                            "}",
                        ),
                        entries: vec![PObjectEntry::KeyValue(PKeyValue {
                            key: PObjectKey::Identifier(PIdentifier {
                                token: Token::new(
                                    TokenKind::Identifier,
                                    TokenLocation { row: 3, column: 5 },
                                    "a",
                                ),
                            }),
                            value: PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                                value: 1.0,
                                token: Token::new(
                                    TokenKind::Literal,
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
                            TokenKind::Identifier,
                            TokenLocation { row: 2, column: 5 },
                            "code",
                        ),
                    },
                    #[cfg(feature = "ts")]
                    ts_type: None,
                    value: Some(PExpression::Atom(PAtom::ObjectLiteral(PObject {
                        start_token: Token::new(
                            TokenKind::BraceOpen,
                            TokenLocation { row: 2, column: 12 },
                            "{",
                        ),
                        end_token: Token::new(
                            TokenKind::BraceClose,
                            TokenLocation { row: 4, column: 1 },
                            "}",
                        ),
                        entries: vec![PObjectEntry::Destructure(PExpression::Atom(
                            PAtom::Identifier(PIdentifier {
                                token: Token::new(
                                    TokenKind::Identifier,
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
                            TokenKind::Identifier,
                            TokenLocation { row: 2, column: 5 },
                            "code",
                        ),
                    },
                    #[cfg(feature = "ts")]
                    ts_type: None,
                    value: Some(PExpression::Atom(PAtom::ObjectLiteral(PObject {
                        start_token: Token::new(
                            TokenKind::BraceOpen,
                            TokenLocation { row: 2, column: 12 },
                            "{",
                        ),
                        end_token: Token::new(
                            TokenKind::BraceClose,
                            TokenLocation { row: 4, column: 1 },
                            "}",
                        ),
                        entries: vec![PObjectEntry::KeyValue(PKeyValue {
                            key: PObjectKey::Expression(PExpression::Atom(PAtom::Literal(
                                PLiteralPrimitive::String {
                                    value_token: Some(Token::new(
                                        TokenKind::Literal,
                                        TokenLocation { row: 3, column: 7 },
                                        "key",
                                    )),
                                    value: "key",
                                    start_delim: Token::new(
                                        TokenKind::StringLiteralStart,
                                        TokenLocation { row: 3, column: 6 },
                                        "'",
                                    ),
                                    end_delim: Token::new(
                                        TokenKind::StringLiteralEnd,
                                        TokenLocation { row: 3, column: 10 },
                                        "'",
                                    ),
                                },
                            ))),
                            value: PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                                value: 1.0,
                                token: Token::new(
                                    TokenKind::Literal,
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
                            TokenKind::Identifier,
                            TokenLocation { row: 2, column: 5 },
                            "code",
                        ),
                    },
                    #[cfg(feature = "ts")]
                    ts_type: None,
                    value: Some(PExpression::Atom(PAtom::ObjectLiteral(PObject {
                        start_token: Token::new(
                            TokenKind::BraceOpen,
                            TokenLocation { row: 2, column: 12 },
                            "{",
                        ),
                        end_token: Token::new(
                            TokenKind::BraceClose,
                            TokenLocation { row: 4, column: 1 },
                            "}",
                        ),
                        entries: vec![PObjectEntry::KeyValue(PKeyValue {
                            key: PObjectKey::Literal(PLiteralPrimitive::string("key", 3, 5, "'")),
                            value: PExpression::Atom(PAtom::Literal(PLiteralPrimitive::Number {
                                value: 1.0,
                                token: Token::new(
                                    TokenKind::Literal,
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

    #[test]
    fn empty_object<'a>() -> ParseResult<'a, ()> {
        let (tree, errors) = parse_code("let a = {}")?;
        assert_eq!(errors.len(), 0);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::Binding {
                    binding_type: BindingType::Let,
                    identifier: PIdentifier {
                        token: Token::new(
                            TokenKind::Identifier,
                            TokenLocation { row: 1, column: 5 },
                            "a",
                        ),
                    },
                    #[cfg(feature = "ts")]
                    ts_type: None,
                    value: Some(PExpression::Atom(PAtom::ObjectLiteral(PObject {
                        entries: vec![],
                        start_token: Token::new(
                            TokenKind::BraceOpen,
                            TokenLocation { row: 1, column: 9 },
                            "{",
                        ),
                        end_token: Token::new(
                            TokenKind::BraceClose,
                            TokenLocation { row: 1, column: 10 },
                            "}",
                        ),
                    }))),
                }],
            },
        };
        assert_eq!(expected_tree, tree);
        Ok(())
    }
}
