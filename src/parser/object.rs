use crate::tokenizer::TokenType;

use super::{PKeyValue, PObject, PObjectEntry, PObjectKey, ParseResult, Parser};

impl<'a> Parser<'a> {
    pub(super) fn parse_object(&mut self) -> ParseResult<'a, PObject<'a>> {
        let identifier = self.parse_identifier()?;
        self.expect_token(TokenType::Colon)?;
        let expression = self.parse_expression()?;
        let _ = self.expect_token(TokenType::Comma);
        self.expect_token(TokenType::BraceClose)?;
        let object = PObject {
            entries: vec![PObjectEntry::KeyValue(PKeyValue {
                key: PObjectKey::Identifier(identifier),
                value: expression,
            })],
        };
        Ok(object)
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
}
