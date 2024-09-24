use crate::tokenizer::TokenType;

use super::{PIdentifier, ParseResult, Parser, ParserError};

impl<'a> Parser<'a> {
    pub(super) fn parse_identifier(&mut self) -> ParseResult<'a, PIdentifier<'a>> {
        if let Some(token) = self.tokenizer.peek() {
            if token.token_type() == &TokenType::Identifier {
                let token = self.tokenizer.next().unwrap();
                Ok(PIdentifier { token })
            } else {
                Err(ParserError::ExpectedToken {
                    expected: TokenType::Identifier,
                    got: token.clone(),
                })
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
            parse_code, PAtom, PExpression, PIdentifier, PStatement, ParseTree, ParseTreeRoot,
        },
        tokenizer::{Token, TokenLocation, TokenType},
    };
    use pretty_assertions::assert_eq;

    #[test]
    fn sanity() {
        let (tree, errors) = parse_code("x");
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::Expression {
                    expression: PExpression::Atom(PAtom::Identifier(PIdentifier {
                        token: Token::new(
                            TokenType::Identifier,
                            TokenLocation { row: 1, column: 1 },
                            "x",
                        ),
                    })),
                }],
            },
        };
        assert_eq!(errors, vec![]);
        assert_eq!(tree, expected_tree);
    }
}