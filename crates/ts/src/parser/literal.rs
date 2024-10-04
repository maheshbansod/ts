use crate::tokenizer::TokenKind;

use super::{PLiteralPrimitive, ParseResult, Parser, ParserError};

impl<'a> Parser<'a> {
    pub(super) fn expect_literal_primitive(&mut self) -> ParseResult<'a, PLiteralPrimitive<'a>> {
        if let Ok(token) = self.expect_token(TokenKind::Literal) {
            if let Ok(n) = token.lexeme().parse::<f32>() {
                Ok(PLiteralPrimitive::Number { value: n, token })
            } else {
                Err(ParserError::UnexpectedToken(token))
            }
        } else if let Ok(start_token) = self.expect_token(TokenKind::StringLiteralStart) {
            if let Some(next) = self.tokenizer.next() {
                let (value, value_token, end_token) = match next.token_type() {
                    TokenKind::Literal => Ok((next.lexeme(), Some(next), None)),
                    TokenKind::StringLiteralEnd => Ok(("", None, Some(next))),
                    _ => Err(ParserError::UnexpectedToken(next)),
                }?;

                let end_token = if end_token.is_none() {
                    self.tokenizer
                        .next()
                        .map_or(Err(ParserError::UnexpectedEof), |next| {
                            if next.token_type() == &TokenKind::StringLiteralEnd {
                                Ok(next)
                            } else {
                                Err(ParserError::UnexpectedToken(next))
                            }
                        })
                } else {
                    Ok(end_token.unwrap())
                }?;

                Ok(PLiteralPrimitive::String {
                    start_delim: start_token,
                    end_delim: end_token,
                    value,
                    value_token,
                })
            } else {
                Err(ParserError::UnexpectedEof)
            }
        } else {
            Err(ParserError::ExpectedLiteral)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        parser::{PAtom, PExpression, PLiteralPrimitive, Parser},
        tokenizer::{Token, TokenKind, TokenLocation, Tokenizer},
    };

    #[test]
    fn string_atom() {
        let code = "'1'";
        let tokenizer = Tokenizer::new(code);
        let mut parser = Parser::new(tokenizer);
        assert_eq!(
            parser.parse_expression().expect("should parse"),
            PExpression::Atom(PAtom::Literal(PLiteralPrimitive::String {
                value: "1",
                start_delim: Token::new(
                    TokenKind::StringLiteralStart,
                    TokenLocation { row: 1, column: 1 },
                    "'"
                ),
                value_token: Some(Token::new(
                    TokenKind::Literal,
                    TokenLocation { row: 1, column: 2 },
                    "1"
                )),
                end_delim: Token::new(
                    TokenKind::StringLiteralEnd,
                    TokenLocation { row: 1, column: 3 },
                    "'"
                ),
            }))
        );
    }

    #[test]
    fn empty_string_atom() {
        let code = "''";
        let tokenizer = Tokenizer::new(code);
        let mut parser = Parser::new(tokenizer);
        assert_eq!(
            parser.parse_expression().expect("should parse"),
            PExpression::Atom(PAtom::Literal(PLiteralPrimitive::String {
                value: "",
                start_delim: Token::new(
                    TokenKind::StringLiteralStart,
                    TokenLocation { row: 1, column: 1 },
                    "'"
                ),
                value_token: None,
                end_delim: Token::new(
                    TokenKind::StringLiteralEnd,
                    TokenLocation { row: 1, column: 2 },
                    "'"
                ),
            }))
        );
    }
}
