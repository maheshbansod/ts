use std::fmt::Display;

use crate::tokenizer::TokenKind;

#[cfg(feature = "ts")]
use super::expression::{PExpression, PTsExpression};
use super::{PStatement, ParseResult, Parser};

#[cfg(feature = "ts")]
use super::PTsAtom;

impl<'a> Parser<'a> {
    /// Parse binding type statement - assume the next token is already checked to be
    /// a binding type token
    pub(super) fn parse_binding(
        &mut self,
        binding_type: BindingType,
    ) -> ParseResult<'a, PStatement<'a>> {
        self.tokenizer.next();
        let identifier = self.parse_identifier()?;
        #[cfg(feature = "ts")]
        let ts_type = if self.expect_token(TokenKind::Colon).is_ok() {
            self.parse_ts_expression().ok()
        } else {
            None
        };
        let value = {
            if self.expect_token(TokenKind::Assign).is_ok() {
                Some(self.parse_expression()?)
            } else {
                None
            }
        };
        let statement = PStatement::Binding {
            binding_type,
            identifier,
            #[cfg(feature = "ts")]
            ts_type,
            value,
        };
        Ok(statement)
    }
}

impl TryFrom<TokenKind> for BindingType {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Const => Ok(Self::Const),
            TokenKind::Let => Ok(Self::Let),
            TokenKind::Var => Ok(Self::Var),
            _ => Err(()),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum BindingType {
    Const,
    Let,
    Var,
}

impl Display for BindingType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Const => write!(f, "const"),
            Self::Let => write!(f, "let"),
            Self::Var => write!(f, "var"),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::error::Error;

    use crate::{
        parser::{BindingType, PIdentifier, PStatement, ParseTree, ParseTreeRoot, Parser},
        tokenizer::{Token, TokenKind, TokenLocation, Tokenizer},
    };

    #[cfg(feature = "ts")]
    use crate::parser::{PAtom, PExpression, PLiteralPrimitive};

    use pretty_assertions::assert_eq;

    #[test]
    fn bindings() -> Result<(), Box<dyn Error>> {
        let code = "
let x;
const y;
var z;
";
        let tokenizer = Tokenizer::new(code);
        let parser = Parser::new(tokenizer);
        let (tree, errors) = parser.parse()?;
        assert_eq!(errors, vec![]);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![
                    PStatement::Binding {
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
                        value: None,
                    },
                    PStatement::Binding {
                        binding_type: BindingType::Const,
                        identifier: PIdentifier {
                            token: Token::new(
                                TokenKind::Identifier,
                                TokenLocation { row: 3, column: 7 },
                                "y",
                            ),
                        },
                        #[cfg(feature = "ts")]
                        ts_type: None,
                        value: None,
                    },
                    PStatement::Binding {
                        binding_type: BindingType::Var,
                        identifier: PIdentifier {
                            token: Token::new(
                                TokenKind::Identifier,
                                TokenLocation { row: 4, column: 5 },
                                "z",
                            ),
                        },
                        #[cfg(feature = "ts")]
                        ts_type: None,
                        value: None,
                    },
                ],
            },
        };
        assert_eq!(expected_tree, tree);
        Ok(())
    }

    #[test]
    #[cfg(feature = "ts")]
    fn binding_with_type() -> Result<(), Box<dyn Error>> {
        use crate::parser::{expression::PTsExpression, PJsExpression, PTsAtom};

        let code = "
let x: number = 4;
";
        let tokenizer = Tokenizer::new(code);
        let parser = Parser::new(tokenizer);
        let (tree, errors) = parser.parse()?;
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
                    ts_type: Some(PExpression::Ts(PTsExpression::Atom(PTsAtom::Number(
                        Token::new(
                            TokenKind::Identifier,
                            TokenLocation { row: 2, column: 8 },
                            "number",
                        ),
                    )))),
                    value: Some(PExpression::Js(PJsExpression::Atom(PAtom::Literal(
                        PLiteralPrimitive::Number {
                            value: 4.0,
                            token: Token::new(
                                TokenKind::Literal,
                                TokenLocation { row: 2, column: 17 },
                                "4",
                            ),
                        },
                    )))),
                }],
            },
        };
        assert_eq!(expected_tree, tree);
        Ok(())
    }
}
