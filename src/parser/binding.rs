use crate::tokenizer::TokenType;

use super::{BindingType, PStatement, ParseResult, Parser};

impl<'a> Parser<'a> {
    /// Parse binding type statement - assume the next token is already checked to be
    /// a binding type token
    pub(super) fn parse_binding(
        &mut self,
        binding_type: BindingType,
    ) -> ParseResult<'a, PStatement<'a>> {
        self.tokenizer.next();
        let identifier = self.parse_identifier()?;
        let value = {
            if let Ok(_) = self.expect_token(TokenType::Assign) {
                Some(self.parse_expression()?)
            } else {
                None
            }
        };
        let statement = PStatement::Binding {
            binding_type,
            identifier,
            value,
        };
        Ok(statement)
    }
}

#[cfg(test)]
mod tests {
    use std::error::Error;

    use crate::{
        parser::{BindingType, PIdentifier, PStatement, ParseTree, ParseTreeRoot, Parser},
        tokenizer::{Token, TokenLocation, TokenType, Tokenizer},
    };

    #[test]
    fn bindings() -> Result<(), Box<dyn Error>> {
        let code = "
let x;
const y;
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
                                TokenType::Identifier,
                                TokenLocation { row: 2, column: 5 },
                                "x",
                            ),
                        },
                        value: None,
                    },
                    PStatement::Binding {
                        binding_type: BindingType::Const,
                        identifier: PIdentifier {
                            token: Token::new(
                                TokenType::Identifier,
                                TokenLocation { row: 3, column: 7 },
                                "y",
                            ),
                        },
                        value: None,
                    },
                ],
            },
        };
        assert_eq!(expected_tree, tree);
        Ok(())
    }
}
