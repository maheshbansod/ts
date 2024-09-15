use crate::tokenizer::{Token, Tokenizer};

pub struct Parser {}

impl Parser {
    pub fn new(tokenizer: Tokenizer) -> Self {
        todo!()
    }

    pub fn parse<'a>(mut self) -> ParseTree<'a> {
        todo!()
    }
}

#[derive(Debug, PartialEq)]
struct ParseTree<'a> {
    root: ParseTreeRoot<'a>,
}

#[derive(Debug, PartialEq)]
struct ParseTreeRoot<'a> {
    statements: Vec<PStatement<'a>>,
}

#[derive(Debug, PartialEq)]
enum PStatement<'a> {
    Binding {
        binding_type: BindingType,
        identifier: PIdentifier<'a>,
        value: Option<PExpression<'a>>,
    },
}

#[derive(Debug, PartialEq)]
enum PExpression<'a> {
    Literal(PLiteral<'a>),
}

#[derive(Debug, PartialEq)]
enum PLiteral<'a> {
    Number {
        value: f32, // to check what value we should keep
        token: Token<'a>,
    },
}

#[derive(Debug, PartialEq)]
struct PIdentifier<'a> {
    token: Token<'a>,
}

#[derive(Debug, PartialEq)]
enum BindingType {
    Let,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::{Token, TokenType, Tokenizer};

    #[test]
    fn it_should_parse() {
        let code = "
let x = 30;
let y = 100;
let z = x + y;
        ";
        let tokenizer = Tokenizer::new(code);
        let parser = Parser::new(tokenizer);
        let tree = parser.parse();

        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::Binding {
                    binding_type: BindingType::Let,
                    identifier: PIdentifier {
                        token: Token::new(TokenType::Identifier, "x"),
                    },
                    value: Some(PExpression::Literal(PLiteral::Number {
                        value: 30.0,
                        token: Token::new(TokenType::Literal, "30"),
                    })),
                }],
            },
            // WIP
        };

        assert_eq!(tree, expected_tree);
    }
}
