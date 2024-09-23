use std::{error::Error, fmt::Display, iter::Peekable};

use crate::tokenizer::{Token, TokenType, Tokenizer};

pub struct Parser<'a> {
    tokenizer: Peekable<Tokenizer<'a>>,
}

type ParseResult<'a, T> = Result<T, ParserError<'a>>;

impl<'a> Parser<'a> {
    pub fn new(tokenizer: Tokenizer<'a>) -> Self {
        Self {
            tokenizer: tokenizer.peekable(),
        }
    }

    pub fn parse(mut self) -> ParseResult<'a, (ParseTree<'a>, Vec<ParserError<'a>>)> {
        let (root, errors) = self.parse_root()?;
        Ok((ParseTree { root }, errors))
    }

    fn parse_root(&mut self) -> ParseResult<'a, (ParseTreeRoot<'a>, Vec<ParserError<'a>>)> {
        let (statements, errors) = self.parse_block_statements(false)?;
        Ok((ParseTreeRoot { statements }, errors))
    }

    /// Parses statements in a block and consumes the tokens including the closing brace
    /// It has no knowledge about opening brace so the caller should consume it
    fn parse_block_statements(
        &mut self,
        is_block: bool,
    ) -> ParseResult<'a, (Vec<PStatement<'a>>, Vec<ParserError<'a>>)> {
        let mut statements = vec![];
        let mut errors = vec![];
        loop {
            if is_block
                && self
                    .tokenizer
                    .peek()
                    .ok_or(ParserError::UnexpectedEof)?
                    .token_type()
                    == &TokenType::BraceClose
            {
                self.tokenizer.next();
                break;
            }
            match self.parse_statement() {
                Ok((statement, mut statement_errors)) => {
                    statements.push(statement);
                    errors.append(&mut statement_errors);
                }
                Err(ParserError::UnexpectedEof) => {
                    if statements.is_empty() {
                        errors.push(ParserError::UnexpectedEof);
                    }
                    break;
                }
                Err(e) => errors.push(e),
            }
        }
        Ok((statements, errors))
    }

    fn parse_statement(&mut self) -> ParseResult<'a, (PStatement<'a>, Vec<ParserError<'a>>)> {
        let (statement, errors) = if let Some(next_token) = self.tokenizer.peek() {
            if let Ok(binding_type) = next_token.token_type().clone().try_into() {
                (self.parse_binding(binding_type)?, vec![])
            } else if next_token.token_type() == &TokenType::BraceOpen {
                // we're in a block hmmm
                self.tokenizer.next(); // consume brace
                let (statements, errors) = self.parse_block_statements(true)?;
                (PStatement::Block { statements }, errors)
            } else if next_token.token_type() == &TokenType::If {
                self.tokenizer.next();
                let (statement, errors) = self.parse_if_else()?;
                (PStatement::If { statement }, errors)
            } else {
                // okay let's maybe parse expression directly here
                let expression = self.parse_expression()?;
                (PStatement::Expression { expression }, vec![])
            }
        } else {
            return Err(ParserError::UnexpectedEof);
        };
        self.optional_semicolon();
        Ok((statement, errors))
    }

    /// Parse binding type statement - assume the next token is already checked to be
    /// a binding type token
    fn parse_binding(&mut self, binding_type: BindingType) -> ParseResult<'a, PStatement<'a>> {
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

    fn parse_block_or_one(
        &mut self,
    ) -> ParseResult<'a, (Vec<PStatement<'a>>, Vec<ParserError<'a>>)> {
        if self.expect_token(TokenType::BraceOpen).is_ok() {
            // let's parse the block
            Ok(self.parse_block_statements(true)?)
        } else {
            let (statement, errors) = self.parse_statement()?;
            Ok((vec![statement], errors))
        }
    }

    fn parse_if(&mut self) -> ParseResult<'a, (PIfStatement<'a>, Vec<ParserError<'a>>)> {
        let mut errors = vec![];
        self.expect_token(TokenType::ParenthesisOpen)?;
        let condition = self.parse_expression()?;
        self.expect_token(TokenType::ParenthesisClose)?;
        let (body, mut body_errors) = self.parse_block_or_one()?;
        errors.append(&mut body_errors);
        let statement = PIfStatement { condition, body };
        Ok((statement, errors))
    }

    /// Assuming the IF token is already parsed here
    fn parse_if_else(&mut self) -> ParseResult<'a, (PIfElseStatement<'a>, Vec<ParserError<'a>>)> {
        let (if_statement, mut errors) = self.parse_if()?;
        let mut else_if_statements = vec![];
        let mut else_body = None;

        while self.expect_token(TokenType::Else).is_ok() {
            if self.expect_token(TokenType::If).is_ok() {
                let (if_statement, mut else_if_errors) = self.parse_if()?;
                else_if_statements.push(if_statement);
                errors.append(&mut else_if_errors);
            } else {
                let (body, mut body_errors) = self.parse_block_or_one()?;
                errors.append(&mut body_errors);
                else_body = Some(body);
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

    fn parse_identifier(&mut self) -> ParseResult<'a, PIdentifier<'a>> {
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

    fn parse_expression(&mut self) -> ParseResult<'a, PExpression<'a>> {
        self.parse_expression_pratt(0)
    }

    fn parse_expression_pratt(
        &mut self,
        min_binding_power: u8,
    ) -> ParseResult<'a, PExpression<'a>> {
        let token = self.tokenizer.peek().ok_or(ParserError::UnexpectedEof)?;
        let mut lhs = if is_token_prefix_operator(token.token_type()) {
            let token_type = token.token_type();
            if let Some(((), bp)) = prefix_binding_power(token_type) {
                let token = self.tokenizer.next().unwrap();
                let operator = self.token_as_operator(token).unwrap();
                let rhs = self.parse_expression_pratt(bp)?;
                PExpression::Cons(operator, vec![rhs])
            } else {
                let token = self.tokenizer.next().unwrap();
                return Err(ParserError::UnexpectedToken(token));
            }
        } else if let (Some(atom), _errors) = {
            let token = token.clone();
            self.try_parse_atom(token)
        }? {
            PExpression::Atom(atom)
        } else {
            let token = self.tokenizer.next().unwrap();
            return Err(ParserError::UnexpectedToken(token));
        };

        loop {
            let op_token = self.tokenizer.peek();
            if op_token.is_none() {
                break;
            }
            let op_token = op_token.unwrap();

            if let Some((l_bp, r_bp)) = infix_binding_power(op_token.token_type()) {
                if l_bp < min_binding_power {
                    break;
                }
                let token = self.tokenizer.next().expect("Already peeked ");
                let operator = self
                    .token_as_operator(token)
                    .expect("Already peeked and checked");
                let rhs = self.parse_expression_pratt(r_bp)?;
                lhs = PExpression::Cons(operator, vec![lhs, rhs]);

                continue;
            }
            break;
        }
        Ok(lhs)
    }

    fn try_parse_atom(
        &mut self,
        first_token: Token<'a>,
    ) -> ParseResult<'a, (Option<PAtom<'a>>, Vec<ParserError<'a>>)> {
        let token = first_token;
        match token.token_type() {
            TokenType::Literal => {
                self.tokenizer.next();
                if let Ok(n) = token.lexeme().parse::<f32>() {
                    Ok((
                        Some(PAtom::Literal(PLiteral::Number { value: n, token })),
                        vec![],
                    ))
                } else {
                    Err(ParserError::UnexpectedToken(token))
                }
            }
            TokenType::StringLiteralStart => {
                self.tokenizer.next();
                let start_token = token;
                if let Some(next) = self.tokenizer.next() {
                    let (value, value_token, end_token) = match next.token_type() {
                        TokenType::Literal => Ok((next.lexeme(), Some(next), None)),
                        TokenType::StringLiteralEnd => Ok(("", None, Some(next))),
                        _ => Err(ParserError::UnexpectedToken(next)),
                    }?;

                    let end_token = if end_token.is_none() {
                        if let Some(next) = self.tokenizer.next() {
                            if next.token_type() == &TokenType::StringLiteralEnd {
                                Ok(next)
                            } else {
                                Err(ParserError::UnexpectedToken(next))
                            }
                        } else {
                            Err(ParserError::UnexpectedEof)
                        }
                    } else {
                        Ok(end_token.unwrap())
                    }?;

                    Ok((
                        Some(PAtom::Literal(PLiteral::String {
                            start_delim: start_token,
                            end_delim: end_token,
                            value,
                            value_token,
                        })),
                        vec![],
                    ))
                } else {
                    Err(ParserError::UnexpectedEof)
                }
            }
            TokenType::Function => {
                self.tokenizer.next();
                // todo - we need to make identifier compulsary for function statements i think.
                let identifier = self.parse_identifier().ok();
                self.expect_token(TokenType::ParenthesisOpen)?;
                let mut args = vec![];
                loop {
                    match self.parse_identifier() {
                        Ok(identifier) => {
                            args.push(identifier);
                        }
                        Err(_e) => {
                            self.expect_token(TokenType::ParenthesisClose)?;
                            break;
                        }
                    }
                    let token = self
                        .expect_token(TokenType::Comma)
                        .or_else(|_| self.expect_token(TokenType::ParenthesisClose))?;
                    if token.token_type() == &TokenType::ParenthesisClose {
                        break;
                    }
                }
                self.expect_token(TokenType::BraceOpen)?;
                let (statements, errors) = self.parse_block_statements(true)?;
                Ok((
                    Some(PAtom::Function(PFunction {
                        identifier,
                        arguments: args,
                        body: statements,
                    })),
                    errors,
                ))
            }
            TokenType::Identifier => {
                self.tokenizer.next();
                Ok((Some(PAtom::Identifier(PIdentifier { token })), vec![]))
            }
            _ => Ok((None, vec![])),
        }
    }

    /// Check if a token is there and consume
    fn expect_token(&mut self, token_type: TokenType) -> ParseResult<'a, Token<'a>> {
        if let Some(token) = self.tokenizer.peek() {
            if token.token_type() != &token_type {
                Err(ParserError::ExpectedToken {
                    expected: token_type,
                    got: token.clone(),
                })
            } else {
                let token = self.tokenizer.next().expect("We peeked in the above if");
                Ok(token)
            }
        } else {
            Err(ParserError::UnexpectedEof)
        }
    }

    /// See if there's a semicolon next if so consume it
    fn optional_semicolon(&mut self) {
        let next_token = self.tokenizer.peek();
        if let Some(next_token) = next_token {
            if next_token.token_type() == &TokenType::Semicolon {
                let _ = self.tokenizer.next();
            }
        }
    }

    fn token_as_operator(&self, token: Token<'a>) -> ParseResult<'a, POperator<'a>> {
        // if matches!(token.token_type(), TokenType::Plus) {
        //     Ok(POperator::BinaryAdd(token))
        // } else {
        //     Err(ParserError::UnexpectedToken(token))
        // }
        match token.token_type() {
            TokenType::Plus => Ok(POperator::BinaryAdd(token)),
            TokenType::Minus => Ok(POperator::Minus(token)),
            _ => Err(ParserError::UnexpectedToken(token)),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ParseTree<'a> {
    root: ParseTreeRoot<'a>,
}

#[derive(Debug, PartialEq)]
struct ParseTreeRoot<'a> {
    statements: Vec<PStatement<'a>>,
}

#[derive(Debug, PartialEq)]
enum POperator<'a> {
    BinaryAdd(Token<'a>),
    Minus(Token<'a>),
}

#[derive(Debug, PartialEq)]
enum PStatement<'a> {
    Binding {
        binding_type: BindingType,
        identifier: PIdentifier<'a>,
        value: Option<PExpression<'a>>,
    },
    Block {
        statements: Vec<PStatement<'a>>,
    },
    Expression {
        expression: PExpression<'a>,
    },
    If {
        statement: PIfElseStatement<'a>,
    },
}

#[derive(Debug, PartialEq)]
enum PExpression<'a> {
    Atom(PAtom<'a>),
    Cons(POperator<'a>, Vec<PExpression<'a>>),
}

#[derive(Debug, PartialEq)]
enum PAtom<'a> {
    Literal(PLiteral<'a>),
    Identifier(PIdentifier<'a>),
    Function(PFunction<'a>),
}

#[derive(Debug, PartialEq)]
enum PLiteral<'a> {
    Number {
        value: f32, // to check what value we should keep
        token: Token<'a>,
    },
    String {
        value_token: Option<Token<'a>>,
        value: &'a str,
        start_delim: Token<'a>,
        end_delim: Token<'a>,
    },
}

#[derive(Debug, PartialEq)]
struct PIdentifier<'a> {
    token: Token<'a>,
}

#[derive(Debug, PartialEq)]
struct PIfStatement<'a> {
    condition: PExpression<'a>,
    body: Vec<PStatement<'a>>,
}

#[derive(Debug, PartialEq)]
struct PIfElseStatement<'a> {
    if_statement: PIfStatement<'a>,
    /// intermediate else ifs in sequence
    else_if_statements: Vec<PIfStatement<'a>>,
    /// final else
    else_body: Option<Vec<PStatement<'a>>>,
}

#[derive(Debug, PartialEq)]
struct PFunction<'a> {
    identifier: Option<PIdentifier<'a>>,
    /// Args - WIP
    arguments: Vec<PIdentifier<'a>>,
    body: Vec<PStatement<'a>>,
}

#[derive(Debug, PartialEq)]
enum BindingType {
    Let,
    Const,
}

#[derive(Debug, PartialEq)]
pub enum ParserError<'a> {
    UnexpectedEof,
    UnexpectedToken(Token<'a>),
    ExpectedToken { expected: TokenType, got: Token<'a> },
}

impl Error for ParserError<'_> {}

impl<'a> Display for PExpression<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PExpression::Atom(atom) => match atom {
                PAtom::Literal(literal) => write!(f, "{}", literal),
                PAtom::Identifier(identifier) => write!(f, "{}", identifier),
                PAtom::Function(function) => write!(f, "{}", function),
            },
            PExpression::Cons(operator, rest) => {
                write!(f, "{} (", operator)?;
                for expr in rest {
                    write!(f, "{} ", expr)?;
                }
                write!(f, ")")
            }
        }
    }
}

impl<'a> Display for PLiteral<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PLiteral::Number { value, .. } => write!(f, "{}", value),
            PLiteral::String { value, .. } => write!(f, "str({})", value),
        }
    }
}

impl<'a> Display for PIdentifier<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.token.lexeme())
    }
}

impl<'a> Display for PFunction<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let fname = if let Some(i) = &self.identifier {
            i.token.lexeme()
        } else {
            "<anon>"
        };
        write!(f, "function({})", fname)?;
        for statement in &self.body {
            write!(f, "{statement}")?;
        }
        write!(f, "functionend({})", fname)?;
        Ok(())
    }
}

impl<'a> Display for PStatement<'a> {
    fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl<'a> Display for POperator<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            POperator::BinaryAdd(_) => write!(f, "+"),
            POperator::Minus(_) => write!(f, "-"),
        }
    }
}

impl<'a> Display for ParserError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::UnexpectedToken(token) => {
                let location = token.location();
                write!(
                    f,
                    "Unexpected token {:?} at line {}, column {}",
                    token.lexeme(),
                    location.row,
                    location.column
                )
            }
            ParserError::UnexpectedEof => write!(f, "Unexpected end of file"),
            // todo: remove Expected token maybe or maybe have what is the current token too
            ParserError::ExpectedToken { expected, got } => {
                write!(f, "Expected token type {expected:?}, got {got:?}")
            }
        }
    }
}

impl TryFrom<TokenType> for BindingType {
    type Error = ();

    fn try_from(value: TokenType) -> Result<Self, Self::Error> {
        match value {
            TokenType::Const => Ok(BindingType::Const),
            TokenType::Let => Ok(BindingType::Let),
            _ => Err(()),
        }
    }
}

fn infix_binding_power(token_type: &TokenType) -> Option<(u8, u8)> {
    match token_type {
        TokenType::Plus => Some((1, 2)),
        _ => None,
    }
}
fn prefix_binding_power(token_type: &TokenType) -> Option<((), u8)> {
    match token_type {
        TokenType::Minus => Some(((), 6)),
        TokenType::Plus => Some(((), 4)),
        _ => None,
    }
}

fn is_token_prefix_operator(token_type: &TokenType) -> bool {
    matches!(token_type, TokenType::Plus | TokenType::Minus)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::{Token, TokenLocation, TokenType, Tokenizer};
    use pretty_assertions::assert_eq;

    #[test]
    fn it_should_parse() {
        let code = "
let x = 30;
let y = 100;
let z = x + y;
        ";
        let tokenizer = Tokenizer::new(code);
        let parser = Parser::new(tokenizer);
        let (tree, errors) = parser.parse().expect("parsing failure");

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
                        value: Some(PExpression::Atom(PAtom::Literal(PLiteral::Number {
                            value: 30.0,
                            token: Token::new(
                                TokenType::Literal,
                                TokenLocation { row: 2, column: 9 },
                                "30",
                            ),
                        }))),
                    },
                    PStatement::Binding {
                        binding_type: BindingType::Let,
                        identifier: PIdentifier {
                            token: Token::new(
                                TokenType::Identifier,
                                TokenLocation { row: 3, column: 5 },
                                "y",
                            ),
                        },
                        value: Some(PExpression::Atom(PAtom::Literal(PLiteral::Number {
                            value: 100.0,
                            token: Token::new(
                                TokenType::Literal,
                                TokenLocation { row: 3, column: 9 },
                                "100",
                            ),
                        }))),
                    },
                    PStatement::Binding {
                        binding_type: BindingType::Let,
                        identifier: PIdentifier {
                            token: Token::new(
                                TokenType::Identifier,
                                TokenLocation { row: 4, column: 5 },
                                "z",
                            ),
                        },
                        value: Some(PExpression::Cons(
                            POperator::BinaryAdd(Token::new(
                                TokenType::Plus,
                                TokenLocation { row: 4, column: 11 },
                                "+",
                            )),
                            vec![
                                PExpression::Atom(PAtom::Identifier(PIdentifier {
                                    token: Token::new(
                                        TokenType::Identifier,
                                        TokenLocation { row: 4, column: 9 },
                                        "x",
                                    ),
                                })),
                                PExpression::Atom(PAtom::Identifier(PIdentifier {
                                    token: Token::new(
                                        TokenType::Identifier,
                                        TokenLocation { row: 4, column: 13 },
                                        "y",
                                    ),
                                })),
                            ],
                        )),
                    },
                ],
            },
        };

        assert_eq!(errors, vec![]);
        assert_eq!(tree, expected_tree);
    }

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

    #[test]
    fn it_should_parse_exp_atom() {
        let code = "1";
        let tokenizer = Tokenizer::new(code);
        let mut parser = Parser::new(tokenizer);
        assert_eq!(
            parser.parse_expression().expect("should parse"),
            PExpression::Atom(PAtom::Literal(PLiteral::Number {
                value: 1.0,
                token: Token::new(TokenType::Literal, TokenLocation { row: 1, column: 1 }, "1")
            }))
        );
        let code = "ident";
        let tokenizer = Tokenizer::new(code);
        let mut parser = Parser::new(tokenizer);
        assert_eq!(
            parser.parse_expression().expect("should parse"),
            PExpression::Atom(PAtom::Identifier(PIdentifier {
                token: Token::new(
                    TokenType::Identifier,
                    TokenLocation { row: 1, column: 1 },
                    "ident"
                )
            }))
        );
    }

    #[test]
    fn it_should_parse_string_atom() {
        let code = "'1'";
        let tokenizer = Tokenizer::new(code);
        let mut parser = Parser::new(tokenizer);
        assert_eq!(
            parser.parse_expression().expect("should parse"),
            PExpression::Atom(PAtom::Literal(PLiteral::String {
                value: "1",
                start_delim: Token::new(
                    TokenType::StringLiteralStart,
                    TokenLocation { row: 1, column: 1 },
                    "'"
                ),
                value_token: Some(Token::new(
                    TokenType::Literal,
                    TokenLocation { row: 1, column: 2 },
                    "1"
                )),
                end_delim: Token::new(
                    TokenType::StringLiteralEnd,
                    TokenLocation { row: 1, column: 3 },
                    "'"
                ),
            }))
        );
    }

    #[test]
    fn it_should_parse_empty_string_atom() {
        let code = "''";
        let tokenizer = Tokenizer::new(code);
        let mut parser = Parser::new(tokenizer);
        assert_eq!(
            parser.parse_expression().expect("should parse"),
            PExpression::Atom(PAtom::Literal(PLiteral::String {
                value: "",
                start_delim: Token::new(
                    TokenType::StringLiteralStart,
                    TokenLocation { row: 1, column: 1 },
                    "'"
                ),
                value_token: None,
                end_delim: Token::new(
                    TokenType::StringLiteralEnd,
                    TokenLocation { row: 1, column: 2 },
                    "'"
                ),
            }))
        );
    }

    #[test]
    fn unary_single_exp() {
        let code = "-1";
        let mut parser = Parser::new(Tokenizer::new(code));
        let tree = parser.parse_expression().expect("it should parse");

        assert_eq!(tree.to_string(), "- (1 )")
    }

    #[test]
    fn test_exp() {
        let code = "4 + 3 + -2";
        let mut parser = Parser::new(Tokenizer::new(code));
        let tree = parser.parse_expression().expect("it should parse");

        assert_eq!(tree.to_string(), "+ (+ (4 3 ) - (2 ) )")
    }

    #[test]
    fn it_should_parse_add_exp() {
        let code = "4 + 3";
        let tokenizer = Tokenizer::new(code);
        let mut parser = Parser::new(tokenizer);
        assert_eq!(
            parser.parse_expression().expect("should parse"),
            PExpression::Cons(
                POperator::BinaryAdd(Token::new(
                    TokenType::Plus,
                    TokenLocation { row: 1, column: 3 },
                    "+"
                )),
                vec![
                    PExpression::Atom(PAtom::Literal(PLiteral::Number {
                        value: 4.0,
                        token: Token::new(
                            TokenType::Literal,
                            TokenLocation { row: 1, column: 1 },
                            "4"
                        )
                    })),
                    PExpression::Atom(PAtom::Literal(PLiteral::Number {
                        value: 3.0,
                        token: Token::new(
                            TokenType::Literal,
                            TokenLocation { row: 1, column: 5 },
                            "3"
                        )
                    }))
                ]
            )
        );
        let code = "a + b";
        let tokenizer = Tokenizer::new(code);
        let mut parser = Parser::new(tokenizer);
        assert_eq!(
            parser.parse_expression().expect("should parse"),
            PExpression::Cons(
                POperator::BinaryAdd(Token::new(
                    TokenType::Plus,
                    TokenLocation { row: 1, column: 3 },
                    "+"
                )),
                vec![
                    PExpression::Atom(PAtom::Identifier(PIdentifier {
                        token: Token::new(
                            TokenType::Identifier,
                            TokenLocation { row: 1, column: 1 },
                            "a"
                        )
                    })),
                    PExpression::Atom(PAtom::Identifier(PIdentifier {
                        token: Token::new(
                            TokenType::Identifier,
                            TokenLocation { row: 1, column: 5 },
                            "b"
                        )
                    })),
                ]
            )
        );
    }

    #[test]
    fn expression_statement() {
        let code = "y;";
        let tokenizer = Tokenizer::new(code);
        let parser = Parser::new(tokenizer);
        let (tree, error) = parser.parse().expect("should parse");
        assert_eq!(error, vec![]);
        let expected_tree = ParseTree {
            root: ParseTreeRoot {
                statements: vec![PStatement::Expression {
                    expression: PExpression::Atom(PAtom::Identifier(PIdentifier {
                        token: Token::new(
                            TokenType::Identifier,
                            TokenLocation { row: 1, column: 1 },
                            "y",
                        ),
                    })),
                }],
            },
        };
        assert_eq!(expected_tree, tree);
    }

    #[test]
    fn block() {
        let code = "
let x = 1;
{
    let y = 2;
    x + y;
}
y;
        ";
        let tokenizer = Tokenizer::new(code);
        let parser = Parser::new(tokenizer);
        let (tree, errors) = parser.parse().expect("should parse");
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
                        value: Some(PExpression::Atom(PAtom::Literal(PLiteral::Number {
                            value: 1.0,
                            token: Token::new(
                                TokenType::Literal,
                                TokenLocation { row: 2, column: 9 },
                                "1",
                            ),
                        }))),
                    },
                    PStatement::Block {
                        statements: vec![
                            PStatement::Binding {
                                binding_type: BindingType::Let,
                                identifier: PIdentifier {
                                    token: Token::new(
                                        TokenType::Identifier,
                                        TokenLocation { row: 4, column: 9 },
                                        "y",
                                    ),
                                },
                                value: Some(PExpression::Atom(PAtom::Literal(PLiteral::Number {
                                    value: 2.0,
                                    token: Token::new(
                                        TokenType::Literal,
                                        TokenLocation { row: 4, column: 13 },
                                        "2",
                                    ),
                                }))),
                            },
                            PStatement::Expression {
                                expression: PExpression::Cons(
                                    POperator::BinaryAdd(Token::new(
                                        TokenType::Plus,
                                        TokenLocation { row: 5, column: 7 },
                                        "+",
                                    )),
                                    vec![
                                        PExpression::Atom(PAtom::Identifier(PIdentifier {
                                            token: Token::new(
                                                TokenType::Identifier,
                                                TokenLocation { row: 5, column: 5 },
                                                "x",
                                            ),
                                        })),
                                        PExpression::Atom(PAtom::Identifier(PIdentifier {
                                            token: Token::new(
                                                TokenType::Identifier,
                                                TokenLocation { row: 5, column: 9 },
                                                "y",
                                            ),
                                        })),
                                    ],
                                ),
                            },
                        ],
                    },
                    PStatement::Expression {
                        expression: PExpression::Atom(PAtom::Identifier(PIdentifier {
                            token: Token::new(
                                TokenType::Identifier,
                                TokenLocation { row: 7, column: 1 },
                                "y",
                            ),
                        })),
                    },
                ],
            },
        };
        assert_eq!(expected_tree, tree);
    }

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
                    expression: PExpression::Atom(PAtom::Function(PFunction {
                        identifier: Some(PIdentifier {
                            token: Token::new(
                                TokenType::Identifier,
                                TokenLocation { row: 2, column: 18 },
                                "f",
                            ),
                        }),
                        arguments: vec![],
                        body: vec![PStatement::Binding {
                            binding_type: BindingType::Let,
                            identifier: PIdentifier {
                                token: Token::new(
                                    TokenType::Identifier,
                                    TokenLocation { row: 3, column: 5 },
                                    "y",
                                ),
                            },
                            value: Some(PExpression::Cons(
                                POperator::BinaryAdd(Token::new(
                                    TokenType::Plus,
                                    TokenLocation { row: 3, column: 10 },
                                    "+",
                                )),
                                vec![
                                    PExpression::Atom(PAtom::Identifier(PIdentifier {
                                        token: Token::new(
                                            TokenType::Identifier,
                                            TokenLocation { row: 3, column: 9 },
                                            "x",
                                        ),
                                    })),
                                    PExpression::Atom(PAtom::Literal(PLiteral::Number {
                                        value: 1.0,
                                        token: Token::new(
                                            TokenType::Literal,
                                            TokenLocation { row: 3, column: 11 },
                                            "1",
                                        ),
                                    })),
                                ],
                            )),
                        }],
                    })),
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
                            TokenType::Identifier,
                            TokenLocation { row: 2, column: 5 },
                            "x",
                        ),
                    },
                    value: Some(PExpression::Atom(PAtom::Function(PFunction {
                        identifier: None,
                        arguments: vec![],
                        body: vec![],
                    }))),
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
                    expression: PExpression::Atom(PAtom::Function(PFunction {
                        identifier: Some(PIdentifier {
                            token: Token::new(
                                TokenType::Identifier,
                                TokenLocation { row: 2, column: 10 },
                                "foo",
                            ),
                        }),
                        arguments: vec![
                            PIdentifier {
                                token: Token::new(
                                    TokenType::Identifier,
                                    TokenLocation { row: 2, column: 14 },
                                    "arg1",
                                ),
                            },
                            PIdentifier {
                                token: Token::new(
                                    TokenType::Identifier,
                                    TokenLocation { row: 2, column: 20 },
                                    "arg2",
                                ),
                            },
                        ],
                        body: vec![],
                    })),
                }],
            },
        };
        assert_eq!(expected_tree, tree);
    }

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
                            condition: PExpression::Atom(PAtom::Literal(PLiteral::Number {
                                value: 1.0,
                                token: Token::new(
                                    TokenType::Literal,
                                    TokenLocation { row: 2, column: 5 },
                                    "1",
                                ),
                            })),
                            body: vec![PStatement::Expression {
                                expression: PExpression::Atom(PAtom::Identifier(PIdentifier {
                                    token: Token::new(
                                        TokenType::Identifier,
                                        TokenLocation { row: 3, column: 1 },
                                        "x",
                                    ),
                                })),
                            }],
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
                            condition: PExpression::Atom(PAtom::Literal(PLiteral::Number {
                                value: 1.0,
                                token: Token::new(
                                    TokenType::Literal,
                                    TokenLocation { row: 2, column: 5 },
                                    "1",
                                ),
                            })),
                            body: vec![PStatement::Expression {
                                expression: PExpression::Atom(PAtom::Identifier(PIdentifier {
                                    token: Token::new(
                                        TokenType::Identifier,
                                        TokenLocation { row: 3, column: 1 },
                                        "x",
                                    ),
                                })),
                            }],
                        },
                        else_if_statements: vec![PIfStatement {
                            condition: PExpression::Atom(PAtom::Literal(PLiteral::Number {
                                value: 0.0,
                                token: Token::new(
                                    TokenType::Literal,
                                    TokenLocation { row: 4, column: 12 },
                                    "0",
                                ),
                            })),
                            body: vec![],
                        }],
                        else_body: Some(vec![PStatement::Expression {
                            expression: PExpression::Atom(PAtom::Identifier(PIdentifier {
                                token: Token::new(
                                    TokenType::Identifier,
                                    TokenLocation { row: 6, column: 1 },
                                    "y",
                                ),
                            })),
                        }]),
                    },
                }],
            },
        };
        assert_eq!(tree, expected_tree);
    }

    #[test]
    fn error_expected() {
        let code = "
let x = function ( {};
        ";
        let tokenizer = Tokenizer::new(code);
        let parser = Parser::new(tokenizer);
        let (_tree, errors) = parser.parse().expect("should parse");
        assert_eq!(
            errors,
            vec![ParserError::ExpectedToken {
                expected: TokenType::ParenthesisClose,
                got: Token::new(
                    TokenType::BraceOpen,
                    TokenLocation { row: 2, column: 20 },
                    "{"
                )
            }]
        );
        // } - // editor is acting weird and parsing the above opening brace in string as an
        // opening brace.
    }
}
