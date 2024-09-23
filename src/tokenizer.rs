use std::str::CharIndices;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    Assign,
    BraceClose,
    BraceOpen,
    Comma,
    Const,
    Else,
    Function,
    Identifier,
    If,
    Let,
    Literal,
    Minus,
    ParenthesisClose,
    ParenthesisOpen,
    Plus,
    Semicolon,
    StringLiteralStart,
    StringLiteralEnd,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token<'a> {
    token_type: TokenType,
    lexeme: &'a str,
    location: TokenLocation,
}

impl<'a> Token<'a> {
    pub fn new(token_type: TokenType, location: TokenLocation, lexeme: &'a str) -> Self {
        Token {
            token_type,
            lexeme,
            location,
        }
    }
    pub fn token_type(&self) -> &TokenType {
        &self.token_type
    }
    pub fn lexeme(&self) -> &'a str {
        self.lexeme
    }

    pub fn location(&self) -> &TokenLocation {
        &self.location
    }
}

#[derive(Clone, Copy, PartialEq)]
enum Delimeter {
    DoubleQuotes,
    SingleQuote,
}

#[derive(Clone, Copy)]
enum TokenizationMode {
    Normal,
    String { delimeter: Delimeter },
}

pub struct Tokenizer<'a> {
    code: &'a str,
    char_indices: CharIndices<'a>,
    current_line: usize,
    current_line_first_index: usize,
    /// Stack of modes
    /// When the tokenizer is within a string or come out of it, it'll store the mode here
    mode_stack: Vec<TokenizationMode>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            code,
            char_indices: code.char_indices(),
            current_line: 1,
            current_line_first_index: 0,

            mode_stack: vec![],
        }
    }

    fn mode(&self) -> TokenizationMode {
        *self.mode_stack.last().unwrap_or(&TokenizationMode::Normal)
    }

    fn set_mode(&mut self, mode: TokenizationMode) {
        self.mode_stack.push(mode);
    }

    fn pop_mode(&mut self) -> Option<TokenizationMode> {
        self.mode_stack.pop()
    }

    fn consume_while<C>(&mut self, condition: C) -> Option<&'a str>
    where
        C: Fn(char) -> bool,
    {
        if let Some((first_index, last_index, it)) =
            Tokenizer::consume_while_it(&self.char_indices, condition)
        {
            self.char_indices = it;
            return Some(&self.code[first_index..last_index + 1]);
        }
        None
    }

    /// @returns Option<(first, last, iterator)>
    /// [first, last] overlaps the consumed
    /// if None is returned that means nothing was matched
    fn consume_while_it<C>(
        it: &CharIndices<'a>,
        condition: C,
    ) -> Option<(usize, usize, CharIndices<'a>)>
    where
        C: Fn(char) -> bool,
    {
        let mut it_clone = it.clone();

        let (first_index, first_char) = it_clone.next()?;
        if !condition(first_char) {
            return None;
        }

        let mut last_index = first_index;
        loop {
            let mut peeker = it_clone.clone();
            if let Some((i, c)) = peeker.next() {
                if !condition(c) {
                    break;
                }
                last_index = i;
                it_clone = peeker;
            } else {
                break;
            }
        }

        Some((first_index, last_index, it_clone))
    }

    fn consume_whitespace(&mut self) -> Option<&'a str> {
        if let Some(whitespace) = self.consume_while(|c| c.is_whitespace()) {
            if whitespace.contains("\n") {
                let n_nl = whitespace.chars().filter(|c| *c == '\n').count();
                self.current_line += n_nl;
                let n_chars_after_nl = whitespace.len() - whitespace.rfind("\n").unwrap() - 1;
                let mut it_clone = self.char_indices.clone();

                if let Some((i, _)) = it_clone.next() {
                    self.current_line_first_index = i - n_chars_after_nl;
                }
            }
            Some(whitespace)
        } else {
            None
        }
    }

    /// create a token use index [first, last) in code for lexeme
    fn match_token(
        &mut self,
        it: CharIndices<'a>,
        token_type: TokenType,
        first: usize,
        last: usize,
    ) -> Token<'a> {
        self.char_indices = it;
        let lexeme = &self.code[first..last + 1];
        Token::new(
            token_type,
            TokenLocation {
                row: self.current_line,
                column: first - self.current_line_first_index + 1,
            },
            lexeme,
        )
    }

    fn try_consume_keyword(&mut self) -> Option<Token<'a>> {
        let mut it_clone = self.char_indices.clone();
        match it_clone.next() {
            Some((first, 'c')) => {
                self.consume_rest_keyword(it_clone, first, "onst", TokenType::Const)
            }
            Some((first, 'e')) => {
                self.consume_rest_keyword(it_clone, first, "lse", TokenType::Else)
            }
            Some((first, 'f')) => {
                self.consume_rest_keyword(it_clone, first, "unction", TokenType::Function)
            }
            Some((first, 'i')) => self.consume_rest_keyword(it_clone, first, "f", TokenType::If),
            Some((first, 'l')) => match it_clone.next() {
                Some((_, 'e')) => match it_clone.next() {
                    Some((last, 't')) => {
                        return Some(self.match_token(it_clone, TokenType::Let, first, last));
                    }
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        }
    }

    fn consume_rest_keyword<'b>(
        &mut self,
        mut it: CharIndices<'a>,
        first_index: usize,
        rest: &'b str,
        token_type: TokenType,
    ) -> Option<Token<'a>> {
        let mut last_index = first_index;
        for c in rest.chars() {
            let (i, actual_c) = it.next()?;
            if actual_c != c {
                return None;
            }
            last_index = i;
        }

        Some(self.match_token(it, token_type, first_index, last_index))
    }

    fn try_consume_operator(&mut self) -> Option<Token<'a>> {
        let mut it_clone = self.char_indices.clone();
        match it_clone.next() {
            Some((first, '+')) => Some(self.match_token(it_clone, TokenType::Plus, first, first)),
            Some((first, '-')) => Some(self.match_token(it_clone, TokenType::Minus, first, first)),
            Some((first, '=')) => Some(self.match_token(it_clone, TokenType::Assign, first, first)),
            Some((first, '{')) => {
                Some(self.match_token(it_clone, TokenType::BraceOpen, first, first))
            }
            Some((first, '}')) => {
                Some(self.match_token(it_clone, TokenType::BraceClose, first, first))
            }
            Some((first, '(')) => {
                Some(self.match_token(it_clone, TokenType::ParenthesisOpen, first, first))
            }
            Some((first, ')')) => {
                Some(self.match_token(it_clone, TokenType::ParenthesisClose, first, first))
            }
            Some((first, ',')) => Some(self.match_token(it_clone, TokenType::Comma, first, first)),
            Some((first, ';')) => {
                Some(self.match_token(it_clone, TokenType::Semicolon, first, first))
            }
            _ => None,
        }
    }

    fn try_consume_identifier(&mut self) -> Option<Token<'a>> {
        let mut it_clone = self.char_indices.clone();
        match it_clone.next() {
            Some((first, c)) if c.is_alphabetic() => {
                if let Some((_, last_index, it)) =
                    Tokenizer::consume_while_it(&it_clone, |c| c.is_alphanumeric())
                {
                    return Some(self.match_token(it, TokenType::Identifier, first, last_index));
                }
                return Some(self.match_token(it_clone, TokenType::Identifier, first, first));
            }
            _ => None,
        }
    }

    fn try_consume_literal(&mut self) -> Option<Token<'a>> {
        let mut it_clone = self.char_indices.clone();
        match it_clone.next() {
            Some((_, c)) if c.is_numeric() => {
                let (first_index, last_index, it) =
                    Tokenizer::consume_while_it(&self.char_indices, |c| c.is_numeric()).unwrap();
                Some(self.match_token(it, TokenType::Literal, first_index, last_index))
            }
            Some((first_index, c)) if is_quote(c) => {
                let delimeter = quote_as_delimeter(c).expect("already checked above");
                self.char_indices = it_clone;
                self.set_mode(TokenizationMode::String { delimeter });
                Some(self.match_token(
                    self.char_indices.clone(),
                    TokenType::StringLiteralStart,
                    first_index,
                    first_index,
                ))
            }
            _ => None,
        }
    }

    fn consume_string(&mut self, delimeter: Delimeter) -> Option<Token<'a>> {
        if let Some((first_index, last_index, it)) =
            Tokenizer::consume_while_it(&self.char_indices, |c| !is_quote(c))
        {
            Some(self.match_token(it, TokenType::Literal, first_index, last_index))
        } else {
            let mut it_clone = self.char_indices.clone();
            if let Some((i, next_char)) = it_clone.next() {
                if let Some(expected_delimeter) = quote_as_delimeter(next_char) {
                    if expected_delimeter == delimeter {
                        let token = self.match_token(it_clone, TokenType::StringLiteralEnd, i, i);
                        self.pop_mode();
                        return Some(token);
                    }
                }
            }
            None
        }
    }
}

fn is_quote(c: char) -> bool {
    c == '\'' || c == '"'
}

fn quote_as_delimeter(c: char) -> Option<Delimeter> {
    match c {
        '\'' => Some(Delimeter::SingleQuote),
        '"' => Some(Delimeter::DoubleQuotes),
        _ => None,
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.mode() {
            TokenizationMode::Normal => {
                self.consume_whitespace();
                self.try_consume_keyword()
                    .or_else(|| self.try_consume_operator())
                    .or_else(|| self.try_consume_identifier())
                    .or_else(|| self.try_consume_literal())
            }
            TokenizationMode::String { delimeter } => self.consume_string(delimeter),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TokenLocation {
    pub row: usize,
    pub column: usize,
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::Tokenizer;
    use pretty_assertions::assert_eq;

    use super::{Token, TokenLocation, TokenType};
    #[test]
    fn it_should_tokenize() {
        let code = "
let first = 30;
let second = 40;
let third;
third = first + second;
        ";

        let tokens = vec![
            (TokenType::Let, "let", TokenLocation { row: 2, column: 1 }),
            (
                TokenType::Identifier,
                "first",
                TokenLocation { row: 2, column: 5 },
            ),
            (TokenType::Assign, "=", TokenLocation { row: 2, column: 11 }),
            (
                TokenType::Literal,
                "30",
                TokenLocation { row: 2, column: 13 },
            ),
            (
                TokenType::Semicolon,
                ";",
                TokenLocation { row: 2, column: 15 },
            ),
            (TokenType::Let, "let", TokenLocation { row: 3, column: 1 }),
            (
                TokenType::Identifier,
                "second",
                TokenLocation { row: 3, column: 5 },
            ),
            (TokenType::Assign, "=", TokenLocation { row: 3, column: 12 }),
            (
                TokenType::Literal,
                "40",
                TokenLocation { row: 3, column: 14 },
            ),
            (
                TokenType::Semicolon,
                ";",
                TokenLocation { row: 3, column: 16 },
            ),
            (TokenType::Let, "let", TokenLocation { row: 4, column: 1 }),
            (
                TokenType::Identifier,
                "third",
                TokenLocation { row: 4, column: 5 },
            ),
            (
                TokenType::Semicolon,
                ";",
                TokenLocation { row: 4, column: 10 },
            ),
            (
                TokenType::Identifier,
                "third",
                TokenLocation { row: 5, column: 1 },
            ),
            (TokenType::Assign, "=", TokenLocation { row: 5, column: 7 }),
            (
                TokenType::Identifier,
                "first",
                TokenLocation { row: 5, column: 9 },
            ),
            (TokenType::Plus, "+", TokenLocation { row: 5, column: 15 }),
            (
                TokenType::Identifier,
                "second",
                TokenLocation { row: 5, column: 17 },
            ),
            (
                TokenType::Semicolon,
                ";",
                TokenLocation { row: 5, column: 23 },
            ),
        ];

        let mut tokenizer = Tokenizer::new(code);

        for (expected_token, lexeme, location) in tokens {
            let actual_token = tokenizer.next().unwrap();

            assert_eq!(actual_token.token_type(), &expected_token);
            assert_eq!(actual_token.lexeme(), lexeme);
            assert_eq!(actual_token.location(), &location);
        }
    }

    #[test]
    fn it_should_tokenize_string_literal() {
        let code = "'1'";
        let mut tokenizer = Tokenizer::new(code);
        let expected_tokens = vec![
            Token::new(
                TokenType::StringLiteralStart,
                TokenLocation { row: 1, column: 1 },
                "'",
            ),
            Token::new(TokenType::Literal, TokenLocation { row: 1, column: 2 }, "1"),
            Token::new(
                TokenType::StringLiteralEnd,
                TokenLocation { row: 1, column: 3 },
                "'",
            ),
        ];
        for expected in expected_tokens {
            assert_eq!(expected, tokenizer.next().unwrap());
        }
        let code = "\"1 abcd let const lets go\"";
        let mut tokenizer = Tokenizer::new(code);
        let expected_tokens = vec![
            Token::new(
                TokenType::StringLiteralStart,
                TokenLocation { row: 1, column: 1 },
                "\"",
            ),
            Token::new(
                TokenType::Literal,
                TokenLocation { row: 1, column: 2 },
                "1 abcd let const lets go",
            ),
            Token::new(
                TokenType::StringLiteralEnd,
                TokenLocation { row: 1, column: 26 },
                "\"",
            ),
        ];
        for expected in expected_tokens {
            assert_eq!(expected, tokenizer.next().unwrap());
        }
    }

    #[test]
    fn it_should_tokenize_empty_string_literal() {
        let code = "''";
        let mut tokenizer = Tokenizer::new(code);
        let expected_tokens = vec![
            Token::new(
                TokenType::StringLiteralStart,
                TokenLocation { row: 1, column: 1 },
                "'",
            ),
            Token::new(
                TokenType::StringLiteralEnd,
                TokenLocation { row: 1, column: 2 },
                "'",
            ),
        ];
        for token in expected_tokens {
            assert_eq!(token, tokenizer.next().unwrap());
        }
    }

    #[test]
    fn it_should_tokenize_number_literal() {
        let code = "1";
        let mut tokenizer = Tokenizer::new(code);
        let next = tokenizer.next().expect("Literal exists");
        let expected = Token::new(TokenType::Literal, TokenLocation { row: 1, column: 1 }, "1");

        assert_eq!(expected, next);
    }

    #[test]
    fn minus() {
        let mut tokenizer = Tokenizer::new("-1");
        let output = tokenizer.next().expect("should exis");
        let expected = Token::new(TokenType::Minus, TokenLocation { row: 1, column: 1 }, "-");
        assert_eq!(output, expected);
        let output = tokenizer.next().expect("should exis");
        let expected = Token::new(TokenType::Literal, TokenLocation { row: 1, column: 2 }, "1");
        assert_eq!(output, expected);
    }

    #[test]
    fn it_should_tokenize_identifier() {
        let code = "ident";
        let mut tokenizer = Tokenizer::new(code);
        let next = tokenizer.next().expect("Identifier exists");
        let expected = Token::new(
            TokenType::Identifier,
            TokenLocation { row: 1, column: 1 },
            "ident",
        );
        assert_eq!(expected, next);
    }

    #[test]
    fn it_should_tokenize_literal_with_ws() {
        let code = "

   1";

        let mut tokenizer = Tokenizer::new(code);
        let next = tokenizer.next().expect("Literal exists");
        let expected = Token::new(TokenType::Literal, TokenLocation { row: 3, column: 4 }, "1");

        assert_eq!(expected, next);
    }

    #[test]
    fn it_should_tokenize_with_ws() {
        let code = "
   let first = 30;   
let second = 40;
      
    let third;
    third = first + second;
        ";

        let tokens = vec![
            (TokenType::Let, "let", TokenLocation { row: 2, column: 4 }),
            (
                TokenType::Identifier,
                "first",
                TokenLocation { row: 2, column: 8 },
            ),
            (TokenType::Assign, "=", TokenLocation { row: 2, column: 14 }),
            (
                TokenType::Literal,
                "30",
                TokenLocation { row: 2, column: 16 },
            ),
            (
                TokenType::Semicolon,
                ";",
                TokenLocation { row: 2, column: 18 },
            ),
            (TokenType::Let, "let", TokenLocation { row: 3, column: 1 }),
            (
                TokenType::Identifier,
                "second",
                TokenLocation { row: 3, column: 5 },
            ),
            (TokenType::Assign, "=", TokenLocation { row: 3, column: 12 }),
            (
                TokenType::Literal,
                "40",
                TokenLocation { row: 3, column: 14 },
            ),
            (
                TokenType::Semicolon,
                ";",
                TokenLocation { row: 3, column: 16 },
            ),
            (TokenType::Let, "let", TokenLocation { row: 5, column: 5 }),
            (
                TokenType::Identifier,
                "third",
                TokenLocation { row: 5, column: 9 },
            ),
            (
                TokenType::Semicolon,
                ";",
                TokenLocation { row: 5, column: 14 },
            ),
            (
                TokenType::Identifier,
                "third",
                TokenLocation { row: 6, column: 5 },
            ),
            (TokenType::Assign, "=", TokenLocation { row: 6, column: 11 }),
            (
                TokenType::Identifier,
                "first",
                TokenLocation { row: 6, column: 13 },
            ),
            (TokenType::Plus, "+", TokenLocation { row: 6, column: 19 }),
            (
                TokenType::Identifier,
                "second",
                TokenLocation { row: 6, column: 21 },
            ),
            (
                TokenType::Semicolon,
                ";",
                TokenLocation { row: 6, column: 27 },
            ),
        ];

        let mut tokenizer = Tokenizer::new(code);

        for (expected_token, lexeme, location) in tokens {
            let actual_token = tokenizer.next().unwrap();

            assert_eq!(actual_token.token_type(), &expected_token);
            assert_eq!(actual_token.lexeme(), lexeme);
            assert_eq!(actual_token.location(), &location);
        }
    }

    #[test]
    fn block() {
        let code = "
        {
x + y;
        }
x
        ";
        let tokenizer = Tokenizer::new(code);
        let expected_tokens = vec![
            Token::new(
                TokenType::BraceOpen,
                TokenLocation { row: 2, column: 9 },
                "{",
            ),
            Token::new(
                TokenType::Identifier,
                TokenLocation { row: 3, column: 1 },
                "x",
            ),
            Token::new(TokenType::Plus, TokenLocation { row: 3, column: 3 }, "+"),
            Token::new(
                TokenType::Identifier,
                TokenLocation { row: 3, column: 5 },
                "y",
            ),
            Token::new(
                TokenType::Semicolon,
                TokenLocation { row: 3, column: 6 },
                ";",
            ),
            Token::new(
                TokenType::BraceClose,
                TokenLocation { row: 4, column: 9 },
                "}",
            ),
            Token::new(
                TokenType::Identifier,
                TokenLocation { row: 5, column: 1 },
                "x",
            ),
        ];
        assert_eq!(expected_tokens, tokenizer.collect::<Vec<_>>());
    }

    #[test]
    fn operators() {
        let code = "+ - ; ,";
        let tokenizer = Tokenizer::new(code);
        let expected_tokens = vec![
            Token::new(TokenType::Plus, TokenLocation { row: 1, column: 1 }, "+"),
            Token::new(TokenType::Minus, TokenLocation { row: 1, column: 3 }, "-"),
            Token::new(
                TokenType::Semicolon,
                TokenLocation { row: 1, column: 5 },
                ";",
            ),
            Token::new(TokenType::Comma, TokenLocation { row: 1, column: 7 }, ","),
        ];
        assert_eq!(expected_tokens, tokenizer.collect::<Vec<_>>())
    }

    #[test]
    fn keywords() {
        let code = "let function const if else";
        let tokenizer = Tokenizer::new(code);
        let expected_tokens = vec![
            Token::new(TokenType::Let, TokenLocation { row: 1, column: 1 }, "let"),
            Token::new(
                TokenType::Function,
                TokenLocation { row: 1, column: 5 },
                "function",
            ),
            Token::new(
                TokenType::Const,
                TokenLocation { row: 1, column: 14 },
                "const",
            ),
            Token::new(TokenType::If, TokenLocation { row: 1, column: 20 }, "if"),
            Token::new(
                TokenType::Else,
                TokenLocation { row: 1, column: 23 },
                "else",
            ),
        ];
        let actual_tokens = tokenizer.collect::<Vec<_>>();
        assert_eq!(actual_tokens, expected_tokens);
    }
}
