use std::{fmt::Display, str::CharIndices};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenKind {
    Assign,
    BraceClose,
    BraceOpen,
    Colon,
    Comma,
    Const,
    Decrement,
    /// ... operator
    Destructure,
    Else,
    Equals,
    Function,
    Identifier,
    If,
    Increment,
    Let,
    Literal,
    Minus,
    Exclamation,
    NotEquals,
    /// A marker for something that's not a token - it isn't tokenized through the file but can be
    /// used as a placeholder where a token is expected but there isn't one
    #[allow(dead_code)]
    None,
    ParenthesisClose,
    ParenthesisOpen,
    QuestionMark,
    Plus,
    Semicolon,
    Slash,
    Star,
    StringLiteralEnd,
    StringLiteralStart,
    SquareBracketClose,
    SquareBracketOpen,
    /// Marker for an unknown token - It's expected for the parser to report unexpected token error
    Unknown,
    While,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token<'a> {
    #[allow(clippy::struct_field_names)]
    token_type: TokenKind,
    lexeme: &'a str,
    location: TokenLocation,
}

impl<'a> Token<'a> {
    pub const fn new(token_type: TokenKind, location: TokenLocation, lexeme: &'a str) -> Self {
        Token {
            token_type,
            lexeme,
            location,
        }
    }
    pub const fn token_type(&self) -> &TokenKind {
        &self.token_type
    }
    pub const fn lexeme(&self) -> &'a str {
        self.lexeme
    }

    pub const fn location(&self) -> &TokenLocation {
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
            return Some(&self.code[first_index..=last_index]);
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
        if let Some(whitespace) = self.consume_while(char::is_whitespace) {
            if whitespace.contains('\n') {
                let n_nl = whitespace.chars().filter(|c| *c == '\n').count();
                self.current_line += n_nl;
                let n_chars_after_nl = whitespace.len() - whitespace.rfind('\n').unwrap() - 1;
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
        token_type: TokenKind,
        first: usize,
        last: usize,
    ) -> Token<'a> {
        self.char_indices = it;
        let lexeme = &self.code[first..=last];
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
                self.merge_rest_in_token(it_clone, first, "onst", TokenKind::Const)
            }
            Some((first, 'e')) => self.merge_rest_in_token(it_clone, first, "lse", TokenKind::Else),
            Some((first, 'f')) => {
                self.merge_rest_in_token(it_clone, first, "unction", TokenKind::Function)
            }
            Some((first, 'i')) => self.merge_rest_in_token(it_clone, first, "f", TokenKind::If),
            Some((first, 'l')) => match it_clone.next() {
                Some((_, 'e')) => match it_clone.next() {
                    Some((last, 't')) => {
                        return Some(self.match_token(it_clone, TokenKind::Let, first, last));
                    }
                    _ => None,
                },
                _ => None,
            },
            Some((first, 'w')) => {
                self.merge_rest_in_token(it_clone, first, "hile", TokenKind::While)
            }
            _ => None,
        }
    }

    fn merge_rest_in_token<'b>(
        &mut self,
        mut it: CharIndices<'a>,
        first_index: usize,
        rest: &'b str,
        token_type: TokenKind,
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
            Some((first, '+')) => self
                .merge_rest_in_token(it_clone.clone(), first, "+", TokenKind::Increment)
                .or_else(|| Some(self.match_token(it_clone, TokenKind::Plus, first, first))),
            Some((first, '*')) => Some(self.match_token(it_clone, TokenKind::Star, first, first)),
            Some((first, '/')) => Some(self.match_token(it_clone, TokenKind::Slash, first, first)),
            Some((first, '-')) => self
                .merge_rest_in_token(it_clone.clone(), first, "-", TokenKind::Decrement)
                .or_else(|| Some(self.match_token(it_clone, TokenKind::Minus, first, first))),
            Some((first, '=')) => self
                .merge_rest_in_token(it_clone.clone(), first, "=", TokenKind::Equals)
                .or_else(|| Some(self.match_token(it_clone, TokenKind::Assign, first, first))),
            Some((first, '!')) => self
                .merge_rest_in_token(it_clone.clone(), first, "=", TokenKind::NotEquals)
                .or_else(|| Some(self.match_token(it_clone, TokenKind::Exclamation, first, first))),
            Some((first, '{')) => {
                Some(self.match_token(it_clone, TokenKind::BraceOpen, first, first))
            }
            Some((first, '}')) => {
                Some(self.match_token(it_clone, TokenKind::BraceClose, first, first))
            }
            Some((first, '(')) => {
                Some(self.match_token(it_clone, TokenKind::ParenthesisOpen, first, first))
            }
            Some((first, ')')) => {
                Some(self.match_token(it_clone, TokenKind::ParenthesisClose, first, first))
            }
            Some((first, '[')) => {
                Some(self.match_token(it_clone, TokenKind::SquareBracketOpen, first, first))
            }
            Some((first, ']')) => {
                Some(self.match_token(it_clone, TokenKind::SquareBracketClose, first, first))
            }
            Some((first, ',')) => Some(self.match_token(it_clone, TokenKind::Comma, first, first)),
            Some((first, ';')) => {
                Some(self.match_token(it_clone, TokenKind::Semicolon, first, first))
            }
            Some((first, ':')) => Some(self.match_token(it_clone, TokenKind::Colon, first, first)),
            Some((first, '?')) => {
                Some(self.match_token(it_clone, TokenKind::QuestionMark, first, first))
            }
            Some((first, '.')) => {
                self.merge_rest_in_token(it_clone, first, "..", TokenKind::Destructure)
            }
            _ => None,
        }
    }

    fn try_consume_identifier(&mut self) -> Option<Token<'a>> {
        let mut it_clone = self.char_indices.clone();
        match it_clone.next() {
            Some((first, c)) if c.is_alphabetic() => {
                if let Some((_, last_index, it)) =
                    Tokenizer::consume_while_it(&it_clone, char::is_alphanumeric)
                {
                    return Some(self.match_token(it, TokenKind::Identifier, first, last_index));
                }
                return Some(self.match_token(it_clone, TokenKind::Identifier, first, first));
            }
            _ => None,
        }
    }

    fn try_consume_unknown(&mut self) -> Option<Token<'a>> {
        let mut it_clone = self.char_indices.clone();
        it_clone
            .next()
            .map(|(i, _c)| self.match_token(it_clone, TokenKind::Unknown, i, i))
    }

    fn try_consume_literal(&mut self) -> Option<Token<'a>> {
        let mut it_clone = self.char_indices.clone();
        match it_clone.next() {
            Some((_, c)) if c.is_numeric() => {
                let (first_index, last_index, it) =
                    Tokenizer::consume_while_it(&self.char_indices, char::is_numeric).unwrap();
                Some(self.match_token(it, TokenKind::Literal, first_index, last_index))
            }
            Some((first_index, c)) if is_quote(c) => {
                let delimeter = quote_as_delimeter(c).expect("already checked above");
                self.char_indices = it_clone;
                self.set_mode(TokenizationMode::String { delimeter });
                Some(self.match_token(
                    self.char_indices.clone(),
                    TokenKind::StringLiteralStart,
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
            Some(self.match_token(it, TokenKind::Literal, first_index, last_index))
        } else {
            let mut it_clone = self.char_indices.clone();
            if let Some((i, next_char)) = it_clone.next() {
                if let Some(expected_delimeter) = quote_as_delimeter(next_char) {
                    if expected_delimeter == delimeter {
                        let token = self.match_token(it_clone, TokenKind::StringLiteralEnd, i, i);
                        self.pop_mode();
                        return Some(token);
                    }
                }
            }
            None
        }
    }
}

const fn is_quote(c: char) -> bool {
    c == '\'' || c == '"'
}

const fn quote_as_delimeter(c: char) -> Option<Delimeter> {
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
                    .or_else(|| self.try_consume_literal())
                    .or_else(|| self.try_consume_identifier())
                    .or_else(|| self.try_consume_unknown())
            }
            TokenizationMode::String { delimeter } => self.consume_string(delimeter),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TokenLocation {
    pub row: usize,
    pub column: usize,
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.token_type())
    }
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::Tokenizer;
    use pretty_assertions::assert_eq;

    use super::{Token, TokenKind, TokenLocation};
    #[test]
    fn it_should_tokenize() {
        let code = "
let first = 30;
let second = 40;
let third;
third = first + second;
        ";

        let tokens = vec![
            (TokenKind::Let, "let", TokenLocation { row: 2, column: 1 }),
            (
                TokenKind::Identifier,
                "first",
                TokenLocation { row: 2, column: 5 },
            ),
            (TokenKind::Assign, "=", TokenLocation { row: 2, column: 11 }),
            (
                TokenKind::Literal,
                "30",
                TokenLocation { row: 2, column: 13 },
            ),
            (
                TokenKind::Semicolon,
                ";",
                TokenLocation { row: 2, column: 15 },
            ),
            (TokenKind::Let, "let", TokenLocation { row: 3, column: 1 }),
            (
                TokenKind::Identifier,
                "second",
                TokenLocation { row: 3, column: 5 },
            ),
            (TokenKind::Assign, "=", TokenLocation { row: 3, column: 12 }),
            (
                TokenKind::Literal,
                "40",
                TokenLocation { row: 3, column: 14 },
            ),
            (
                TokenKind::Semicolon,
                ";",
                TokenLocation { row: 3, column: 16 },
            ),
            (TokenKind::Let, "let", TokenLocation { row: 4, column: 1 }),
            (
                TokenKind::Identifier,
                "third",
                TokenLocation { row: 4, column: 5 },
            ),
            (
                TokenKind::Semicolon,
                ";",
                TokenLocation { row: 4, column: 10 },
            ),
            (
                TokenKind::Identifier,
                "third",
                TokenLocation { row: 5, column: 1 },
            ),
            (TokenKind::Assign, "=", TokenLocation { row: 5, column: 7 }),
            (
                TokenKind::Identifier,
                "first",
                TokenLocation { row: 5, column: 9 },
            ),
            (TokenKind::Plus, "+", TokenLocation { row: 5, column: 15 }),
            (
                TokenKind::Identifier,
                "second",
                TokenLocation { row: 5, column: 17 },
            ),
            (
                TokenKind::Semicolon,
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
                TokenKind::StringLiteralStart,
                TokenLocation { row: 1, column: 1 },
                "'",
            ),
            Token::new(TokenKind::Literal, TokenLocation { row: 1, column: 2 }, "1"),
            Token::new(
                TokenKind::StringLiteralEnd,
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
                TokenKind::StringLiteralStart,
                TokenLocation { row: 1, column: 1 },
                "\"",
            ),
            Token::new(
                TokenKind::Literal,
                TokenLocation { row: 1, column: 2 },
                "1 abcd let const lets go",
            ),
            Token::new(
                TokenKind::StringLiteralEnd,
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
                TokenKind::StringLiteralStart,
                TokenLocation { row: 1, column: 1 },
                "'",
            ),
            Token::new(
                TokenKind::StringLiteralEnd,
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
        let expected = Token::new(TokenKind::Literal, TokenLocation { row: 1, column: 1 }, "1");

        assert_eq!(expected, next);
    }

    #[test]
    fn minus() {
        let mut tokenizer = Tokenizer::new("-1");
        let output = tokenizer.next().expect("should exis");
        let expected = Token::new(TokenKind::Minus, TokenLocation { row: 1, column: 1 }, "-");
        assert_eq!(output, expected);
        let output = tokenizer.next().expect("should exis");
        let expected = Token::new(TokenKind::Literal, TokenLocation { row: 1, column: 2 }, "1");
        assert_eq!(output, expected);
    }

    #[test]
    fn it_should_tokenize_identifier() {
        let code = "ident";
        let mut tokenizer = Tokenizer::new(code);
        let next = tokenizer.next().expect("Identifier exists");
        let expected = Token::new(
            TokenKind::Identifier,
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
        let expected = Token::new(TokenKind::Literal, TokenLocation { row: 3, column: 4 }, "1");

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
            (TokenKind::Let, "let", TokenLocation { row: 2, column: 4 }),
            (
                TokenKind::Identifier,
                "first",
                TokenLocation { row: 2, column: 8 },
            ),
            (TokenKind::Assign, "=", TokenLocation { row: 2, column: 14 }),
            (
                TokenKind::Literal,
                "30",
                TokenLocation { row: 2, column: 16 },
            ),
            (
                TokenKind::Semicolon,
                ";",
                TokenLocation { row: 2, column: 18 },
            ),
            (TokenKind::Let, "let", TokenLocation { row: 3, column: 1 }),
            (
                TokenKind::Identifier,
                "second",
                TokenLocation { row: 3, column: 5 },
            ),
            (TokenKind::Assign, "=", TokenLocation { row: 3, column: 12 }),
            (
                TokenKind::Literal,
                "40",
                TokenLocation { row: 3, column: 14 },
            ),
            (
                TokenKind::Semicolon,
                ";",
                TokenLocation { row: 3, column: 16 },
            ),
            (TokenKind::Let, "let", TokenLocation { row: 5, column: 5 }),
            (
                TokenKind::Identifier,
                "third",
                TokenLocation { row: 5, column: 9 },
            ),
            (
                TokenKind::Semicolon,
                ";",
                TokenLocation { row: 5, column: 14 },
            ),
            (
                TokenKind::Identifier,
                "third",
                TokenLocation { row: 6, column: 5 },
            ),
            (TokenKind::Assign, "=", TokenLocation { row: 6, column: 11 }),
            (
                TokenKind::Identifier,
                "first",
                TokenLocation { row: 6, column: 13 },
            ),
            (TokenKind::Plus, "+", TokenLocation { row: 6, column: 19 }),
            (
                TokenKind::Identifier,
                "second",
                TokenLocation { row: 6, column: 21 },
            ),
            (
                TokenKind::Semicolon,
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
                TokenKind::BraceOpen,
                TokenLocation { row: 2, column: 9 },
                "{",
            ),
            Token::new(
                TokenKind::Identifier,
                TokenLocation { row: 3, column: 1 },
                "x",
            ),
            Token::new(TokenKind::Plus, TokenLocation { row: 3, column: 3 }, "+"),
            Token::new(
                TokenKind::Identifier,
                TokenLocation { row: 3, column: 5 },
                "y",
            ),
            Token::new(
                TokenKind::Semicolon,
                TokenLocation { row: 3, column: 6 },
                ";",
            ),
            Token::new(
                TokenKind::BraceClose,
                TokenLocation { row: 4, column: 9 },
                "}",
            ),
            Token::new(
                TokenKind::Identifier,
                TokenLocation { row: 5, column: 1 },
                "x",
            ),
        ];
        assert_eq!(expected_tokens, tokenizer.collect::<Vec<_>>());
    }

    #[test]
    fn operators() {
        let code = "+ - * / ; , : ... [ ] ++ -- ? == != !";
        let tokenizer = Tokenizer::new(code);
        let expected = vec![
            "Plus",
            "Minus",
            "Star",
            "Slash",
            "Semicolon",
            "Comma",
            "Colon",
            "Destructure",
            "SquareBracketOpen",
            "SquareBracketClose",
            "Increment",
            "Decrement",
            "QuestionMark",
            "Equals",
            "NotEquals",
            "Exclamation",
        ];
        let actual = tokenizer.map(|t| t.to_string()).collect::<Vec<_>>();
        assert_eq!(expected, actual);
    }

    #[test]
    fn keywords() {
        let code = "let function const if else while";
        let tokenizer = Tokenizer::new(code);
        let expected_tokens = vec![
            Token::new(TokenKind::Let, TokenLocation { row: 1, column: 1 }, "let"),
            Token::new(
                TokenKind::Function,
                TokenLocation { row: 1, column: 5 },
                "function",
            ),
            Token::new(
                TokenKind::Const,
                TokenLocation { row: 1, column: 14 },
                "const",
            ),
            Token::new(TokenKind::If, TokenLocation { row: 1, column: 20 }, "if"),
            Token::new(
                TokenKind::Else,
                TokenLocation { row: 1, column: 23 },
                "else",
            ),
            Token::new(
                TokenKind::While,
                TokenLocation { row: 1, column: 28 },
                "while",
            ),
        ];
        let actual_tokens = tokenizer.collect::<Vec<_>>();
        assert_eq!(actual_tokens, expected_tokens);
    }
}
