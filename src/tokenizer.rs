use std::str::CharIndices;

#[derive(Debug, PartialEq)]
pub enum TokenType {
    Assign,
    Literal,
    Identifier,
    Let,
    Plus,
    Semicolon,
}

#[derive(Debug, PartialEq)]
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
        &self.lexeme
    }

    pub fn location(&self) -> &TokenLocation {
        &self.location
    }
}

pub struct Tokenizer<'a> {
    code: &'a str,
    char_indices: CharIndices<'a>,
    current_line: usize,
    last_line_end_index: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            code,
            char_indices: code.char_indices(),
            current_line: 1,
            last_line_end_index: 0,
        }
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
        return None;
    }

    fn consume_while_it<C>(
        it: &CharIndices<'a>,
        condition: C,
    ) -> Option<(usize, usize, CharIndices<'a>)>
    where
        C: Fn(char) -> bool,
    {
        let mut it_clone = it.clone();

        let first = it_clone.next();
        if first.is_none() {
            return None;
        }
        let (first_index, first_char) = first.unwrap();
        if !condition(first_char) {
            return None;
        }

        let mut last_index = first_index;
        loop {
            let mut peeker = it_clone.clone();
            if let Some((i, c)) = peeker.next() {
                last_index = i;
                if !condition(c) {
                    return Some((first_index, last_index - 1, it_clone));
                }
                it_clone = peeker;
            } else {
                break;
            }
        }
        if last_index == first_index {
            return None;
        }

        return Some((first_index, last_index, it_clone));
    }

    fn consume_whitespace(&mut self) -> Option<&'a str> {
        if let Some(whitespace) = self.consume_while(|c| c.is_whitespace()) {
            if whitespace.contains("\n") {
                self.current_line += 1;
                let mut it_clone = self.char_indices.clone();
                if let Some((i, _)) = it_clone.next() {
                    self.last_line_end_index = i - 1;
                }
            }
            Some(whitespace)
        } else {
            None
        }
    }

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
                column: first - self.last_line_end_index,
            },
            lexeme,
        )
    }

    fn try_consume_keyword(&mut self) -> Option<Token<'a>> {
        let mut it_clone = self.char_indices.clone();
        match it_clone.next() {
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

    fn try_consume_operator(&mut self) -> Option<Token<'a>> {
        let mut it_clone = self.char_indices.clone();
        match it_clone.next() {
            Some((first, '+')) => Some(self.match_token(it_clone, TokenType::Plus, first, first)),
            Some((first, '=')) => Some(self.match_token(it_clone, TokenType::Assign, first, first)),
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
        // only number for now
        if let Some((first_index, last_index, it)) =
            Tokenizer::consume_while_it(&self.char_indices, |c| c.is_numeric())
        {
            return Some(self.match_token(it, TokenType::Literal, first_index, last_index));
        }

        return None;
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        self.consume_whitespace();
        return self
            .try_consume_keyword()
            .or_else(|| self.try_consume_operator())
            .or_else(|| self.try_consume_identifier())
            .or_else(|| self.try_consume_literal());
    }
}

#[derive(Debug, PartialEq)]
pub struct TokenLocation {
    pub row: usize,
    pub column: usize,
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::Tokenizer;

    use super::{TokenLocation, TokenType};
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
}
