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

#[derive(Debug)]
pub struct Token<'a> {
    token_type: TokenType,
    lexeme: &'a str,
}

impl<'a> Token<'a> {
    pub fn new(token_type: TokenType, lexeme: &'a str) -> Self {
        Token { token_type, lexeme }
    }
    pub fn token_type(&self) -> &TokenType {
        &self.token_type
    }
    pub fn lexeme(&self) -> &'a str {
        &self.lexeme
    }
}

pub struct Tokenizer<'a> {
    code: &'a str,
    char_indices: CharIndices<'a>,
}

impl<'a> Tokenizer<'a> {
    pub fn new(code: &'a str) -> Self {
        Self {
            code,
            char_indices: code.char_indices(),
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
        self.consume_while(|c| c.is_whitespace())
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
        Token::new(token_type, lexeme)
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

#[cfg(test)]
mod tests {
    use crate::tokenizer::Tokenizer;

    use super::TokenType;
    #[test]
    fn it_should_tokenize() {
        let code = "
let first = 30;
let second = 40;
let third;
third = first + second;
        ";

        let tokens = vec![
            (TokenType::Let, "let"),
            (TokenType::Identifier, "first"),
            (TokenType::Assign, "="),
            (TokenType::Literal, "30"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Identifier, "second"),
            (TokenType::Assign, "="),
            (TokenType::Literal, "40"),
            (TokenType::Semicolon, ";"),
            (TokenType::Let, "let"),
            (TokenType::Identifier, "third"),
            (TokenType::Semicolon, ";"),
            (TokenType::Identifier, "third"),
            (TokenType::Assign, "="),
            (TokenType::Identifier, "first"),
            (TokenType::Plus, "+"),
            (TokenType::Identifier, "second"),
            (TokenType::Semicolon, ";"),
        ];

        let mut tokenizer = Tokenizer::new(code);

        for (expected_token, lexeme) in tokens {
            let actual_token = tokenizer.next().unwrap();

            assert_eq!(actual_token.token_type(), &expected_token);
            assert_eq!(actual_token.lexeme(), lexeme);
        }
    }
}
