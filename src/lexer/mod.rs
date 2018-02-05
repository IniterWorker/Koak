//!
//! Koak's lexer
//!

mod token;

pub use self::token::{Token, TokenType};
use input::{InputFeeder, InputFeederIterator};
use pos::Pos;
use syntaxerror::{SyntaxError, ErrorReason};

#[derive(Debug)]
pub struct Lexer<'a, T: 'a + InputFeeder, I: InputFeederIterator> {
    feeder: &'a mut T,
    input: I,
    start_pos: Pos,
}

impl<'a, T: InputFeeder, I: InputFeederIterator> Lexer<'a, T, I> {
    #[inline]
    pub fn from(feeder: &'a mut T, it: I) -> Lexer<'a, T, I> {
        Lexer {
            feeder: feeder,
            input: it,
            start_pos: Pos::new(0, 0),
        }
    }

    #[inline]
    fn new_token(&self, t: TokenType) -> Token {
        Token::from(
            self.feeder.get_line().clone(),
            self.start_pos,
            Pos::new(self.input.get_row(), self.input.get_col()),
            t
        )
    }

    #[inline]
    fn new_syntaxerror(&self, e: ErrorReason) -> SyntaxError {
        SyntaxError::from(
            self.feeder.get_name(),
            self.feeder.get_line(),
            self.start_pos,
            Pos::new(self.input.get_row(), self.input.get_col()),
            e
        )
    }

    fn lex_comment(&mut self) {
        loop {
            match self.input.peek().unwrap_or('\0') {
                '\n' | '\0' => break,
                _ => { self.input.next(); },
            }
        }
    }

    fn lex_number(&mut self, c: char) -> Result<Token, SyntaxError> {
        let mut s = c.to_string();

        while let Some(c) = self.input.peek() {
            match c {
                '0'...'9' | '.' => {
                    s.push(self.input.next().unwrap());
                },
                _ => break,
            }
        }

        match s.parse::<f64>().ok() {
            Some(f) => Ok(self.new_token(TokenType::Number(f))),
            _ => Err(self.new_syntaxerror(ErrorReason::InvalidNum(s))),
        }
    }

    fn lex_identifier(&mut self, c: char) -> Result<Token, SyntaxError> {
        let mut s = c.to_string();

        while let Some(c) = self.input.peek() {
            match c {
                '0'...'9' | 'a'...'z' | 'A'...'Z' | '_' => {
                    s.push(self.input.next().unwrap());
                }
                _ => break,
            }
        }
        match s.as_ref() {
            "def" => Ok(self.new_token(TokenType::Def)),
            "extern" => Ok(self.new_token(TokenType::Extern)),
            _ => Ok(self.new_token(TokenType::Identifier(s))),
        }
    }
}

impl<'a, T: InputFeeder, I: InputFeederIterator> Iterator for Lexer<'a, T, I> {
    type Item = Result<Token, SyntaxError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.input.next() {
            Some(c) => {
                self.start_pos = Pos::new(self.input.get_row(), self.input.get_col());
                match c {
                    ' ' | '\r' | '\t' | '\n' => self.next(),
                    'a'...'z' | 'A'...'Z' | '_' => Some(self.lex_identifier(c)),
                    '0'...'9' | '.' => Some(self.lex_number(c)),
                    '#' => { self.lex_comment(); self.next() },
                    '+' | '-' | '*' | '/' | '%' | '>' | '<' | '=' | '!' | '(' | ')' | ',' => {
                        Some(Ok(self.new_token(TokenType::Operator(c))))
                    },
                    _ => Some(Err(self.new_syntaxerror(ErrorReason::UnknownChar(c))))
                }
            },
            _ => None,
        }
    }
}
