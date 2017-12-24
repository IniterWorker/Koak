//!
//! Koak's lexer
//!

use token::{Token, TokenType};
use input::InputFeeder;
use pos::Pos;

#[derive(Debug)]
pub struct Lexer {
    feeder: InputFeeder,
    line: String,
    line_idx: usize,
    idx: usize,
    start_pos: Pos,
}

impl Lexer {

    pub fn from(feeder: InputFeeder) -> Lexer {
        Lexer {
            feeder: feeder,
            line: String::new(),
            line_idx: 0,
            idx: 0,
            start_pos: Pos::new(1, 1),
        }
    }

    fn next_line(&mut self) -> bool {
        match self.feeder.next() {
            Some(l) => {
                self.line = l;
                self.line_idx += 1;
                self.idx = 0;
                false
            },
            None => true,
        }
    }

    fn next_char(&mut self) -> Option<char> {
        while self.idx >= self.line.len() {
            if self.next_line() {
                return None;
            }
        }
        self.idx += 1;
        Some(self.line.as_bytes()[self.idx - 1] as char)
    }

    fn peek_char(&self) -> Option<char> {
        if self.idx >= self.line.len() {
            None
        } else {
            Some(self.line.as_bytes()[self.idx] as char)
        }
    }

    fn lex_comment(&mut self) {
        loop {
            match self.peek_char().unwrap_or('\0') {
                '\n' | '\0' => break,
                _ => { self.next_char(); },
            }
        }
    }

    fn lex_number(&mut self, c: char) -> Token {
        let mut s = c.to_string();

        loop {
            match self.peek_char().unwrap_or('\0') {
                '0'...'9' => s.push(self.next_char().unwrap()),
                _ => break,
            }
        }

        match s.parse::<f64>().ok() {
            Some(f) => Token::from(self, TokenType::Number(f)),
            _ => Token::from(self, TokenType::Error(format!("Invalid litteral number \"{}\"", s))),
        }
    }

    fn lex_identifier(&mut self, c: char) -> Token {
        let mut s = c.to_string();

        loop {
            match self.peek_char().unwrap_or('\0') {
                '0'...'9' | 'a'...'z' | 'A'...'Z' => {
                    s.push(self.next_char().unwrap());
                }
                _ => break,
            }
        }
        match s.as_ref() {
            "def" => Token::from(self, TokenType::Def),
            "extern" => Token::from(self, TokenType::Extern),
            _ => Token::from(self, TokenType::Identifier(s)),
        }
    }

    pub fn get_current_line(&self) -> String {
        self.line.clone()
    }

    pub fn get_current_pos(&self) -> Pos {
        Pos::new(self.line_idx, self.idx)
    }

    pub fn get_start_pos(&self) -> Pos {
        self.start_pos
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        match self.next_char() {
            Some(c) => {
                self.start_pos = self.get_current_pos();
                match c {
                    ' ' | '\r' | '\t' | '\n' => self.next(),
                    'a'...'z' | 'A'...'Z' => Some(self.lex_identifier(c)),
                    '0'...'9' => Some(self.lex_number(c)),
                    '#' => { self.lex_comment(); self.next() },
                    '+' | '-' | '*' | '/' | '%' | '>' | '<' | '=' | '!' | '(' | ')' => {
                        Some(Token::from(self, TokenType::Operator(c)))
                    },
                    _ => Some(Token::from(self, TokenType::Error(format!("Unknown character \'{}\'", c)))),
                }
            },
            _ => None,
        }
    }
}
