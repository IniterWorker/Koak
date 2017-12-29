//!
//! Koak's lexer
//!

use token::{Token, TokenType};
use input::InputFeeder;
use pos::Pos;
use syntaxerror::{SyntaxError, ErrorReason};

#[derive(Debug)]
pub struct Lexer {
    feeder: Box<InputFeeder>,
    peeked: Option<Option<Result<Token, SyntaxError>>>,
    line: String,
    line_idx: usize,
    idx: usize,
    start_pos: Pos,
}

impl Lexer {

    pub fn from(feeder: Box<InputFeeder>) -> Lexer {
        Lexer {
            feeder: feeder,
            peeked: None,
            line: String::new(),
            line_idx: 0,
            idx: 0,
            start_pos: Pos::new(1, 1),
        }
    }

    fn next_line(&mut self) -> bool {
        match self.feeder.next_line() {
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

    fn lex_number(&mut self, c: char) -> Result<Token, SyntaxError> {
        let mut s = c.to_string();

        while let Some(c) = self.peek_char() {
            match c {
                '0'...'9' | '.' => {
                    s.push(self.next_char().unwrap());
                },
                _ => break,
            }
        }

        match s.parse::<f64>().ok() {
            Some(f) => Ok(Token::from(self, TokenType::Number(f))),
            _ => Err(SyntaxError::from_pos(self, ErrorReason::InvalidNum(s))),
        }
    }

    fn lex_identifier(&mut self, c: char) -> Result<Token, SyntaxError> {
        let mut s = c.to_string();

        while let Some(c) = self.peek_char() {
            match c {
                '0'...'9' | 'a'...'z' | 'A'...'Z' | '_' => {
                    s.push(self.next_char().unwrap());
                }
                _ => break,
            }
        }
        match s.as_ref() {
            "def" => Ok(Token::from(self, TokenType::Def)),
            "extern" => Ok(Token::from(self, TokenType::Extern)),
            _ => Ok(Token::from(self, TokenType::Identifier(s))),
        }
    }

    fn next_token(&mut self) -> Option<Result<Token, SyntaxError>> {
        match self.next_char() {
            Some(c) => {
                self.start_pos = self.get_current_pos();
                match c {
                    ' ' | '\r' | '\t' | '\n' => self.next_token(),
                    'a'...'z' | 'A'...'Z' | '_' => Some(self.lex_identifier(c)),
                    '0'...'9' | '.' => Some(self.lex_number(c)),
                    '#' => { self.lex_comment(); self.next_token() },
                    '+' | '-' | '*' | '/' | '%' | '>' | '<' | '=' | '!' | '(' | ')' | ',' => {
                        Some(Ok(Token::from(self, TokenType::Operator(c))))
                    },
                    _ => Some(Err(SyntaxError::from_pos(self, ErrorReason::UnknownChar(c))))
                }
            },
            _ => None,
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

    pub fn get_feeder(&self) -> &Box<InputFeeder> {
        &self.feeder
    }

    pub fn get_feeder_mut(&mut self) -> &mut Box<InputFeeder> {
        &mut self.feeder
    }

    pub fn reset(&mut self) {
        self.peeked = None;
        self.idx = self.line.len();
    }

    pub fn peek(&mut self) -> Option<Result<Token, SyntaxError>> {
        if self.peeked.is_none() {
            self.peeked = Some(self.next());
        }
        match self.peeked {
            Some(Some(ref value)) => Some(value.clone()),
            _ => None,
        }
    }

    pub fn peek_type(&mut self) -> Option<Result<TokenType, SyntaxError>> {
        self.peek().map(|r| r.map(|t| t.get_type()))
    }

    pub fn next_or(&mut self, er: ErrorReason) -> Result<Token, SyntaxError> {
        self.next().ok_or(SyntaxError::from_lexer(self, er))?
    }
}

impl Iterator for Lexer {
    type Item = Result<Token, SyntaxError>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.peeked.take() {
            Some(v) => v,
            None => self.next_token(),
        }
    }
}
