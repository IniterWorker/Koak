//!
//! A Token. The lexer's output.
//!

use pos::Pos;

#[derive(Debug, Clone)]
pub enum TokenType {
    Unknown,
    Def,
    Extern,
    Operator(char),
    Identifier(String),
    Number(f64),
}

#[derive(Debug, Clone)]
pub struct Token {
    pub line: String,
    pub start: Pos,
    pub end: Pos,
    pub token_type: TokenType,
}

impl Token {
    #[inline]
    pub fn new() -> Token {
        Token {
            line: String::from(""),
            start: Pos::new(0, 0),
            end: Pos::new(0, 0),
            token_type: TokenType::Unknown,
        }
    }

    #[inline]
    pub fn from(line: &str, start: Pos, end: Pos, t: TokenType) -> Token {
        Token {
            line: line.to_string(),
            start: start,
            end: end,
            token_type: t,
        }
    }
}
