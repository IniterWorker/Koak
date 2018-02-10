//!
//! Tokens created by the Lexer and used by the parser.
//!

use std::rc::Rc;
use std::fmt;

#[derive(Debug, Clone)]
pub enum TokenType {
    Def,
    Extern,
    Operator(char),
    Identifier(Rc<String>),
    Number(f64),

    Unknown,
}

#[derive(Clone)]
pub struct Token {
    pub token_type: TokenType,
    pub line: Rc<String>,
    pub row: usize,
    pub col: (usize, usize),
}

impl Token {
    pub fn new() -> Token {
        Token {
            token_type: TokenType::Unknown,
            line: Rc::new(String::new()),
            row: 0,
            col: (0, 0),
        }
    }

    pub fn from(tt: TokenType, line: Rc<String>, row: usize, col: (usize, usize)) -> Token {
        Token {
            token_type: tt,
            line: line,
            row: row,
            col: col,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self.token_type)
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{:?}", self.token_type)
    }
}
