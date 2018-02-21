//!
//! Tokens created by the Lexer and used by the parser.
//!

use std::rc::Rc;
use std::fmt;

use super::OperatorType;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Def,
    Extern,
    If,
    Then,
    Else,
    For,
    In,
    Operator(OperatorType),
    Identifier(Rc<String>),
    IntegerLiteral(i32),
    DoubleLiteral(f64),
    CharLiteral(i8),
    StringLitteral(String),
    OpenParenthesis,
    CloseParenthesis,
    Comma,
    SemiColon,
    Equal,
    Colon,
    Arrow,
    Void,
    Bool,
    Int,
    Char,
    Double,
    True,
    False,
    Import,

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
