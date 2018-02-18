//!
//! Tokens created by the Lexer and used by the parser.
//!

use std::rc::Rc;
use std::fmt;

use llvm_sys::prelude::LLVMTypeRef;

use iron_llvm::core::types::{IntTypeRef, IntTypeCtor, RealTypeRef, RealTypeCtor};
use iron_llvm::LLVMRef;
use error::{SyntaxError, ErrorReason};

use super::OperatorType;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Def,
    Extern,
    If,
    Then,
    Else,
    Operator(OperatorType),
    Identifier(Rc<String>),
    IntegerLitteral(i32),
    DoubleLitteral(f64),
    OpenParenthesis,
    CloseParenthesis,
    Comma,
    SemiColon,
    Colon,
    Arrow,
    Int,
    Double,
    Bool,
    True,
    False,

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

    pub fn as_llvm_type(&self) -> Result<LLVMTypeRef, SyntaxError> {
        match self.token_type {
            TokenType::Bool => Ok(IntTypeRef::get_int1().to_ref()),
            TokenType::Int => Ok(IntTypeRef::get_int32().to_ref()),
            TokenType::Double => Ok(RealTypeRef::get_double().to_ref()),
            _ => Err(SyntaxError::from(self, ErrorReason::InvalidType)),
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
