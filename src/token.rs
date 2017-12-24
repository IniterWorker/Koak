//!
//! A Token. The lexer's output.
//!

use lexer::Lexer;
use pos::Pos;

#[derive(Debug, Clone)]
pub enum TokenType {
    Def,
    Extern,
    Operator(char),
    Identifier(String),
    Number(f64),
    Error(String),
}

#[derive(Debug)]
pub struct Token {
    start: Pos,
    end: Pos,
    token_type: TokenType,
}

impl Token {
    pub fn from(lexer: &Lexer, t: TokenType) -> Token {
        Token {
            start: lexer.get_start_pos(),
            end: lexer.get_current_pos(),
            token_type: t,
        }
    }

    pub fn get_start(&self) -> Pos {
        self.start
    }

    pub fn get_end(&self) -> Pos {
        self.end
    }

    pub fn get_type(&self) -> TokenType {
        self.token_type.clone()
    }
}

