//!
//! Compiler error
//!

use ansi_term::Colour::*;
use pos::Pos;
use token::Token;
use lexer::Lexer;

#[derive(Debug, Clone)]
pub struct SyntaxError {
    line: String,
    start: Pos,
    end: Pos,
    what: ErrorReason,
    file_name: String,
}

#[derive(Debug, Clone)]
pub enum ErrorReason {
    UnknownChar(char),
    InvalidNum(String),
    UnmatchedParenthesis,
    ExprExpected,
    ExpectedFuncName,
    ExpectedOpenParenthesis,
    ArgMustBeIdentifier,
}

impl SyntaxError {
    pub fn from(lexer: &Lexer, token: &Token, what: ErrorReason) -> SyntaxError {
        SyntaxError {
            line: lexer.get_current_line().clone(),
            start: token.get_start(),
            end: token.get_end(),
            what: what,
            file_name: lexer.get_feeder().get_name(),
        }
    }

    pub fn from_pos(lexer: &Lexer, what: ErrorReason) -> SyntaxError {
        SyntaxError {
            line: lexer.get_current_line().clone(),
            start: lexer.get_start_pos(),
            end: lexer.get_current_pos(),
            what: what,
            file_name: lexer.get_feeder().get_name(),
        }
    }

    pub fn from_lexer(lexer: &Lexer, what: ErrorReason) -> SyntaxError {
        SyntaxError {
            line: lexer.get_current_line().clone(),
            start: lexer.get_current_pos(),
            end: lexer.get_current_pos(),
            what: what,
            file_name: lexer.get_feeder().get_name(),
        }
    }

    pub fn print_error(&self) {
        let reason = match self.what {
            ErrorReason::UnknownChar(c) => format!("Unknown char \'{}\'", Purple.bold().paint(format!("{}", c))),
            ErrorReason::InvalidNum(ref s) => format!("Invalid litteral number \"{}\"", Purple.bold().paint(format!("{}", s))),
            ErrorReason::UnmatchedParenthesis => format!("Unmatched parenthesis"),
            ErrorReason::ExprExpected => format!("An expression was expected"),
            ErrorReason::ExpectedFuncName => format!("Function name was expected in prototype"),
            ErrorReason::ExpectedOpenParenthesis => format!("Open parenthesis was expected in prototype"),
            ErrorReason::ArgMustBeIdentifier => format!("Function arguments must be identifiers seperated by spaces"),
        };
        eprintln!("{}:{}:{}: {}: {}", self.file_name, self.start.line, self.start.col, Red.bold().paint("Syntax Error"), reason);
        eprintln!("{}", self.line);
        for _ in 0..self.start.col - 1{
            eprint!(" ");
        }
        for _ in 0..self.end.col - self.start.col {
            eprint!("{}", Green.bold().paint("~"));
        }
        eprintln!("{}", Green.bold().paint("^"));
    }
}

