//!
//! Compiler error
//!

use ansi_term::Colour::*;
use pos::Pos;

use lexer::Token;

#[derive(Debug, Clone)]
pub enum ErrorReason {
    UnknownChar(char),
    InvalidNum(String),
    UnmatchedParenthesis,
    ExprExpected,
    ExpectedFuncName,
    ExpectedOpenParenthesis,
    ArgMustBeIdentifier,
    UndefinedVariable(String),
    UndefinedFunction(String),
    WrongArgNumber(String, usize, usize),
    RedefinedFunc(String),
    RedefinedFuncWithDiffArgs(String),
}

#[derive(Debug, Clone)]
pub struct SyntaxError {
    line: String,
    file_name: String,
    start: Pos,
    end: Pos,
    what: ErrorReason,
}

impl SyntaxError {
    pub fn from(file_name: &str, line: &str, start: Pos, end: Pos, what: ErrorReason) -> SyntaxError {
        SyntaxError {
            line: line.to_string(),
            start: start,
            end: end,
            what: what,
            file_name: file_name.to_string(),
        }
    }

    pub fn from_token(file_name: &str, token: &Token, what: ErrorReason) -> SyntaxError {
        SyntaxError {
            line: token.line.clone(),
            start: token.start,
            end: token.end,
            what: what,
            file_name: file_name.to_string(),
        }
    }

    pub fn print_error(&self) {
        let reason = match self.what {
            ErrorReason::UnknownChar(ref c) => format!("Unknown char \'{}\'", Purple.bold().paint(format!("{}", c))),
            ErrorReason::InvalidNum(ref s) => format!("Invalid litteral number \"{}\"", Purple.bold().paint(s.to_string())),
            ErrorReason::UnmatchedParenthesis => format!("Unmatched parenthesis"),
            ErrorReason::ExprExpected => format!("An expression was expected"),
            ErrorReason::ExpectedFuncName => format!("Function name was expected in a prototype"),
            ErrorReason::ExpectedOpenParenthesis => format!("Open parenthesis was expected after function name in a prototype"),
            ErrorReason::ArgMustBeIdentifier => format!("Function arguments must be identifiers seperated by spaces"),
            ErrorReason::UndefinedVariable(ref s) => format!("Undefined variable \"{}\"", Purple.bold().paint(s.to_string())),
            ErrorReason::UndefinedFunction(ref s) => format!("Undefined function \"{}\"", Purple.bold().paint(s.to_string())),
            ErrorReason::WrongArgNumber(ref name, expected, given) =>
                format!("Wrong number of argument: The function \"{}\" expects {} argument(s), but {} are given.", Purple.bold().paint(name.to_string()), expected, given),
            ErrorReason::RedefinedFunc(ref name) => format!("Redefinition of function \"{}\".", Purple.bold().paint(name.to_string())),
            ErrorReason::RedefinedFuncWithDiffArgs(ref func) => format!("Redefined function \"{}\" with a different number of args.", Purple.bold().paint(func.to_string())),
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

