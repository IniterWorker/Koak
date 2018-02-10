//!
//! Koak's error handling
//!

use std::rc::Rc;
use std::fmt;

use ansi_term::Colour::*;

use lexer::Token;

#[allow(dead_code)]
pub enum ErrorReason {
    UnknownChar(char),
    InvalidLitteralNum(String),
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

///
/// Implementation of the Display trait to print each error reasons correctly.
///
impl fmt::Display for ErrorReason {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &ErrorReason::UnknownChar(ref c) =>
                write!(f, "Unknown char \'{}\'", Purple.bold().paint(c.to_string())),
            &ErrorReason::InvalidLitteralNum(ref s) =>
                write!(f, "Invalid litteral number \"{}\"", Purple.bold().paint(s.to_string())),
            &ErrorReason::UnmatchedParenthesis =>
                write!(f, "Unmatched parenthesis"),
            &ErrorReason::ExprExpected =>
                write!(f, "An expression was expected"),
            &ErrorReason::ExpectedFuncName =>
                write!(f, "Function name was expected in a prototype"),
            &ErrorReason::ExpectedOpenParenthesis =>
                write!(f, "Open parenthesis was expected after function name in a prototype"),
            &ErrorReason::ArgMustBeIdentifier =>
                write!(f, "Function arguments must be identifiers seperated by spaces"),
            &ErrorReason::UndefinedVariable(ref s) =>
                write!(f, "Undefined variable \"{}\"", Purple.bold().paint(s.to_string())),
            &ErrorReason::UndefinedFunction(ref s) =>
                write!(f, "Undefined function \"{}\"", Purple.bold().paint(s.to_string())),
            &ErrorReason::WrongArgNumber(ref name, expected, given) =>
                write!(f, "Wrong number of argument: The function \"{}\" expects {} argument(s), but {} are given.", Purple.bold().paint(name.to_string()), expected, given),
            &ErrorReason::RedefinedFunc(ref name) =>
                write!(f, "Redefinition of function \"{}\".", Purple.bold().paint(name.to_string())),
            &ErrorReason::RedefinedFuncWithDiffArgs(ref func) =>
                write!(f, "Function \"{}\" redefined with different arguments.", Purple.bold().paint(func.to_string())),
        }
    }
}

pub struct SyntaxError {
    line: Rc<String>,
    what: ErrorReason,
    row: usize,
    col: (usize, usize),
}

impl SyntaxError {
    #[inline]
    pub fn new(what: ErrorReason, line: Rc<String>, row: usize, col: (usize, usize)) -> SyntaxError {
        SyntaxError {
            line: line,
            what: what,
            row: row,
            col: col,
        }
    }

    #[inline]
    pub fn from(token: &Token, er: ErrorReason) -> SyntaxError {
        SyntaxError::new(
            er,
            token.line.clone(),
            token.row,
            token.col,
        )
    }
}

///
/// Implementation of the Display trait to print syntax errors
///
impl fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        writeln!(f, "{} at line {}, column {}: {}", Red.bold().paint("Syntax Error"), self.row, self.col.0, self.what)?;
        writeln!(f, "{}", self.line)?;

        for (i, c) in self.line.char_indices() {
            if i < self.col.0 - 1 { // Print prefix blank spaces
                if c == '\t' {
                    write!(f, "\t")?;
                }
                else {
                    write!(f, " ")?;
                }
            } else if i < self.col.1 - 1 { // Print '~'
                write!(f, "{}", Green.bold().paint("~"))?;
            } else { // Print '^'
                write!(f, "{}", Green.bold().paint("^"))?;
                break;
            }
        }
        Ok(())
    }
}

pub fn print_errors(errors: &Vec<SyntaxError>) {
    for e in errors {
        eprintln!("{}", e);
    }
}
