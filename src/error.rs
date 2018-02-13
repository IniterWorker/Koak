//!
//! Koak's error handling
//!

use std::rc::Rc;
use std::fmt;

use libc;
use ansi_term::Colour::*;

use args::Args;
use lexer::Token;

///
/// Quick macro to enable colors if stderr is a tty
///

macro_rules! color {
    ($color:expr, $( $arg:expr ), *) => {
        if unsafe { libc::isatty(2) } == 1 {
            format!("{}", ($color.bold().paint(format!($( $arg ), * ))))
        } else {
            format!($( $arg ), * )
        }
    };
}

macro_rules! purple {
    ( $var:expr ) => (color!(Purple, "{}", $var));
    ( $( $arg:expr ), * ) => (color!(Purple, $( $arg ),* ));
}

macro_rules! red {
    ( $var:expr ) => (color!(Red, "{}", $var));
    ( $( $arg:expr ), * ) => (color!(Red, $( $arg ),* ));
}

macro_rules! green {
    ( $var:expr ) => (color!(Green, "{}", $var));
    ( $( $arg:expr ), * ) => (color!(Green, $( $arg ),* ));
}

///
/// Enum of all possible errors.
///
#[allow(dead_code)]
#[derive(Debug)]
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
    MissingSemiColonAfterExtern,
    MissingSemiColonAfterDef,
    MissingSemiColonAfterTopLevelExpr,
}

///
/// Implementation of the Display trait to print each error reasons correctly.
///
impl fmt::Display for ErrorReason {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match self {
            &ErrorReason::UnknownChar(ref c) =>
                write!(f, "Unknown char \'{}\'", purple!(c)),
            &ErrorReason::InvalidLitteralNum(ref s) =>
                write!(f, "Invalid litteral number \"{}\"", purple!(s)),
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
                write!(f, "Undefined variable \"{}\"", purple!(s)),
            &ErrorReason::UndefinedFunction(ref s) =>
                write!(f, "Undefined function \"{}\"", purple!(s)),
            &ErrorReason::WrongArgNumber(ref name, expected, given) =>
                write!(f, "Wrong number of argument: The function \"{}\" expects {} argument(s), but {} are given.", purple!(name), expected, given),
            &ErrorReason::RedefinedFunc(ref name) =>
                write!(f, "Redefinition of function \"{}\".", purple!(name)),
            &ErrorReason::RedefinedFuncWithDiffArgs(ref func) =>
                write!(f, "Function \"{}\" redefined with different arguments.", purple!(func.to_string())),
            &ErrorReason::MissingSemiColonAfterExtern =>
                write!(f, "Missing semi-colon after an extern declaration"),
            &ErrorReason::MissingSemiColonAfterDef =>
                write!(f, "Missing semi-colon at the end of a function definition"),
            &ErrorReason::MissingSemiColonAfterTopLevelExpr =>
                write!(f, "Missing semi-colon at the end of a top-level expression"),
        }
    }
}

#[derive(Debug)]
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
/// Structure representing a syntax error that must be printed in it's complete form.
///
pub struct ComplexError<'a>(&'a SyntaxError);

///
/// Structure representing a syntax error that must be printed in a tiny, non-verbose form.
///
pub struct TinyError<'a>(&'a SyntaxError);

///
/// Implementation of the Display trait to print tiny syntax errors
///
impl<'a> fmt::Display for TinyError<'a> {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let se = self.0;
        write!(f, "{} at line {}, column {}: {}", red!("Syntax Error"), se.row, se.col.0, se.what)
    }
}

///
/// Implementation of the Display trait to print complete syntax errors
///
impl<'a> fmt::Display for ComplexError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        let se = self.0;
        let te = TinyError(se);

        writeln!(f, "{}", te)?; // Print tiny error first
        writeln!(f, "{}", se.line)?;

        for (i, c) in se.line.char_indices() {
            if i < se.col.0 - 1 { // Print prefix blank spaces
                if c == '\t' {
                    write!(f, "\t")?;
                }
                else {
                    write!(f, " ")?;
                }
            } else if i < se.col.1 - 1 { // Print '~'
                write!(f, "{}", green!("~"))?;
            } else { // Print '^'
                write!(f, "{}", green!("^"))?;
                break;
            }
        }
        Ok(())
    }
}

pub fn print_errors(args: &Args, errors: &Vec<SyntaxError>) {
    for e in errors {
        match args.tiny_errors {
            true => eprintln!("{}", TinyError(e)),
            _ => eprintln!("{}", ComplexError(e)),
        }
    }
}
