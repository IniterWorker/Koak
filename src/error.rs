//!
//! Koak's error handling
//!

use std::rc::Rc;
use std::fmt;

use libc;
use ansi_term::Colour::*;

use args::Args;
use lexer::Token;
use lang::types::KoakType;

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
#[derive(Debug, Clone)]
pub enum ErrorReason {
    UnknownChar(char),
    InvalidLiteralNum(String),
    InvalidCharLiteral(String),
    UnterminatedString,
    UnmatchedParenthesis,
    ExprExpected,
    ExpectedFuncName,
    ExpectedOpenParenthesis,
    ExpectedOpenBracket,
    UndefinedVariable(String),
    UndefinedFunction(String),
    WrongArgNumber(String, usize, usize),
    RedefinedFunc(String),
    RedefinedFuncWithDiffArgs(String),
    MissingSemiColonAfterExtern,
    MissingSemiColonAfterTopLevelExpr,
    MissingSemiColonAfterImport,
    ThenTokenExpected,
    ElseTokenExpected,
    ArgTypeExpected,
    RetTypeExpected,
    InvalidType,
    IfBodiesTypeDoesntMatch(KoakType, KoakType),
    IncompatibleBinOp(KoakType, KoakType),
    IncompatibleUnaryOp(KoakType),
    ExpectedNextArgOrCloseParenthesis,
    CantCastTo(KoakType, KoakType),
    ModuleNameExpected,
    CantOpenModule(String, String),
    ModuleContainsErrors(String),
    VoidOnlyReturnType,
    ForLoopIdentifierExpected,
    ExpectedAssignmentAfterVarName,
    ExpectedComma,
    ExpectedInAfterFor,
    UnterminatedBlock,
    ExpectedSemiColorOrCloseBracket,
    ExpectedAssignmentVarName,
    VarAlreadyDefined(String),
    ReassigningConstVar(String),
    AssigningRvalue,
    TopLevelAssignForbidden,
    CantAssignVoidValue,
}

///
/// Implementation of the Display trait to print each error reasons correctly.
///
impl fmt::Display for ErrorReason {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        match *self {
            ErrorReason::UnknownChar(ref c) =>
                write!(f, "Unknown char \'{}\'", purple!(c)),
            ErrorReason::InvalidLiteralNum(ref s) =>
                write!(f, "Invalid literal number \"{}\"", purple!(s)),
            ErrorReason::InvalidCharLiteral(ref s) =>
                write!(f, "Invalid literal char \'{}\'", purple!(s)),
            ErrorReason::UnterminatedString =>
                write!(f, "Unterminated string"),
            ErrorReason::UnmatchedParenthesis =>
                write!(f, "Unmatched parenthesis"),
            ErrorReason::ExprExpected =>
                write!(f, "An expression was expected"),
            ErrorReason::ExpectedFuncName =>
                write!(f, "Function name was expected in a prototype"),
            ErrorReason::ExpectedOpenParenthesis =>
                write!(f, "Open parenthesis was expected after function name in a prototype"),
            ErrorReason::ExpectedOpenBracket =>
                write!(f, "Open bracket was expected to initiate a block"),
            ErrorReason::UndefinedVariable(ref s) =>
                write!(f, "Undefined variable \"{}\"", purple!(s)),
            ErrorReason::UndefinedFunction(ref s) =>
                write!(f, "Undefined function \"{}\"", purple!(s)),
            ErrorReason::WrongArgNumber(ref name, expected, given) =>
                write!(f, "Wrong number of argument: The function \"{}\" expects {} argument(s), but {} are given", purple!(name), expected, given),
            ErrorReason::RedefinedFunc(ref name) =>
                write!(f, "Redefinition of function \"{}\"", purple!(name)),
            ErrorReason::RedefinedFuncWithDiffArgs(ref func) =>
                write!(f, "Function \"{}\" redefined with different arguments", purple!(func.to_string())),
            ErrorReason::MissingSemiColonAfterExtern =>
                write!(f, "Missing semi-colon at the end of an extern declaration"),
            ErrorReason::MissingSemiColonAfterTopLevelExpr =>
                write!(f, "Missing semi-colon at the end of a top-level expression"),
            ErrorReason::MissingSemiColonAfterImport =>
                write!(f, "Missing semi-colon at the end an import declaration"),
            ErrorReason::ThenTokenExpected =>
                write!(f, "\"{}\" is expected after an \"{}\"", purple!("then"), purple!("if")),
            ErrorReason::ElseTokenExpected =>
                write!(f, "\"{}\" is expected after a \"{}\"", purple!("else"), purple!("then")),
            ErrorReason::ArgTypeExpected =>
                write!(f, "Argument type is expected"),
            ErrorReason::RetTypeExpected =>
                write!(f, "Return type is expected"),
            ErrorReason::InvalidType =>
                write!(f, "Given type isn't valid"),
            ErrorReason::IfBodiesTypeDoesntMatch(ref a, ref b) =>
                write!(f, "If bodies's type doesn't match. Got \"{}\" on one side, and \"{}\" on the other side", purple!(a), purple!(b)),
            ErrorReason::IncompatibleBinOp(ref lhs, ref rhs) =>
                write!(f, "Invalid binary operator for type \"{}\" and \"{}\"", purple!(lhs), purple!(rhs)),
            ErrorReason::ExpectedNextArgOrCloseParenthesis =>
                write!(f, "Expected next function's argument or a close parenthesis"),
            ErrorReason::CantCastTo(ref a, ref b) =>
                write!(f, "Can't cast type \"{}\" to type \"{}\"", purple!(a), purple!(b)),
            ErrorReason::IncompatibleUnaryOp(ref a) =>
                write!(f, "Invalid unary operator for type \"{}\"", purple!(a)),
            ErrorReason::ModuleNameExpected =>
                write!(f, "Module name was expected"),
            ErrorReason::CantOpenModule(ref name, ref error) =>
                write!(f, "Can't open module \"{}\": {}", purple!(name), error),
            ErrorReason::ModuleContainsErrors(ref name) =>
                write!(f, "The module \"{}\" contains errors", purple!(name)),
            ErrorReason::VoidOnlyReturnType =>
                write!(f, "The \"{}\" type can only be used as a return type of a function", purple!("void")),
            ErrorReason::ForLoopIdentifierExpected =>
                write!(f, "A for-loop must begin with an identifier"),
            ErrorReason::ExpectedAssignmentAfterVarName =>
                write!(f, "An equal symbol ('{}') is expected in an assignation", purple!("=")),
            ErrorReason::ExpectedComma =>
                write!(f, "An comma symbol ('{}') is expected", purple!(",")),
            ErrorReason::ExpectedInAfterFor =>
                write!(f, "The \"{}\" keyword is expected after a \"{}\" declaration", purple!("in"), purple!("for")),
            ErrorReason::UnterminatedBlock =>
                write!(f, "Unterminated block"),
            ErrorReason::ExpectedSemiColorOrCloseBracket =>
                write!(f, "Unexpected token: A '{}' or a '{}' is expected", purple!(";"), purple!("}")),
            ErrorReason::ExpectedAssignmentVarName =>
                write!(f, "The variable name is expected after a let construct"),
            ErrorReason::VarAlreadyDefined(ref name) =>
                write!(f, "The variable \"{}\" is already defined", purple!(name)),
            ErrorReason::ReassigningConstVar(ref name) =>
                write!(f, "Can't re-assign the constant variable \"{}\"", purple!(name)),
            ErrorReason::AssigningRvalue =>
                write!(f, "Can't assign an r-value expression"),
            ErrorReason::TopLevelAssignForbidden =>
                write!(f, "Top level assigniations in JIT environnement are forbidden (yeah, that sucks)"),
            ErrorReason::CantAssignVoidValue =>
                write!(f, "Can't assign variable, operand is of type \"{}\"", purple!("void")),
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

pub fn print_errors(args: &Args, errors: &[SyntaxError]) {
    for e in errors {
        if args.tiny_errors {
            eprintln!("{}", TinyError(e));
        }
        else {
            eprintln!("{}", ComplexError(e));
        }
    }
}
