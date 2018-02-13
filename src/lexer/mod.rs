//!
//! Koak's Lexer Module.
//!

mod token;
mod lexer;

pub use self::token::Token as Token;
pub use self::token::TokenType as TokenType;
pub use self::lexer::Lexer as Lexer;

use error::SyntaxError;

pub type LexerResult = Result<Token, SyntaxError>;

#[allow(dead_code)]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum OperatorType {
    Add,          // '+'
    Sub,          // '-'
    Mul,          // '*'
    Div,          // '/'
    Rem,          // '%'
    Shl,          // '<<'
    Shr,          // '>>'
    Or,           // '|'
    And,          // '&'
    Xor,          // '^'

    Assign,       // '='
    AddAssign,    // '+='
    SubAssign,    // '-='
    MulAssign,    // '*='
    DivAssign,    // '/='
    RemAssign,    // '%='
    ShlAssign,    // '<<='
    ShrAssign,    // '>>='
    OrAssign,     // '|='
    AndAssign,    // '&='
    XorAssign,    // '^='

    Less,         // '<'
    More,         // '>'
    LessOrEqual,  // '<='
    MoreOrEqual,  // '>='
    Equal,        // '=='
    Different,    // '!='

    LogicalAnd,   // '&&'
    LogicalOr,    // '||'
}
