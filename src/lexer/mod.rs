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
