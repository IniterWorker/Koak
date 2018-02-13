//!
//! This module implements extern function declaration.
//!

use std::fmt;

use lexer::TokenType;
use parser::Parser;
use error::{SyntaxError, ErrorReason};
use lang::prototype::{Prototype, parse_prototype};
use codegen::{IRContext, IRGenerator, IRResult, IRModuleProvider};

///
/// An extern declaration is simply the prototype of a function.
///
pub struct ExternFunc {
    pub prototype: Prototype,
}

impl ExternFunc {
    #[inline]
    pub fn new(p: Prototype) -> ExternFunc {
        ExternFunc {
            prototype: p,
        }
    }
}

impl fmt::Debug for ExternFunc {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.prototype)
    }
}

#[inline]
pub fn parse_extern_func(parser: &mut Parser) -> Result<ExternFunc, SyntaxError> {
    let ext = parser.tokens.pop().unwrap(); // Eat extern

    let proto = parse_prototype(parser)?;

    if let Some(&TokenType::SemiColon) = parser.peek_type() { // Check for semi-colon
        parser.tokens.pop();
        Ok(ExternFunc::new(proto))
    } else {
        Err(SyntaxError::from(&ext, ErrorReason::MissingSemiColonAfterExtern))
    }
}

impl IRGenerator for ExternFunc {
    #[inline]
    fn gen_ir(&self, context: &mut IRContext, module_provider: &mut IRModuleProvider) -> IRResult {
        self.prototype.gen_ir(context, module_provider)
    }
}
