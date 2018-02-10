//!
//! This module implements extern function declaration.
//!

use std::fmt;

use parser::Parser;
use error::SyntaxError;
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
    parser.tokens.pop(); // Eat def
    Ok(ExternFunc::new(parse_prototype(parser)?))
}

impl IRGenerator for ExternFunc {
    #[inline]
    fn gen_ir(&self, context: &mut IRContext, module_provider: &mut IRModuleProvider) -> IRResult {
        self.prototype.gen_ir(context, module_provider)
    }
}
