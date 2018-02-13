//!
//! This module implements prototypes. It's used by the `function` and `extern` module, and not on its own.
//!

use std::fmt;
use std::iter;
use std::rc::Rc;

use iron_llvm::LLVMRef;
use iron_llvm::core::value::{Function, FunctionCtor, FunctionRef, Value};
use iron_llvm::core::types::{FunctionTypeCtor, FunctionTypeRef};

use lexer::{Token, TokenType};
use parser::Parser;
use error::{SyntaxError, ErrorReason};
use codegen::{IRContext, IRGenerator, IRResult, IRModuleProvider};

pub struct Prototype {
    pub token: Token,
    pub name: Rc<String>,
    pub args: Vec<(Token, Rc<String>)>,
}

impl Prototype {
    #[inline]
    pub fn new(token: Token, name: Rc<String>, args: Vec<(Token, Rc<String>)>) -> Prototype {
        Prototype {
            token: token,
            name: name,
            args: args,
        }
    }
}

impl fmt::Debug for Prototype {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}, {:?}", self.name, self.args.iter().map(|x| &x.1).collect::<Vec<&Rc<String>>>())
    }
}

pub fn parse_prototype(parser: &mut Parser) -> Result<Prototype, SyntaxError> {
    let iden = parser.next_or(ErrorReason::ExpectedFuncName)?;
    if let TokenType::Identifier(_) = iden.token_type {
        let open = parser.next_or(ErrorReason::ExpectedOpenParenthesis)?;
        match open.token_type {
            TokenType::OpenParenthesis => {

                // Parse args
                let mut args = Vec::new();
                while let Some(&TokenType::Identifier(_)) = parser.peek_type() {
                    let arg_token = parser.tokens.pop().unwrap();
                    let arg_name = if let TokenType::Identifier(ref s) = arg_token.token_type { s.clone() } else { unreachable!() };
                    args.push((arg_token, arg_name));
                }

                let close = parser.next_or(ErrorReason::UnmatchedParenthesis)?;
                match close.token_type {
                    TokenType::CloseParenthesis => {
                        let func_name = if let TokenType::Identifier(ref s) = iden.token_type { s.clone() } else { unreachable!() };
                        Ok(Prototype::new(iden, func_name, args))
                    }
                    _ => Err(SyntaxError::from(&close, ErrorReason::ArgMustBeIdentifier))
                }
            },
            _ => Err(SyntaxError::from(&open, ErrorReason::ExpectedOpenParenthesis))
        }
    } else {
        Err(SyntaxError::from(&iden, ErrorReason::ExpectedFuncName))
    }
}

impl IRGenerator for Prototype {
    fn gen_ir(&self, context: &mut IRContext, module_provider: &mut IRModuleProvider) -> IRResult {

        // Find function with same name
        let func = match module_provider.get_function(&self.name) {
            Some(prev_def) => { // Function already defined. This is ok only if there is no implementation yet.
                if prev_def.count_basic_blocks() > 0 {
                    return Err(SyntaxError::from(&self.token, ErrorReason::RedefinedFunc((*self.name).clone())));
                } else if prev_def.count_params() as usize != self.args.len() {
                    return Err(SyntaxError::from(&self.token, ErrorReason::RedefinedFuncWithDiffArgs((*self.name).clone())));
                } else {
                    prev_def
                }
            },
            None => { // New function
                let mut params_type = iter::repeat(context.double_type.to_ref()).take(self.args.len()).collect::<Vec<_>>();
                let fty = FunctionTypeRef::get(&context.double_type, params_type.as_mut_slice(), false);
                FunctionRef::new(&mut module_provider.get_module(), &self.name, &fty)
            },
        };

        // Update param name
        for (param, arg) in func.params_iter().zip(&self.args) {
            param.set_name(&arg.1);
        }

        Ok(func.to_ref())
    }
}
