//!
//! This module implements function definition and implementation.
//!

use std::fmt;
use std::rc::Rc;

use llvm_sys::analysis::LLVMVerifierFailureAction::LLVMAbortProcessAction;
use llvm_sys::core::LLVMDeleteFunction;

use iron_llvm::core::value::{Function, FunctionRef};
use iron_llvm::{LLVMRef, LLVMRefCtor};

use lexer::{Token, TokenType};
use parser::Parser;
use error::{SyntaxError, ErrorReason};
use lang::expr::{Expr, parse_expr};
use lang::prototype::{Prototype, parse_prototype};
use codegen::{IRContext, IRGenerator, IRResult, IRModuleProvider};

///
/// An function definition is the prototype and the body of a function.
///
pub struct Func {
    pub prototype: Prototype,
    pub body: Expr,
}

impl Func {
    #[inline]
    pub fn new(p: Prototype, b: Expr) -> Func {
        Func {
            prototype: p,
            body: b,
        }
    }

    #[inline]
    pub fn new_anonymous(b: Expr) -> Func {
        static mut NB_ANON: u64 = 0;

        let mut name = String::from("__"); // Create unique name for anonymous function
        unsafe {
            name += &NB_ANON.to_string();
            name += "$"; // Prevent the name from being taken by a user-defined function
            NB_ANON += 1;
        }
        Func::new(Prototype::new(Token::new(), Rc::new(name), Vec::new()), b)
    }
}

impl fmt::Debug for Func {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}, {:?}", self.prototype, self.body)
    }
}

#[inline]
pub fn parse_func_def(parser: &mut Parser) -> Result<Func, SyntaxError> {
    let def = parser.tokens.pop().unwrap(); // Eat def
    let proto = parse_prototype(parser)?;
    let content = parse_expr(parser)?;

    if let Some(&TokenType::SemiColon) = parser.peek_type() { // Check for semi-colon
        parser.tokens.pop();
        Ok(Func::new(proto, content))
    } else {
        Err(SyntaxError::from(&def, ErrorReason::MissingSemiColonAfterDef))
    }
}

impl IRGenerator for Func {
    #[inline]
    fn gen_ir(&self, context: &mut IRContext, module_provider: &mut IRModuleProvider) -> IRResult {
        let func = self.prototype.gen_ir(context, module_provider)?;
        let mut func = unsafe { FunctionRef::from_ref(func)};

        // Generate basic block that contain body
        let mut bb = func.append_basic_block_in_context(&mut context.context, "entry");
        context.builder.position_at_end(&mut bb);

        // Add function parameter in current context
        for (param, arg) in func.params_iter().zip(&self.prototype.args) {
            context.named_values.insert((*arg.1).clone(), param.to_ref());
        }

        // Generate body + remove function on error (to let the user redefines it later)
        let body = self.body.gen_ir(context, module_provider).map_err(|x| {
                unsafe { LLVMDeleteFunction(func.to_ref()); }
                x
        })?;

        // Return the last instruction
        context.builder.build_ret(&body);

        // Let LLVM Verify our function
        func.verify(LLVMAbortProcessAction);

        // Optimize this function
        module_provider.get_pass_manager().run(&mut func);

        // Clear local variables
        context.named_values.clear();
        Ok(func.to_ref())
    }
}
