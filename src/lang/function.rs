//!
//! Koak's functions
//!

use std::fmt;
use std::rc::Rc;

use llvm_sys::prelude::LLVMTypeRef;
use llvm_sys::analysis::LLVMVerifierFailureAction::LLVMAbortProcessAction;
use llvm_sys::core::LLVMDeleteFunction;

use iron_llvm::core::value::{Function, FunctionRef, FunctionCtor};
use iron_llvm::core::types::{FunctionTypeCtor, FunctionTypeRef};
use iron_llvm::LLVMRef;

use lexer::{Token, TokenType};
use parser::Parser;
use lang::Type;
use lang::expr::{Expr, parse_expr};
use codegen::{IRContext, IRModuleProvider, IRGenerator, IRResult, IRExprGenerator};
use error::{SyntaxError, ErrorReason};
use lang::variable::Variable;

pub struct ConcreteArg {
    pub name: Rc<String>,
    pub token: Token,
    pub ty: Type,
}

impl ConcreteArg {
    #[inline]
    pub fn new(name: Rc<String>, token: Token, ty: Type) -> ConcreteArg {
        ConcreteArg {
            name: name,
            token: token,
            ty: ty,
        }
    }
}

impl fmt::Debug for ConcreteArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}: {}", self.name, self.ty)
    }
}

pub struct ConcreteFunction {
    pub token: Token,
    pub name: Rc<String>,
    pub args: Vec<ConcreteArg>,
    pub ret: Type,
    pub body: Option<Expr>,
}

impl ConcreteFunction {
    #[inline]
    pub fn new(token: Token, name: Rc<String>, args: Vec<ConcreteArg>, ret: Type, body: Option<Expr>) -> ConcreteFunction {
        ConcreteFunction {
            token: token,
            name: name,
            args: args,
            ret: ret,
            body: body,
        }
    }

    #[inline]
    pub fn new_anonymous(b: Expr) -> ConcreteFunction {
        static mut NB_ANON: u64 = 0;

        let mut name = String::from("__"); // Create unique name for anonymous function
        unsafe {
            name += &NB_ANON.to_string();
            name += "$"; // Prevent the name from being taken by a user-defined function
            NB_ANON += 1;
        }
        ConcreteFunction::new(Token::new(), Rc::new(name), Vec::new(), Type::Int, Some(b)) // TODO Change this to an abstract type
    }
}

impl fmt::Debug for ConcreteFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{} (", self.name)?;
        for (pos, arg) in self.args.iter().enumerate() {
            if pos == 0 {
                write!(f, "{:?}", arg)?;
            } else {
                write!(f, ", {:?}", arg)?;
            }
        }
        write!(f, ") -> {}", self.ret)
    }
}

impl IRGenerator for ConcreteFunction {
    #[inline]
    fn gen_ir(&self, context: &mut IRContext, module_provider: &mut IRModuleProvider) -> IRResult {

        // Find function with same name
        let mut func = match module_provider.get_llvm_funcref_by_name(&self.name) {
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

                let mut params_type: Vec<LLVMTypeRef> = self.args.iter().map(|a| a.ty.as_llvm_ref()).collect();

                let ret_ty = FunctionTypeRef::get(&self.ret.as_llvm_ref(), params_type.as_mut_slice(), false);
                let f = FunctionRef::new(&mut module_provider.get_module(), &self.name, &ret_ty);
                use iron_llvm::core::Value;
                f.dump();
                f
            },
        };

        // Continue only if we are not in an extern declaration
        if let Some(ref body_expr) = self.body {
            // Update param name
            for (param, arg) in func.params_iter().zip(&self.args) {
                use iron_llvm::core::Value;

                param.set_name(&arg.name);
            }

            // Generate basic block that contain body
            let mut bb = func.append_basic_block_in_context(&mut context.context, "entry");
            context.builder.position_at_end(&mut bb);

            // Add function parameter in current context
            for (llvm_arg_ref, arg) in func.params_iter().zip(&self.args) {
                let arg_value = arg.ty.new_value(llvm_arg_ref.to_ref()).unwrap();
                let var = Variable::new(arg.name.clone(), arg_value);
                context.add_var(arg.name.clone(), var);
            }

            // Generate body + remove function on error (to let the user redefines it later)
            let ret_val = body_expr.gen_ir(context, module_provider).map_err(|x| {
                    unsafe { LLVMDeleteFunction(func.to_ref()); }
                    x
            })?;

            // Return the last instruction
            context.builder.build_ret(&ret_val.as_llvm_ref());

            use iron_llvm::core::Value;
            func.dump();

            // Let LLVM Verify our function
            func.verify(LLVMAbortProcessAction);

            // Optimize this function
            module_provider.get_pass_manager().run(&mut func);

            Ok(Some(func.to_ref()))
        } else {
            return Ok(Some(func.to_ref()));
        }
    }
}

pub fn parse_prototype(parser: &mut Parser) -> Result<ConcreteFunction, SyntaxError> { // TODO Change this to abstract function
    let iden = parser.next_or(ErrorReason::ExpectedFuncName)?;
    if let TokenType::Identifier(_) = iden.token_type {
        parser.next_of(TokenType::OpenParenthesis, ErrorReason::ExpectedOpenParenthesis)?;

        // Parse args
        let mut args = Vec::new();
        while let Some(&TokenType::Identifier(_)) = parser.peek_type() {
            let arg_token = parser.tokens.pop().unwrap();
            let arg_name = if let TokenType::Identifier(ref s) = arg_token.token_type { s.clone() } else { unreachable!() };

            // Parse argument type
            parser.next_of(TokenType::Colon, ErrorReason::ArgTypeExpected)?;

            let ty = parser.next_or(ErrorReason::ArgTypeExpected)?;
            if let TokenType::Type(arg_type) = ty.token_type {
                args.push(ConcreteArg::new(arg_name, arg_token, arg_type));
            } else {
                return Err(SyntaxError::from(&ty, ErrorReason::InvalidType));
            }

            // Try to eat comma
            let t = parser.next_or(ErrorReason::ExpectedNextArgOrCloseParenthesis)?;
            match t.token_type {
                TokenType::Comma => continue,
                TokenType::CloseParenthesis => break,
                _ => return Err(SyntaxError::from(&t, ErrorReason::ExpectedNextArgOrCloseParenthesis)),
            }
        }

        parser.next_of(TokenType::Arrow, ErrorReason::RetTypeExpected)?;
        let ty = parser.next_or(ErrorReason::ArgTypeExpected)?;
        if let TokenType::Type(ret_type) = ty.token_type {
            let func_name = if let TokenType::Identifier(ref s) = iden.token_type { s.clone() } else { unreachable!() };
            Ok(ConcreteFunction::new(iden, func_name, args, ret_type, None))
        } else {
            return Err(SyntaxError::from(&ty, ErrorReason::InvalidType));
        }
    } else {
        Err(SyntaxError::from(&iden, ErrorReason::ExpectedFuncName))
    }
}

#[inline]
pub fn parse_extern_func(parser: &mut Parser) -> Result<ConcreteFunction, SyntaxError> {
    parser.tokens.pop().unwrap(); // Eat extern

    let func = parse_prototype(parser)?;
    parser.next_of(TokenType::SemiColon, ErrorReason::MissingSemiColonAfterExtern)?;
    Ok(func)
}

#[inline]
pub fn parse_func_def(parser: &mut Parser) -> Result<ConcreteFunction, SyntaxError> {
    parser.tokens.pop().unwrap(); // Eat def

    let mut func = parse_prototype(parser)?;
    let content = parse_expr(parser)?;

    parser.next_of(TokenType::SemiColon, ErrorReason::MissingSemiColonAfterExtern)?;
    func.body = Some(content);
    Ok(func)
}
