//!
//! Koak's functions
//!

use std::fmt;
use std::rc::Rc;

use llvm_sys::prelude::LLVMTypeRef;
use llvm_sys::analysis::LLVMVerifierFailureAction::LLVMAbortProcessAction;
use llvm_sys::core::LLVMDeleteFunction;

use iron_llvm::LLVMRef;
use iron_llvm::core::value::{Function, FunctionRef, FunctionCtor};
use iron_llvm::core::types::{FunctionTypeCtor, FunctionTypeRef};

use lexer::{Token, TokenType};
use parser::Parser;
use lang::types;
use lang::value::KoakValue;
use lang::types::KoakType;
use lang::block::{Block, parse_bracket_block};
use codegen::{IRContext, IRModuleProvider, IRExprGenerator, IRFuncGenerator, IRFuncResult};
use error::{SyntaxError, ErrorReason};

pub struct FunctionArg {
    pub name: Rc<String>,
    pub token: Token,
    pub ty: KoakType,
}

impl FunctionArg {
    #[inline]
    pub fn new(name: Rc<String>, token: Token, ty: KoakType) -> FunctionArg {
        FunctionArg {
            name: name,
            token: token,
            ty: ty,
        }
    }
}

impl fmt::Debug for FunctionArg {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{}: {}", self.name, self.ty)
    }
}

pub struct FunctionPrototype {
    pub token: Token,
    pub name: Rc<String>,
    pub args: Vec<FunctionArg>,
    pub ret_ty: KoakType,
}

impl FunctionPrototype {
    pub fn new(token: Token, name: Rc<String>, args: Vec<FunctionArg>, ret_ty: KoakType) -> FunctionPrototype {
        FunctionPrototype {
            token: token,
            name: name,
            args: args,
            ret_ty: ret_ty,
        }
    }
}

pub struct ConcreteFunction {
    pub proto: Rc<FunctionPrototype>,
    pub body: Option<Block>,
}

impl ConcreteFunction {
    #[inline]
    pub fn new(proto: Rc<FunctionPrototype>, body: Option<Block>) -> ConcreteFunction {
        ConcreteFunction {
            proto: proto,
            body: body,
        }
    }
}

impl fmt::Debug for ConcreteFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        write!(f, "{} (", self.proto.name)?;
        for (pos, arg) in self.proto.args.iter().enumerate() {
            if pos == 0 {
                write!(f, "{:?}", arg)?;
            } else {
                write!(f, ", {:?}", arg)?;
            }
        }
        write!(f, ") -> {}", self.proto.ret_ty)?;
        if let Some(ref body) = self.body {
            write!(f, " {}", body)?;
        }
        Ok(())
    }
}

pub fn parse_prototype(parser: &mut Parser) -> Result<FunctionPrototype, SyntaxError> {
    let iden = parser.next_or(ErrorReason::ExpectedFuncName)?;
    if let TokenType::Identifier(_) = iden.token_type {
        let func_name = if let TokenType::Identifier(ref s) = iden.token_type { s.clone() } else { unreachable!() };

        parser.next_of(&TokenType::OpenParenthesis, ErrorReason::ExpectedOpenParenthesis)?;

        // Parse args
        let mut args = Vec::new();
        while let Some(&TokenType::Identifier(_)) = parser.peek_type() {
            let arg_token = parser.tokens.pop().unwrap();
            let arg_name = if let TokenType::Identifier(ref s) = arg_token.token_type { s.clone() } else { unreachable!() };

            // Parse argument type
            parser.next_of(&TokenType::Colon, ErrorReason::ArgTypeExpected)?;
            let ty = parser.next_or(ErrorReason::ArgTypeExpected)?;
            let koak_type = types::KoakType::from(&ty)?;
            if let KoakType::Void = koak_type {
                return Err(SyntaxError::from(&ty, ErrorReason::VoidOnlyReturnType));
            }
            args.push(FunctionArg::new(arg_name, arg_token, types::KoakType::from(&ty)?));

            // Try to eat comma
            if let TokenType::Comma = parser.peek_or(ErrorReason::ExpectedNextArgOrCloseParenthesis)?.token_type {
                parser.tokens.pop();
            }
        }
        parser.next_of(&TokenType::CloseParenthesis, ErrorReason::UnmatchedParenthesis)?;
        parser.next_of(&TokenType::Arrow, ErrorReason::RetTypeExpected)?;
        let ret_type = types::KoakType::from(&parser.next_or(ErrorReason::ArgTypeExpected)?)?;
        Ok(FunctionPrototype::new(iden, func_name, args, ret_type))
    } else {
        Err(SyntaxError::from(&iden, ErrorReason::ExpectedFuncName))
    }
}

#[inline]
pub fn parse_extern_func(parser: &mut Parser) -> Result<ConcreteFunction, SyntaxError> {
    parser.tokens.pop().unwrap(); // Eat extern

    let proto = parse_prototype(parser)?;
    parser.next_of(&TokenType::SemiColon, ErrorReason::MissingSemiColonAfterExtern)?;
    Ok(ConcreteFunction::new(Rc::new(proto), None))
}

#[inline]
pub fn parse_func_def(parser: &mut Parser) -> Result<ConcreteFunction, SyntaxError> {
    parser.tokens.pop().unwrap(); // Eat def

    let proto = parse_prototype(parser)?;
    parser.next_of(&TokenType::OpenBracket, ErrorReason::ExpectedOpenBracket)?;
    let body = Some(parse_bracket_block(parser)?);

    Ok(ConcreteFunction::new(Rc::new(proto), body))
}

impl IRFuncGenerator for ConcreteFunction {
    #[inline]
    fn gen_ir(&self, context: &mut IRContext, module_provider: &mut IRModuleProvider) -> IRFuncResult {

        // Find function with same name
        let mut func = match module_provider.get_llvm_funcref_by_name(&self.proto.name) {
            Some((prev_def, have_body)) => { // Function already defined. This is ok only if there is no implementation yet.
                if have_body && self.body.is_some() {
                    return Err(SyntaxError::from(&self.proto.token, ErrorReason::RedefinedFunc((*self.proto.name).clone())));
                } else if prev_def.count_params() as usize != self.proto.args.len() {
                    return Err(SyntaxError::from(&self.proto.token, ErrorReason::RedefinedFuncWithDiffArgs((*self.proto.name).clone())));
                } else {
                    // Compare arguments's types
                    let old_func = &context.functions[&self.proto.name];
                    for (prev, arg) in old_func.args.iter().zip(self.proto.args.iter()) {
                        if prev.ty != arg.ty {
                            return Err(SyntaxError::from(&self.proto.token, ErrorReason::RedefinedFuncWithDiffArgs((*self.proto.name).clone())));
                        }
                    }
                    // Compare return value's type
                    if old_func.ret_ty != self.proto.ret_ty {
                        return Err(SyntaxError::from(&self.proto.token, ErrorReason::RedefinedFuncWithDiffArgs((*self.proto.name).clone())));
                    }
                    prev_def
                }
            },
            None => { // New function
                let mut params_type: Vec<LLVMTypeRef> = self.proto.args.iter().map(|a| a.ty.as_llvm_ref()).collect();
                let func_ty_ref = FunctionTypeRef::get(&self.proto.ret_ty.as_llvm_ref(), params_type.as_mut_slice(), false);
                FunctionRef::new(&mut module_provider.get_module(), &self.proto.name, &func_ty_ref)
            },
        };

        // Add it to context to enable recursivity. Store old to put it back in case of failure
        let old = context.functions.insert(self.proto.name.clone(), self.proto.clone());

        do catch {

            // Continue only if we are not in an extern declaration
            if let Some(ref body_block) = self.body {

                // Generate entry block
                let mut bb = func.append_basic_block_in_context(&mut context.context, "entry");
                context.builder.position_at_end(&mut bb);

                // Update param name and add them to current context
                for (param_value, arg) in func.get_params().iter().zip(&self.proto.args) {
                    use iron_llvm::core::Value;

                    param_value.set_name(&arg.name);
                    context.create_local_var(&func, arg.name.clone(), KoakValue::new(*param_value, arg.ty));
                }

                // Generate body
                let ret_val = body_block.gen_ir(context, module_provider)?;

                // Append return
                if let KoakType::Void = self.proto.ret_ty {
                    context.builder.build_ret_void();
                } else {
                    // Cast it to return type
                    let ret_casted = types::cast_to(&body_block.token, ret_val, self.proto.ret_ty, context)?;
                    context.builder.build_ret(&ret_casted.llvm_ref);
                }

                // Let LLVM Verify our function
                func.verify(LLVMAbortProcessAction);

                // Optimize this function
                module_provider.get_pass_manager().run(&mut func);

            }
            Ok(func.to_ref())
        }.map_err(|e| unsafe {
            LLVMDeleteFunction(func.to_ref()); // Delete function on error, to let the user redefine it later
            match old {
                Some(old) => context.functions.insert(self.proto.name.clone(), old),
                None => context.functions.remove(&self.proto.name),
            };
            e
        })
    }
}
