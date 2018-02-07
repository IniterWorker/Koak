//!
//! Koak's code generation
//!

use std::collections::HashMap;
use std::iter;

use parser;
use lexer::Token;
use syntaxerror::{SyntaxError, ErrorReason};

use llvm_sys::LLVMRealPredicate::LLVMRealOLT;
use llvm_sys::analysis::LLVMVerifierFailureAction::LLVMAbortProcessAction;
use llvm_sys::prelude::LLVMValueRef;

use iron_llvm::core;
use iron_llvm::core::value::{Function, FunctionCtor, FunctionRef, Value, RealConstRef, RealConstCtor};
use iron_llvm::core::types::{FunctionTypeCtor, FunctionTypeRef, RealTypeCtor, RealTypeRef};
use iron_llvm::{LLVMRef, LLVMRefCtor};

pub struct Context {
    pub file_name: String,
    pub context: core::Context,
    pub builder: core::Builder,
    pub named_values: HashMap<String, LLVMValueRef>,
    pub double_type: RealTypeRef,
}

impl Context {
    #[inline]
    pub fn new(file_name: &str) -> Context {
        Context {
            file_name: file_name.to_string(),
            context: core::Context::get_global(),
            builder: core::Builder::new(),
            named_values: HashMap::new(),
            double_type: RealTypeRef::get_double(),
        }
    }

    pub fn new_syntaxerror(&self, token: &Token, what: ErrorReason) -> SyntaxError {
        SyntaxError::from_token(&self.file_name, token, what)
    }
}

pub trait ModuleProvider {
    fn dump(&self);
    fn get_module(&mut self) -> &mut core::Module;
    fn get_function(&mut self, name: &str) -> Option<(FunctionRef, bool)>;
}

pub struct SimpleModuleProvider {
    module: core::Module,
}

impl SimpleModuleProvider {
    pub fn from(name: &str) -> SimpleModuleProvider {
        SimpleModuleProvider {
            module: core::Module::new(name),
        }
    }
}

impl ModuleProvider for SimpleModuleProvider {
    fn dump(&self) {
        self.module.dump();
    }

    fn get_module(&mut self) -> &mut core::Module {
        &mut self.module
    }

    fn get_function(&mut self, name: &str) -> Option<(FunctionRef, bool)> {
        match self.module.get_function_by_name(name) {
            Some(f) => Some((f, f.count_basic_blocks() > 0)),
            None => None
        }
    }
}

pub type IRResult = Result<LLVMValueRef, SyntaxError>;

pub trait IRBuilder {
    fn gen_ir(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRResult;
}

impl IRBuilder for parser::ASTNode {
    fn gen_ir(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRResult {
        match self {
            &parser::ASTNode::Expr(ref e) => e.gen_ir(context, module_provider),
            &parser::ASTNode::Func(ref proto, ref body) => {
                let mut func = proto.gen_ir(context, module_provider)?;
                let mut func = unsafe { FunctionRef::from_ref(func)};

                // Generate basic block that contain body
                let mut bb = func.append_basic_block_in_context(&mut context.context, "entry");
                context.builder.position_at_end(&mut bb);

                // Add function parameter in current context
                for (param, arg) in func.params_iter().zip(&proto.1) {
                    context.named_values.insert(arg.1.clone(), param.to_ref());
                }

                // Generate body
                let body = body.gen_ir(context, module_provider)?;

                // Return the last instruction
                context.builder.build_ret(&body);

                func.verify(LLVMAbortProcessAction);

                // Clear local variables
                context.named_values.clear();
                Ok(func.to_ref())
            },
            &parser::ASTNode::ExternProto(ref proto) => Ok(proto.gen_ir(context, module_provider)?.to_ref()),
        }
    }
}

impl IRBuilder for parser::Prototype {
    fn gen_ir(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRResult {
        let (ref tok_name, ref name) = self.0;
        let func = match module_provider.get_function(&name) {
            Some((prev_def, have_body)) => {
                if have_body {
                    return Err(context.new_syntaxerror(tok_name, ErrorReason::RedefinedFunc(name.clone())));
                } else if prev_def.count_params() as usize != self.1.len() {
                    return Err(context.new_syntaxerror(tok_name, ErrorReason::RedefinedFuncWithDiffArgs(name.clone())));
                } else {
                    prev_def
                }
            },
            None => {
                let mut params_type = iter::repeat(context.double_type.to_ref()).take(self.1.len()).collect::<Vec<_>>();
                let fty = FunctionTypeRef::get(&context.double_type, params_type.as_mut_slice(), false);
                FunctionRef::new(&mut module_provider.get_module(), name, &fty)
            },
        };

        // Update param name
        for (param, arg) in func.params_iter().zip(&self.1) {
            param.set_name(&arg.1);
        }

        Ok(func.to_ref())
    }
}

impl IRBuilder for parser::Expr {
    fn gen_ir(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRResult {
        match self.1 {
            parser::ExprType::Number(n) => Ok(RealConstRef::get(&context.double_type, n).to_ref()),
            parser::ExprType::Variable(ref s) => match context.named_values.get(s) {
                Some(value) => Ok(*value),
                None => Err(context.new_syntaxerror(&self.0, ErrorReason::UndefinedVariable(s.to_string())))
            },
            parser::ExprType::Binary(op, ref lhs, ref rhs) => {
                let lhs = lhs.gen_ir(context, module_provider)?;
                let rhs = rhs.gen_ir(context, module_provider)?;
                match op {
                    '+' => Ok(context.builder.build_fadd(lhs, rhs, "addtmp")),
                    '-' => Ok(context.builder.build_fsub(lhs, rhs, "subtmp")),
                    '*' => Ok(context.builder.build_fmul(lhs, rhs, "mulmp")),
                    '/' => Ok(context.builder.build_fdiv(lhs, rhs, "divtmp")),
                    '%' => Ok(context.builder.build_frem(lhs, rhs, "modtmp")),
                    '<' => Ok(context.builder.build_fcmp(LLVMRealOLT, lhs, rhs, "cmptmp")),
                    '>' => Ok(context.builder.build_fcmp(LLVMRealOLT, rhs, lhs, "cmptmp")),
                    _ => panic!("Unexpected binary operator \'{}\'.", op),
                }
            }
            parser::ExprType::Call(ref name, ref args) => {
                let (func, _) = module_provider.get_function(name).ok_or(context.new_syntaxerror(&self.0, ErrorReason::UndefinedFunction(name.to_string())))?;
                if args.len() == func.count_params() as usize {
                    let mut args_value = Vec::new();
                    for arg in args.iter() {
                        args_value.push(arg.gen_ir(context, module_provider)?);
                    }
                    Ok(context.builder.build_call(func.to_ref(), args_value.as_mut_slice(), "calltmp"))
                } else {
                    Err(context.new_syntaxerror(&self.0, ErrorReason::WrongArgNumber(name.to_string(), func.count_params() as usize, args.len())))
                }
            },
        }
    }
}
