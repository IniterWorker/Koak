//!
//! Koak's code generation
//!

use std::collections::HashMap;

use parser;

use llvm_sys::LLVMRealPredicate::LLVMRealOLT;
use llvm_sys::analysis::LLVMVerifierFailureAction::LLVMAbortProcessAction;
use llvm_sys::core::LLVMDeleteFunction;
use llvm_sys::prelude::LLVMValueRef;

use iron_llvm::core;
use iron_llvm::core::value::{Function, FunctionCtor, FunctionRef, Value, RealConstRef, RealConstCtor};
use iron_llvm::core::types::{FunctionTypeCtor, FunctionTypeRef, RealTypeCtor, RealTypeRef};
use iron_llvm::{LLVMRef, LLVMRefCtor};

pub struct Context {
    pub context: core::Context,
    pub builder: core::Builder,
    pub named_values: HashMap<String, LLVMValueRef>,
    pub double_type: RealTypeRef,
}

impl Context {
    #[inline]
    pub fn new() -> Context {
        Context {
            context: core::Context::get_global(),
            builder: core::Builder::new(),
            named_values: HashMap::new(),
            double_type: RealTypeRef::get_double(),
        }
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

pub type IRResult = Result<(LLVMValueRef, bool), String>;

pub trait IRBuilder {
    fn gen_ir(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRResult;
}

impl IRBuilder for parser::ASTNode {
    fn gen_ir(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRResult {
        match self {
            &parser::ASTNode::Expr(ref e) => e.gen_ir(context, module_provider),
            _ => unimplemented!(),
        }
    }
}

impl IRBuilder for parser::Expr {
    fn gen_ir(&self, context: &mut Context, module_provider: &mut ModuleProvider) -> IRResult {
        match self {
            &parser::Expr::Number(n) => Ok((RealConstRef::get(&context.double_type, n).to_ref(), false)),
            &parser::Expr::Variable(ref s) => match context.named_values.get(s) {
                Some(value) => Ok((*value, false)),
                None => Err(String::from("unknown variable name")),
            },
            _ => unimplemented!(),
        }
    }
}
