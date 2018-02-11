//!
//! Koak's IR generator
//!

use std::collections::HashMap;

use llvm_sys::prelude::LLVMValueRef;

use iron_llvm::core;
use iron_llvm::core::types::{RealTypeCtor, RealTypeRef};
use iron_llvm::core::value::FunctionRef;

use error::SyntaxError;

///
/// Structure holding LLVM's stuff, used when generating IR code.
///
pub struct IRContext {
    pub context: core::Context,
    pub builder: core::Builder,
    pub named_values: HashMap<String, LLVMValueRef>,
    pub double_type: RealTypeRef,
}

impl IRContext {
    pub fn new() -> IRContext {
        IRContext {
            context: core::Context::get_global(),
            builder: core::Builder::new(),
            named_values: HashMap::new(),
            double_type: RealTypeRef::get_double(),
        }
    }
}

///
/// Trait that each language element must provide to generate their corresponding IR code.
///
pub type IRResult = Result<LLVMValueRef, SyntaxError>;
pub trait IRGenerator {
    fn gen_ir(&self, context: &mut IRContext, module_provider: &mut IRModuleProvider) -> IRResult;
}

///
/// Trait that all module providers must implement (JIT, ASM etc.)
///
pub trait IRModuleProvider {
    fn dump(&self);
    fn get_module(&mut self) -> &mut core::Module;
    fn get_function(&mut self, name: &str) -> Option<FunctionRef>;
    fn get_pass_manager(&mut self) -> &mut core::FunctionPassManager;
}

pub struct SimpleModuleProvider {
    module: core::Module,
    pass_manager: core::FunctionPassManager,
}

impl SimpleModuleProvider {
    pub fn from(name: &str, optimizations: bool) -> SimpleModuleProvider {
        let m = core::Module::new(name);
        let pm = get_pass_manager(&m, optimizations);

        SimpleModuleProvider {
            module: core::Module::new(name),
            pass_manager: pm,
        }
    }
}

impl IRModuleProvider for SimpleModuleProvider {
    fn dump(&self) {
        self.module.dump();
    }

    fn get_module(&mut self) -> &mut core::Module {
        &mut self.module
    }

    fn get_function(&mut self, name: &str) -> Option<FunctionRef> {
        self.module.get_function_by_name(name)
    }

    fn get_pass_manager(&mut self) -> &mut core::FunctionPassManager {
        &mut self.pass_manager
    }
}

///
/// Returns a pass manager depending on the given parameters
///
pub fn get_pass_manager(m: &core::Module, optimizations: bool) -> core::FunctionPassManager {
    let mut pm = core::FunctionPassManager::new(&m);
    if optimizations {
        pm.add_basic_alias_analysis_pass();
        pm.add_instruction_combining_pass();
        pm.add_reassociate_pass();
        pm.add_GVN_pass();
        pm.add_CFG_simplification_pass();
    }
    pm.initialize();
    pm
}