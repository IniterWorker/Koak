//!
//! JIT (Just In Time) support for Koak
//!

use std;

use llvm_sys::prelude::LLVMValueRef;

use iron_llvm::core;
use iron_llvm::target;
use iron_llvm::{LLVMRef, LLVMRefCtor};
use iron_llvm::execution_engine::{ExecutionEngine, MCJITBuilder};
use iron_llvm::execution_engine::execution_engine::FrozenModule;
use iron_llvm::core::value::{Function, FunctionCtor, FunctionRef, Value};
use iron_llvm::core::types::{FunctionType, FunctionTypeRef};
use iron_llvm::core::types::{RealTypeCtor, RealTypeRef};

use codegen;
use codegen::{IRModuleProvider, IRContext};
use lang::types::KoakType;

pub struct JitModuleProvider {
    module_name: String,
    optimization: bool,
    current_module: core::Module,
    pass_manager: core::FunctionPassManager,
    exec_engine: ExecutionEngine,
    compiled_modules: Vec<FrozenModule>,
}

impl JitModuleProvider {
    ///
    /// Creates a JIT (Just In Time) module provider.
    ///
    pub fn from(name: &str, optimization: bool) -> JitModuleProvider {
        target::initilalize_native_target();
        target::initilalize_native_asm_printer();

        let (exec, tmp_module) = MCJITBuilder::new().create(core::Module::new(name)).expect("Couldn't create JIT Compiler");

        let m = core::Module::new(name);
        let pm = codegen::get_pass_manager(&m, optimization);

        JitModuleProvider {
            module_name: String::from(name),
            optimization: optimization,
            current_module: m,
            pass_manager: pm,
            exec_engine: exec,
            compiled_modules: vec![tmp_module],
        }
    }

    ///
    /// Closes the current modules and creates a new one, replacing the old one.
    /// The closed module is frozen so it can be executed later.
    ///
    pub fn close_current_module(&mut self) {
        let m = core::Module::new(&self.module_name);
        let pm = codegen::get_pass_manager(&m, self.optimization);

        self.pass_manager = pm;
        let current_module = std::mem::replace(&mut self.current_module, m);

        let module = self.exec_engine.add_module(current_module);
        self.compiled_modules.push(module);
    }

    ///
    /// Runs the given function in the current execution engine.
    ///
    /// This function asserts the module containg the given function has been freezed.
    ///
    pub fn run_function(&mut self, context: &IRContext, func: LLVMValueRef) {
        let func_proto = &context.functions[&func.get_name()];
        let func = unsafe { FunctionRef::from_ref(func) };
        let res = self.exec_engine.run_function(&func, Vec::new().as_mut_slice());

        match func_proto.ret_ty {
            KoakType::Bool => println!("=> {}", res.to_int(false) != 0),
            KoakType::Char => {
                let res = res.to_int(true) as u8 as char;
                let v: String = res.escape_default().collect();
                println!("=> '{}'", v);
            },
            KoakType::Int => println!("=> {}", res.to_int(true) as i32),
            KoakType::Double => println!("=> {}", res.to_float(&RealTypeRef::get_double())),
            _ => (),
        }
    }
}

impl IRModuleProvider for JitModuleProvider {
    fn dump(&self) {
        for module in &self.compiled_modules {
            module.get().dump();
        }
        self.current_module.dump();
    }

    fn get_module(&mut self) -> &mut core::Module {
        &mut self.current_module
    }

    ///
    /// Looks for the given function. Returns the function and boolean saying if this
    /// is a protoype (the function is defined elsewhere) or the real definition.
    ///
    fn get_llvm_funcref_by_name(&mut self, name: &str) -> Option<(FunctionRef, bool)> {
        // Search old modules first
        for module in &self.compiled_modules {
            if let Some(func) = module.get().get_function_by_name(name) {
                // We can't call functions from other modules, so we need to add a prototype in the current module
                let proto = match self.current_module.get_function_by_name(name) {
                    Some(f) => {
                        assert!(f.count_basic_blocks() == 0);
                        f
                    },
                    None => {
                        let fty = unsafe { FunctionTypeRef::from_ref(func.get_type().to_ref()) };
                        let fty = unsafe { FunctionTypeRef::from_ref(fty.get_return_type().to_ref()) };
                        FunctionRef::new(&mut self.current_module, name, &fty)
                    }
                };
                return Some((proto, func.count_basic_blocks() > 0));
            };
        }
        // If not found, Search current module
        self.current_module.get_function_by_name(name).map(|e| (e, e.count_basic_blocks() > 0))
    }

    fn get_pass_manager(&mut self) -> &mut core::FunctionPassManager {
        &mut self.pass_manager
    }
}
