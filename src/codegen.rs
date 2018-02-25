//!
//! Koak's IR generator
//!

use std::rc::Rc;
use std::collections::HashMap;

use llvm_sys::prelude::LLVMValueRef;

use iron_llvm::target;
use iron_llvm::core;
use iron_llvm::core::value::FunctionRef;
use iron_llvm::core::Function;
use iron_llvm::core::basic_block::BasicBlock;

use lexer::Token;
use error::{SyntaxError, ErrorReason};
use lang::function::FunctionPrototype;
use lang::value::KoakValue;

///
/// Structure holding LLVM's stuff, used when generating IR code.
///
pub struct IRContext {
    pub context: core::Context,
    pub builder: core::Builder,
    pub functions: HashMap<Rc<String>, Rc<FunctionPrototype>>,
    scopes: Vec<HashMap<Rc<String>, KoakValue>>, // of local variables
    pub toplevel: bool,
    pub cli: bool,
}

impl IRContext {
    #[inline]
    pub fn new(cli: bool) -> IRContext {
        IRContext {
            context: core::Context::get_global(),
            builder: core::Builder::new(),
            functions: HashMap::new(),
            scopes: vec![HashMap::new()], // Push global scope
            toplevel: false,
            cli: cli,
        }
    }

    pub fn create_stack_var(&mut self, function: &FunctionRef, name: Rc<String>, value: KoakValue) {
        let mut builder = core::Builder::new();
        let mut bb = function.get_entry();
        let fi = bb.get_first_instruction();

        builder.position(&mut bb, &fi);
        let val_ptr = builder.build_alloca(value.ty.as_llvm_ref(), &name);
        self.builder.build_store(value.llvm_ref, val_ptr);

        let mut koak_val = value.copy_and_reassign(val_ptr);
        koak_val.name = Some(name.clone());
        self.scopes.last_mut().unwrap().insert(name, koak_val);
    }

    // new_val's type must be valid
    pub fn update_local_var(&mut self, token: &Token, name: Rc<String>, new_val: LLVMValueRef) -> IRExprResult {
        for scope in self.scopes.iter().rev() {
            if let Some(val_ptr) = scope.get(&name) {
                self.builder.build_store(new_val, val_ptr.llvm_ref);
                return Ok(val_ptr.copy_and_reassign(new_val));
            }
        }
        Err(SyntaxError::from(token, ErrorReason::UndefinedVariable(name.to_string())))
    }

    pub fn get_local_var(&mut self, name: &String) -> Option<KoakValue> {
        for scope in self.scopes.iter().rev() {
            if let Some(val_ptr) = scope.get(name) {
                return Some(val_ptr.copy_and_reassign(self.builder.build_load(val_ptr.llvm_ref, name)));
            }
        }
        None
    }

    #[inline]
    pub fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    #[inline]
    pub fn pop_scope(&mut self) {
        self.scopes.pop();
    }
}

///
/// Trait that each language element must provide to generate their corresponding IR code.
///
///
pub type IRFuncResult = Result<LLVMValueRef, SyntaxError>;
pub type IRExprResult = Result<KoakValue, SyntaxError>;

pub trait IRFuncGenerator {
    fn gen_ir(&self, context: &mut IRContext, module_provider: &mut IRModuleProvider) -> IRFuncResult;
}

pub trait IRExprGenerator {
    fn gen_ir(&self, context: &mut IRContext, module_provider: &mut IRModuleProvider) -> IRExprResult;
}

///
/// Trait that all module providers must implement (JIT, ASM etc.)
///
pub trait IRModuleProvider {
    fn dump(&self);
    fn get_module(&mut self) -> &mut core::Module;
    fn get_llvm_funcref_by_name(&mut self, name: &str) -> Option<(FunctionRef, bool)>;
    fn get_pass_manager(&mut self) -> &mut core::FunctionPassManager;
}

pub struct SimpleModuleProvider {
    module: core::Module,
    pass_manager: core::FunctionPassManager,
}

impl SimpleModuleProvider {
    pub fn from(name: &str, optimizations: bool) -> SimpleModuleProvider {
        target::initilalize_native_target();
        target::initilalize_native_asm_printer();

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

    fn get_llvm_funcref_by_name(&mut self, name: &str) -> Option<(FunctionRef, bool)> {
        self.module.get_function_by_name(name).map(|e| (e, e.count_basic_blocks() > 0))
    }

    fn get_pass_manager(&mut self) -> &mut core::FunctionPassManager {
        &mut self.pass_manager
    }
}

///
/// Returns a pass manager depending on the given parameters
///
pub fn get_pass_manager(m: &core::Module, optimizations: bool) -> core::FunctionPassManager {
    let mut pm = core::FunctionPassManager::new(m);
    if optimizations {
        pm.add_basic_alias_analysis_pass();
        pm.add_instruction_combining_pass();
        pm.add_reassociate_pass();
        pm.add_GVN_pass();
        pm.add_CFG_simplification_pass();
        pm.add_dead_store_elimination_pass();
        pm.add_promote_memory_to_register_pass();
    }
    pm.initialize();
    pm
}
