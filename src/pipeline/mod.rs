//!
//! Koak's pipeline.
//!

mod stdin;
mod file;
pub mod module;

pub use self::stdin::StdinPipeline as StdinPipeline;
pub use self::file::FilePipeline as FilePipeline;

use std::fmt;
use std::rc::Rc;

use llvm_sys::prelude::LLVMValueRef;
use iron_llvm::core::Value;

use args::Args;
use lexer::{Lexer, Token};
use parser::{Parser, ASTNode};
use error::{print_errors, SyntaxError};
use codegen::{IRContext, IRFuncGenerator, IRModuleProvider};
use jit::JitModuleProvider;
use self::module::ModuleManager;

pub struct Pipeline<'a, T: IRModuleProvider> {
    args: &'a Args,
    pub context: IRContext,
    pub module_provider: T,
    pub module_manager: ModuleManager,
    pub errors: Vec<SyntaxError>,
}

impl<'a, T: IRModuleProvider> Pipeline<'a, T> {
    pub fn new(args: &'a Args, mp: T) -> Pipeline<'a, T> {
        Pipeline {
            args: args,
            context: IRContext::new(),
            module_provider: mp,
            module_manager: ModuleManager::new(),
            errors: Vec::new(),
        }
    }

    pub fn run_lexer(&mut self, input: &Rc<String>, row: usize) -> Vec<Token> {
        let lexer = Lexer::new(input, row);
        let mut tokens = Vec::new();

        for r in lexer {
            match r {
                Ok(tok) => tokens.push(tok),
                Err(se) => self.errors.push(se),
            }
        }
        tokens
    }

    pub fn run_parser(&mut self, tokens: Vec<Token>) -> Vec<ASTNode> {
        let parser = Parser::new(&mut self.module_manager, tokens);
        let mut nodes= Vec::new();

        for r in parser {
            match r {
                Ok(mut node) => nodes.append(&mut node),
                Err(mut se) => self.errors.append(&mut se),
            }
        }
        nodes
    }

    pub fn generate_ir<F>(&mut self, nodes: Vec<ASTNode>, cb_toplevel_expr: F) -> (Vec<LLVMValueRef>, Vec<LLVMValueRef>)
        where F: Fn(&mut Self)
    {
        let mut irs = Vec::new();
        let mut expr = Vec::new();

        for r in nodes {
            match r.gen_ir(&mut self.context, &mut self.module_provider) {
                Ok(i) => {
                    if let ASTNode::TopLevelExpr(_) = r {
                        cb_toplevel_expr(self);
                        expr.push(i);
                    }
                    irs.push(i);
                },
                Err(se) => self.errors.push(se),
            }
        }
        (irs, expr)
    }

    pub fn preload_modules<F>(&mut self, cb_toplevel_expr: F) -> (Vec<LLVMValueRef>, Vec<LLVMValueRef>)
        where F: Fn(&mut Self)
    {
        let mut nodes = Vec::new();
        for path in &self.args.preloaded_modules {
            match module::load_module(&mut self.module_manager, &Token::new(), path) {
                Ok(mut n) => nodes.append(&mut n),
                Err(mut ses) => self.errors.append(&mut ses),
            }
        }
        self.generate_ir(nodes, cb_toplevel_expr)
    }

    pub fn print_errors_or_vec<E>(&self, vec: &[E])
        where E: fmt::Debug + Sized
    {
        print_errors(self.args, &self.errors);
        if self.errors.is_empty() {
            for e in vec {
                println!("{:?}", e);
            }
        }
    }
}

impl<'a> Pipeline<'a, JitModuleProvider> {
    ///
    /// Print errors or dump code and execute top-level exprsessions.
    ///
    pub fn print_errors_or_exec(&mut self, irs: Vec<LLVMValueRef>, exprs: Vec<LLVMValueRef>) {
            if self.errors.is_empty() {
                for ir in irs {
                    ir.dump();
                }
                for func in exprs {
                    self.module_provider.run_function(&self.context, func);
                }
            } else {
                print_errors(self.args, &self.errors);
            }
    }
}
