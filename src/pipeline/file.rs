//!
//! Koak's file-to-assembly pipeline.
//!

use args::Args;
use lexer::Lexer;
use parser::Parser;
use error::print_errors;
use input::FileSourceInput;
use codegen::{SimpleModuleProvider, IRModuleProvider, IRContext, IRGenerator};

use super::print_vec;
use super::module;

pub struct FilePipeline<'a> {
    input: FileSourceInput,
    args: &'a Args,
}

impl<'a> FilePipeline<'a> {
    #[inline]
    pub fn new(input: FileSourceInput, args: &'a Args) -> FilePipeline {
        FilePipeline {
            input: input,
            args: args,
        }
    }

    ///
    /// Runs the Koak's whole pipeline.
    ///
    pub fn run(&mut self) -> bool {
        let mut context = IRContext::new();
        let mut module_provider = SimpleModuleProvider::from(&self.input.path, self.args.optimization);
        let mut module_manager = module::ModuleManager::new();

        // Vector of errors
        let mut errors = Vec::new();
        let mut tokens = Vec::new();
        let mut ast = Vec::new();
        let mut ir = Vec::new();

        // Iterate on lines
        for (row, line) in (&mut self.input).enumerate() {
            for r in Lexer::new(&line, row + 1) { // Lexe input
                match r {
                    Ok(token) => tokens.push(token),
                    Err(se) => errors.push(se),
                }
            }
        }

        if self.args.stop_after_lexer {
            if errors.len() != 0 {
                print_errors(self.args, &errors);
            } else {
                print_vec(&tokens);
            }
            return errors.len() != 0;
        }

        for r in Parser::new(&mut module_manager, tokens) {
            match r {
                Ok(mut nodes) => ast.append(&mut nodes),
                Err(mut ses) => errors.append(&mut ses),
            }
        }

        if self.args.stop_after_parser {
            if errors.len() != 0 {
                print_errors(self.args, &errors);
            } else {
                print_vec(&ast);
            }
            return errors.len() != 0;
        }

        for r in ast.iter() {
            match r.gen_ir(&mut context, &mut module_provider) {
                Ok(i) => ir.push(i),
                Err(se) => errors.push(se),
            }
        }

        if errors.len() != 0 {
            print_errors(self.args, &errors);
            true
        } else {
            module_provider.dump();
            false
        }
    }
}
