//!
//! Koak's stdin pipeline.
//!

use args::Args;
use lexer::Lexer;
use parser::Parser;
use error::print_errors;
use input::{SourceInput, FileSourceInput};
use codegen::{SimpleModuleProvider, IRModuleProvider, IRContext, IRGenerator};

use super::print_vec;

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
    pub fn run(&mut self) {
        let mut context = IRContext::new();
        let mut module_provider = SimpleModuleProvider::from(self.input.get_name(), self.args.optimization);

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
            print_errors(&errors);
            print_vec(&tokens);
            return;
        }

        for r in Parser::new(tokens) {
            match r {
                Ok(node) => ast.push(node),
                Err(se) => errors.push(se),
            }
        }

        if self.args.stop_after_parser {
            print_errors(&errors);
            print_vec(&ast);
            return;
        }

        for r in ast.iter() {
            match r.gen_ir(&mut context, &mut module_provider) {
                Ok(i) => ir.push(i),
                Err(se) => errors.push(se),
            }
        }

        if errors.len() != 0 {
            print_errors(&errors);
        } else {
            module_provider.dump();
        }
    }
}
