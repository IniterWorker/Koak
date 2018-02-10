//!
//! Koak's stdin pipeline.
//!

use args::Args;
use lexer::Lexer;
use parser::Parser;
use error::print_errors;
use input::{SourceInput, StdinSourceInput};
use codegen::{SimpleModuleProvider, IRModuleProvider, IRContext, IRGenerator};

use iron_llvm::core::Value;

use super::print_vec;

pub struct StdinPipeline<'a> {
    input: StdinSourceInput,
    args: &'a Args,
}

impl<'a> StdinPipeline<'a> {
    #[inline]
    pub fn new(args: &'a Args) -> StdinPipeline {
        StdinPipeline {
            input: StdinSourceInput::new(),
            args: args,
        }
    }

    ///
    /// Runs the Koak's whole pipeline.
    ///
    pub fn run(&mut self) {
        let mut context = IRContext::new();
        let mut module_provider = SimpleModuleProvider::from(self.input.get_name(), self.args.optimization);

        // Iterate on stdin
        for (row, line) in (&mut self.input).enumerate() {
            let mut errors = Vec::new();
            let mut tokens = Vec::new();
            let mut ast = Vec::new();

            for r in Lexer::new(&line, row + 1) { // Lexe input
                match r {
                    Ok(token) => tokens.push(token),
                    Err(se) => errors.push(se),
                }
            }

            if self.args.stop_after_lexer {
                print_errors(&errors);
                print_vec(&tokens);
                continue
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
                continue
            }

            // Generate IR
            for r in ast.iter() {
                match r.gen_ir(&mut context, &mut module_provider) {
                    Ok(i) => i.dump(),
                    Err(se) => errors.push(se),
                }
            }

            print_errors(&errors);
        }
        module_provider.dump();
    }
}
