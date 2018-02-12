//!
//! Koak's stdin pipeline.
//!

use args::Args;
use lexer::Lexer;
use parser::{Parser, ASTNode};
use error::print_errors;
use input::{SourceInput, StdinSourceInput};
use codegen::{IRContext, IRGenerator};
use jit::JitModuleProvider;

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
        let mut module_provider = JitModuleProvider::from(self.input.get_name(), self.args.optimization);

        // Iterate on stdin
        for (row, line) in (&mut self.input).enumerate() {
            let mut errors = Vec::new();
            let mut tokens = Vec::new();
            let mut ast = Vec::new();
            let mut irs = Vec::new();

            for r in Lexer::new(&line, row + 1) { // Lexe input
                match r {
                    Ok(token) => tokens.push(token),
                    Err(se) => errors.push(se),
                }
            }

            if self.args.stop_after_lexer {
                if errors.len() != 0 {
                    print_errors(self.args, &errors);
                } else {
                    print_vec(&tokens);
                }
                continue
            }

            for r in Parser::new(tokens) {
                match r {
                    Ok(node) => ast.push(node),
                    Err(se) => errors.push(se),
                }
            }

            if self.args.stop_after_parser {
                if errors.len() != 0 {
                    print_errors(self.args, &errors);
                } else {
                    print_vec(&ast);
                }
                continue
            }

            // Generate IR
            for r in ast.iter() {
                match r.gen_ir(&mut context, &mut module_provider) {
                    Ok(i) => {
                        if let &ASTNode::TopLevelExpr(_) = r { // Exec top level expressions
                            if errors.len() == 0 { // Don't exec if errors
                                println!("=> {}", module_provider.run_function(i));
                            }
                        } else {
                            irs.push(i); // Append IR only when non-top level expression
                        }
                    },
                    Err(se) => errors.push(se),
                }
            }

            // Print errors or generated code
            if errors.len() != 0 {
                print_errors(self.args, &errors);
            } else {
                for ir in irs {
                    ir.dump();
                }
            }
        }
    }
}
