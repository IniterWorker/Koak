//!
//! Koak's stdin pipeline.
//!

use iron_llvm::core::Value;

use args::Args;
use lexer::Lexer;
use parser::{Parser, ASTNode};
use error::print_errors;
use input::{SourceInput, StdinSourceInput};
use codegen::{IRContext, IRGenerator};
use jit::JitModuleProvider;

use super::print_vec;
use super::module;

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
        let mut module_manager = module::ModuleManager::new();

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
                continue
            }

            // Generate IR
            let mut toplevel_exprs = Vec::new(); // The top-level expressions to be executed by the JIT execution engine.
            for r in ast.iter() {
                match r.gen_ir(&mut context, &mut module_provider) {
                    Ok(i) => {
                        if let &ASTNode::TopLevelExpr(_) = r {
                            module_provider.close_current_module(); // Close current module and save expression
                            toplevel_exprs.push(i);
                        }
                        irs.push(i);
                    },
                    Err(se) => errors.push(se),
                }
            }

            // Print errors or dump code and execute top-level exprsessions.
            if errors.len() != 0 {
                print_errors(self.args, &errors);
            } else {
                for ir in irs {
                    ir.dump();
                }
                for func in toplevel_exprs {
                    println!("=> {}", module_provider.run_function(func));
                }
            }
        }
    }
}
