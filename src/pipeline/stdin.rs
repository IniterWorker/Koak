//!
//! Koak's stdin pipeline.
//!

use llvm_sys::prelude::LLVMValueRef;

use iron_llvm::core::Value;


use args::Args;
use error::print_errors;
use input::{SourceInput, StdinSourceInput};
use jit::JitModuleProvider;
use codegen::IRModuleProvider;

use super::Pipeline;

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

    pub fn run(&mut self) {
        let mut pipeline = Pipeline::new(self.args, JitModuleProvider::from(self.input.get_name(), self.args.optimization));

        // Pre-load modules
        let (irs, exprs) = pipeline.preload_modules(|pipeline| {
            pipeline.module_provider.close_current_module();
        });
        pipeline.print_errors_or_exec(irs, exprs);

        // iterate on stdin
        for (row, line) in (&mut self.input).enumerate() {
            pipeline.errors.clear();

            let tokens = pipeline.run_lexer(&line, row);
            if self.args.stop_after_lexer {
                pipeline.print_errors_or_vec(&tokens);
                continue;
            }

            let nodes = pipeline.run_parser(tokens);
            if self.args.stop_after_parser {
                pipeline.print_errors_or_vec(&nodes);
                continue;
            }

            let (irs, exprs) = pipeline.generate_ir(nodes, |pipeline| {
                pipeline.module_provider.close_current_module();
            });

            pipeline.print_errors_or_exec(irs, exprs);
        }
    }
}
