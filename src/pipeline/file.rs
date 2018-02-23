//!
//! Koak's file-to-assembly pipeline.
//!

use args::Args;
use error::print_errors;
use input::FileSourceInput;
use codegen::{SimpleModuleProvider, IRModuleProvider};

use super::Pipeline;

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
        let mut pipeline = Pipeline::new(self.args, SimpleModuleProvider::from(&self.input.path, self.args.optimization));

        // Pre-load modules
        pipeline.preload_modules(|_| {});

        // iterate on stdin
        let mut tokens = Vec::new();
        for (row, line) in (&mut self.input).enumerate() {
            tokens.append(&mut pipeline.run_lexer(&line, row));
        }

        if self.args.stop_after_lexer {
            pipeline.print_errors_or_vec(&tokens);
            return !pipeline.errors.is_empty();
        }

        let nodes = pipeline.run_parser(tokens);
        if self.args.stop_after_parser {
            pipeline.print_errors_or_vec(&nodes);
            return !pipeline.errors.is_empty();
        }

        pipeline.generate_ir(nodes, |_| {});

        // print errors or dump code
        if pipeline.errors.is_empty() {
            pipeline.module_provider.dump();
            false
        } else {
            print_errors(self.args, &pipeline.errors);
            true
        }
    }
}
