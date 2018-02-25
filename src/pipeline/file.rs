//!
//! Koak's file-to-assembly pipeline.
//!

use std::env;

use iron_llvm::core::Module;

use args::Args;
use error::print_errors;
use input::FileSourceInput;
use codegen::{SimpleModuleProvider, IRModuleProvider};
use parser::ASTNode;

use super::Pipeline;

pub struct FilePipeline<'a> {
    pub input: FileSourceInput,
    pub args: &'a Args,
    pub last: bool,
}

impl<'a> FilePipeline<'a> {
    #[inline]
    pub fn new(input: FileSourceInput, args: &'a Args, last: bool) -> FilePipeline {
        FilePipeline {
            input: input,
            args: args,
            last: last,
        }
    }

    ///
    /// Runs the Koak's whole pipeline.
    ///
    pub fn run(&mut self) -> Option<Module> {
        let mut pipeline = Pipeline::new(self.args, false, SimpleModuleProvider::from(&self.input.path, self.args.optimization));

        // Cd to target directory
        let cwd = env::current_dir().unwrap();
        assert!(env::set_current_dir(self.input.dir_path.clone()).is_ok()); // Switch current directory to the module's one.

        // Pre-load modules
        pipeline.preload_modules(|_| {});

        // iterate on stdin
        let mut tokens = Vec::new();
        for (row, line) in (&mut self.input).enumerate() {
            tokens.append(&mut pipeline.run_lexer(&line, row));
        }

        if self.args.stop_after_lexer {
            pipeline.print_errors_or_vec(&tokens);
            return None
        }

        let mut nodes = pipeline.run_parser(tokens);
        if self.args.stop_after_parser {
            pipeline.print_errors_or_vec(&nodes);
            return None
        }

        // Drain top level expressions
        let mut i = 0;
        while i != nodes.len() {
            if let ASTNode::TopLevelExpr(_) = nodes[i] {
                let node = nodes.remove(i);
                if let ASTNode::TopLevelExpr(em) = node {
                    pipeline.main.exprs.push(em);
                }
            } else {
                i += 1;
            }
        }

        pipeline.generate_ir(nodes, |_| {});

        // If last, gen main
        if self.last {
            pipeline.gen_main();
        }

        // Go back to old dir
        assert!(env::set_current_dir(cwd).is_ok());
        // print errors or dump code
        if pipeline.errors.is_empty() {
            Some(pipeline.module_provider.get_module().clone())
        } else {
            print_errors(self.args, &pipeline.errors);
            None
        }
    }
}
