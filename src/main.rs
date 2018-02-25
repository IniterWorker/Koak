//!
//! Kind Of Alternative Kaleidoscope
//!

#![feature(catch_expr)]

#[macro_use]
extern crate lazy_static;
extern crate rustyline;
extern crate ansi_term;
extern crate getopts;
extern crate llvm_sys;
extern crate iron_llvm;
extern crate libc;

use std::process::exit;

mod args;
mod pipeline;
mod input;
mod lexer;
mod error;
mod parser;
mod lang;
mod codegen;
mod jit;

use pipeline::{FilePipeline, StdinPipeline, ObjectPipeline};
use input::FileSourceInput;
use args::Args;

fn main() {
    // Parse args
    let args = Args::parse_args();

    // Pipeline depends on arguments
    if args.input.is_empty() {
        let mut pipeline = StdinPipeline::new(&args);
        pipeline.run();
    } else {
        let mut op = ObjectPipeline::new(&args);
        for (i, file) in args.input.iter().enumerate() {
            let fsi = match FileSourceInput::open(file) {
                Ok(fsi) => fsi,
                Err(e) => {
                    eprintln!("{}: {}", file, e);
                    exit(1);
                },
            };
            let mut pipeline = FilePipeline::new(fsi, &args, i == args.input.len() - 1);
            if let Some(m) = pipeline.run() {
                op.modules.push((pipeline.input.canon_path, m));
            } else {
                exit(1);
            }
        }
        unsafe { op.run(); }
    }
}
