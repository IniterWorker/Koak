//!
//! Kind Of Alternative Kaleidoscope
//!

#[macro_use]
extern crate lazy_static;
extern crate rustyline;
extern crate ansi_term;
extern crate getopts;
extern crate llvm_sys;
extern crate iron_llvm;

use std::process::exit;

mod args;
mod pipeline;
mod input;
mod lexer;
mod error;
mod parser;
mod lang;
mod codegen;

use pipeline::{FilePipeline, StdinPipeline};
use input::FileSourceInput;
use args::Args;

fn main() {

    // Parse args
    let args = Args::parse_args();

    // Pipeline depends on arguments
    if args.input.len() == 0 {
        let mut pipeline = StdinPipeline::new(&args);
        pipeline.run();
    } else {
        for file in &args.input {
            let fsi = match FileSourceInput::open(&file) {
                Ok(fsi) => fsi,
                Err(e) => {
                    eprintln!("{}: {}", file, e);
                    exit(1);
                },
            };
            let mut pipeline = FilePipeline::new(fsi, &args);
            pipeline.run();
        }
    }
}
