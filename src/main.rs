//!
//! Koak's compiler main entry point.
//!

#![feature(io)]

extern crate ansi_term;
extern crate rustyline;
extern crate iron_llvm;
extern crate llvm_sys;

mod pipeline;
mod input;
mod lexer;
mod pos;
mod syntaxerror;
mod parser;
mod codegen;

fn main() {
    use std::process::exit;
    use pipeline::{KoakPipeLine, StdinKoakPipeLine, FileKoakPipeLine};

    let mut toolchain: Box<KoakPipeLine> = match std::env::args().nth(1) {
        Some(ref filename) => match FileKoakPipeLine::from(filename) {
            Ok(fktc) => Box::new(fktc),
            Err((filename, e)) => {
                eprintln!("{}: {}", filename, e);
                exit(1);
            },
        },
        _ => Box::new(StdinKoakPipeLine::new()),
    };

    toolchain.run();
}
