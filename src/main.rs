//!
//! Koak's compiler main entry point.
//!

#![feature(io)]

extern crate ansi_term;
extern crate rustyline;
extern crate iron_llvm;
extern crate llvm_sys;

mod toolchain;
mod input;
mod lexer;
mod token;
mod pos;
mod syntaxerror;
mod parser;
mod codegen;

fn main() {
    use std::process::exit;
    use toolchain::{KoakToolChain, StdinKoakToolChain, FileKoakToolChain};

    let mut toolchain: Box<KoakToolChain> = match std::env::args().nth(1) {
        Some(ref filename) => match FileKoakToolChain::from(filename) {
            Ok(fktc) => Box::new(fktc),
            Err((filename, e)) => {
                eprintln!("{}: {}", filename, e);
                exit(1);
            },
        },
        _ => match StdinKoakToolChain::new() {
            Ok(fktc) => Box::new(fktc),
            Err((filename, e)) => {
                eprintln!("{}: {}", filename, e);
                exit(1);
            },
        },
    };

    toolchain.run();
}
