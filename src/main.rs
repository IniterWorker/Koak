//!
//! Koak's compiler main entry point.
//!

#![feature(io)]

extern crate ansi_term;
extern crate rustyline;

mod input;
mod lexer;
mod token;
mod pos;
mod syntaxerror;
mod parser;

fn main() {
    use lexer::Lexer;
    use parser::Parser;
    use input::{LineInputFeeder, FileInputFeeder};

    let feeder = match std::env::args().nth(1) {
        Some(ref filename) => FileInputFeeder::from(filename),
        _ => LineInputFeeder::new(),
    };

    match feeder {
        Ok(feeder) => {
            let mut lexer = Lexer::from(feeder);
            let mut parser = Parser::new();

            while {
                lexer.reset();
                match parser.parse(&mut lexer) {
                    Ok(_) => (),
                    Err(se) => {
                        se.print_error();
                        if !lexer.get_feeder().is_stdin() {
                            std::process::exit(1);
                        }
                    }
                }
                lexer.get_feeder_mut().should_continue()
            } {}
        },
        Err((f, s)) =>  {
            use std::error::Error;
            eprintln!("{}: {}", f, s.description());
        }
    }
}
