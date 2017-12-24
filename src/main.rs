//!
//! Koak's compiler main entry point.
//!

#![feature(io)]

extern crate ansi_term;

mod input;
mod lexer;
mod token;
mod pos;
mod error;

fn main() {
    use lexer::Lexer;
    use token::TokenType;
    use error::SyntaxError;
    use input::InputFeeder;

    let filename = std::env::args().nth(1).unwrap_or(String::from("/dev/stdin"));

    match InputFeeder::from(&filename) {
        Ok(feeder) => {
            let mut lexer = Lexer::from(feeder);
            while let Some(token) = lexer.next() {
                match token.get_type() {
                    TokenType::Error(what) => {
                        SyntaxError::from(lexer.get_current_line(), token.get_start(), token.get_end(), what).print_error();
                        std::process::exit(1);
                    },
                    _ => println!("{:?}", token),
                }
            }
        },
        Err(s) =>  {
            use std::error::Error;
            eprintln!("{}: {}", filename, s.description());
        }
    }
}
