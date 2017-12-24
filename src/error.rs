//!
//! Compiler error
//!

use ansi_term::Colour::*;
use pos::Pos;

#[derive(Debug)]
pub struct SyntaxError {
    line: String,
    start: Pos,
    end: Pos,
    what: String,
}

impl SyntaxError {
    pub fn from(line: String, start: Pos, end: Pos, what: String) -> SyntaxError {
        SyntaxError {
            line: line.clone(),
            start: start,
            end: end,
            what: what,
        }
    }

    pub fn print_error(&mut self) {
        println!("{}:{}:{}: {}: {}", "/dev/stdin", self.start.line, self.start.col, Red.bold().paint("Syntax Error"), self.what);
        println!("{}", self.line);
        for _ in 0..self.start.col - 1{
            print!(" ");
        }
        for _ in 0..self.end.col - self.start.col {
            print!("{}", Green.bold().paint("~"));
        }
        println!("{}", Green.bold().paint("^"));
    }
}

