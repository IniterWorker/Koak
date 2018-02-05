//!
//! Koak's input feeder based on stdin.
//!

use std::str::Chars;
use std::iter::Peekable;

use rustyline::Editor;

use super::{InputFeeder, InputFeederIterator};

#[derive(Debug)]
pub struct StdinInput {
    rl: Editor<()>,
    line: String,
    row: usize,
}

impl StdinInput {
    pub fn new() -> StdinInput {
        StdinInput {
            rl: Editor::<()>::new(),
            line: String::new(),
            row: 0,
        }
    }

    pub fn prompt(&mut self) -> bool {
        match self.rl.readline(">> ") {
            Ok(line) => {
                self.line = line;
                self.row += 1;
                false
            }
            Err(_) => true,
        }
    }

    pub fn get_row(&self) -> usize {
        self.row
    }
}

impl InputFeeder for StdinInput {
    fn get_name(&self) -> &str {
        "stdin"
    }

    fn get_line(&self) -> &str {
        &self.line
    }
}

pub struct StdinInputIterator<'a> {
    chars: Peekable<Chars<'a>>,
    col: usize,
    row: usize,
}

impl<'a> StdinInputIterator<'a> {
    #[inline]
    pub fn from(line: &str, row: usize) -> StdinInputIterator {
        StdinInputIterator {
            chars: line.chars().peekable(),
            col: 0,
            row: row,
        }
    }
}

impl<'a> Iterator for StdinInputIterator<'a> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(c) = self.chars.next() {
            self.col += 1;
            Some(c)
        } else {
            None
        }
    }
}

impl<'a> InputFeederIterator for StdinInputIterator<'a> {
    #[inline]
    fn get_row(&self) -> usize {
        self.row
    }
    #[inline]
    fn get_col(&self) -> usize {
        self.col
    }

    #[inline]
    fn peek(&mut self) -> Option<Self::Item> {
        self.chars.peek().map(|x| *x)
    }
}
