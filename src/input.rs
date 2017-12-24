//!
//! Input feeder
//!

use std::fs::File;
use std::io::{BufReader, BufRead, Lines};
use std::io;

#[derive(Debug)]
pub struct InputFeeder {
    path: String,
    lines: Lines<BufReader<File>>,
}

impl InputFeeder {
    pub fn from(path: &String) -> Result<InputFeeder, io::Error> {
        File::open(path).map(|file| {
            InputFeeder {
                path: path.clone(),
                lines: BufReader::new(file).lines(),
            }
        })
    }
}

impl Iterator for InputFeeder {
    type Item = String;

    fn next(&mut self) -> Option<String> {
        match self.lines.next() {
            Some(Ok(l)) => Some(l),
            _ => None,
        }
    }
}
