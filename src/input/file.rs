//!
//! Koak's input feeder based on a file.
//!

use std::io;
use std::fs::File;
use std::str::Chars;
use std::iter::Peekable;
use std::io::{BufReader, BufRead, Lines};

use super::{InputFeeder, InputFeederIterator};

#[derive(Debug)]
pub struct FileInput {
    path: String,
    line: String,
    lines: Lines<BufReader<File>>,
    row: usize,
}

impl FileInput {
    pub fn from(path: &str) -> Result<FileInput, (String, io::Error)> {
        File::open(path).map(|file| {
            FileInput {
                path: String::from(path),
                line: String::new(),
                lines: BufReader::new(file).lines(),
                row: 0,
            }
        }).map_err(|e| (String::from(path), e))
    }

    pub fn new_line(&mut self) -> bool {
        if let Some(Ok(line)) = self.lines.next() {
            self.line = line;
            self.row += 1;
            true
        } else {
            false
        }
    }

    pub fn get_row(&self) -> usize {
        self.row
    }
}

impl InputFeeder for FileInput {
    fn get_name(&self) -> &str {
        &self.path
    }

    fn get_line(&self) -> &str {
        &self.line
    }
}

pub struct FileInputIterator<'a> {
    chars: Peekable<Chars<'a>>,
    col: usize,
    row: usize,
}

impl<'a> FileInputIterator<'a> {
    #[inline]
    pub fn from(line: &str, row: usize) -> FileInputIterator {
        FileInputIterator {
            chars: line.chars().peekable(),
            col: 0,
            row: row,
        }
    }
}

impl<'a> Iterator for FileInputIterator<'a> {
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

impl<'a> InputFeederIterator for FileInputIterator<'a> {
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
