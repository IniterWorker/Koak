//!
//! Input feeder
//!

use std::fs::File;
use std::io::{BufReader, BufRead, Lines};
use std::io;
use std::fmt::Debug;

use rustyline::Editor;

pub trait InputFeeder: Debug {
    fn next_line(&mut self) -> Option<String>;
    fn get_name(&self) -> String;
    fn should_continue(&mut self) -> bool;
}

#[derive(Debug)]
pub struct FileInputFeeder {
    path: String,
    lines: Lines<BufReader<File>>,
}

impl FileInputFeeder {
    pub fn from(path: &str) -> Result<Box<InputFeeder>, (String, io::Error)> {
        File::open(path).map(|file| {
            Box::new(FileInputFeeder {
                path: String::from(path),
                lines: BufReader::new(file).lines(),
            }) as Box<InputFeeder>
        }).map_err(|e| (String::from(path), e))
    }
}

impl InputFeeder for FileInputFeeder {
    fn get_name(&self) -> String {
        self.path.clone()
    }

    fn next_line(&mut self) -> Option<String> {
        match self.lines.next() {
            Some(Ok(l)) => Some(l),
            _ => {
                None
            },
        }
    }

    fn should_continue(&mut self) -> bool {
        false
    }
}

#[derive(Debug)]
pub struct LineInputFeeder {
    rl: Editor<()>,
    fake_eof: bool,
    eof: bool
}

impl LineInputFeeder {
    pub fn new() -> Result<Box<InputFeeder>, (String, io::Error)> {
        Ok(Box::new(LineInputFeeder {
            rl: Editor::<()>::new(),
            fake_eof: false,
            eof: false,
        }) as Box<InputFeeder>)
    }
}

impl InputFeeder for LineInputFeeder {
    fn get_name(&self) -> String {
        String::from("stdin")
    }

    fn next_line(&mut self) -> Option<String> {
        if !self.fake_eof {
            self.fake_eof = true;
            match self.rl.readline(">> ") {
                Ok(line) => Some(line),
                Err(_) => {
                    self.eof = true;
                    None
                }
            }
        }
        else {
            None
        }
    }

    fn should_continue(&mut self) -> bool {
        self.fake_eof = false;
        !self.eof
    }
}
