//!
//! File input source
//!

use std::io;
use std::rc::Rc;
use std::io::{BufRead, BufReader};
use std::fs::File;

use super::SourceInput;

pub struct FileSourceInput {
    bufreader: BufReader<File>,
    path: String,
}

impl FileSourceInput {
    #[inline]
    pub fn open(path: &str) -> Result<FileSourceInput, io::Error> {
        File::open(path).map(|file| {
            FileSourceInput {
                bufreader: BufReader::new(file),
                path: path.to_string(),
            }
        })
    }
}

impl Iterator for FileSourceInput {
    type Item = Rc<String>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut line = String::new();
        match self.bufreader.read_line(&mut line) {
            Ok(c) => {
                if c != 0 {
                    if line.as_bytes().last() == Some(&('\n' as u8)) { // Remove last '\n'
                        line.pop();
                    }
                    Some(Rc::new(line))
                } else {
                    None
                }
            }
            Err(_) => None,
        }
    }
}

impl SourceInput for FileSourceInput {
    #[inline]
    fn get_name(&self) -> &str {
        &self.path
    }
}
