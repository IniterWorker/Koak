//!
//! File input source
//!

use std::io;
use std::rc::Rc;
use std::io::{BufRead, BufReader};
use std::fs;
use std::fs::File;

pub struct FileSourceInput {
    bufreader: BufReader<File>,
    pub path: String,
    pub canon_path: String,
    pub dir_path: String,
}

impl FileSourceInput {
    #[inline]
    pub fn open(path: &str) -> Result<FileSourceInput, io::Error> {
        let mut canon = fs::canonicalize(path)?;
        let canon_path = canon.clone().into_os_string().into_string().unwrap();
        canon.pop();
        let dir_path = canon.into_os_string().into_string().unwrap();

        File::open(path).map(|file| {
            FileSourceInput {
                bufreader: BufReader::new(file),
                path: path.to_string(),
                canon_path: canon_path,
                dir_path: dir_path,
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
