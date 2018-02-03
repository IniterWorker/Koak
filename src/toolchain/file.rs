//!
//! File Input Koak's Toolchain
//!

use std::io;
use std::process::exit;

use lexer::Lexer;
use parser::Parser;
use input::FileInputFeeder;
use super::KoakToolChain;

pub struct FileKoakToolChain {
    parser: Parser,
}

impl FileKoakToolChain {
    pub fn from(path: &str) -> Result<FileKoakToolChain, (String, io::Error)> {
        Ok(FileKoakToolChain {
            parser: Parser::new(Lexer::from(FileInputFeeder::from(path)?)),
        })
    }
}

impl KoakToolChain for FileKoakToolChain {
    fn run(&mut self) {
        while let Some(res) = self.parser.next() {
            match res {
                Ok(node) => println!("Node: {:?}", node),
                Err(se) => {
                    se.print_error();
                    exit(1);
                },
            }
        }
    }
}
