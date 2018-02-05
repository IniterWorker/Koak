//!
//! File Input Koak's Toolchain
//!

use std::io;
use std::process::exit;

use lexer::Lexer;
use parser::Parser;
use input::{InputFeeder, FileInput, FileInputIterator};
use super::KoakPipeLine;

pub struct FileKoakPipeLine {
    input: FileInput,
}

impl FileKoakPipeLine {
    pub fn from(path: &str) -> Result<FileKoakPipeLine, (String, io::Error)> {
        Ok(FileKoakPipeLine {
            input: FileInput::from(path)?,
        })
    }
}

impl KoakPipeLine for FileKoakPipeLine {
    fn run(&mut self) {
        let mut errors = Vec::new();
        let mut tokens = Vec::new();
        let mut nodes = Vec::new();

        while self.input.new_line() {
            let line = self.input.get_line().to_string();
            let it = FileInputIterator::from(&line, self.input.get_row());
            let lexer = Lexer::from(&mut self.input, it);

            for r in lexer {
                match r {
                    Ok(t) => tokens.push(t),
                    Err(se) => errors.push(se),
                }
            }
        }

        let parser = Parser::from(
                tokens.iter().peekable(),
                &tokens,
                self.input.get_name()
        );

        for r in parser {
            match r {
               Ok(n) => nodes.push(n),
               Err(se) => errors.push(se),
            }
        }

        if errors.len() != 0 {
            for se in errors {
                se.print_error();
            }
            exit(1);
        }

        for node in nodes {
            println!("{:?}", node);
        }
    }
}
