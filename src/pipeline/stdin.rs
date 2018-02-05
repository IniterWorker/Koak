//!
//! Standard Input Koak's Toolchain
//!

use lexer::Lexer;
use parser::Parser;
use input::{InputFeeder, StdinInput, StdinInputIterator};
use super::KoakPipeLine;

pub struct StdinKoakPipeLine {
    stdin: StdinInput,
}

impl StdinKoakPipeLine {
    pub fn new() -> StdinKoakPipeLine {
        StdinKoakPipeLine {
            stdin: StdinInput::new(),
        }
    }
}

impl KoakPipeLine for StdinKoakPipeLine {

    fn run(&mut self) {
        loop {
            if self.stdin.prompt() {
                break;
            }
            let mut errors = Vec::new();
            let mut tokens = Vec::new();
            let mut nodes = Vec::new();
            {
                let line = self.stdin.get_line().to_string();
                let it = StdinInputIterator::from(&line, self.stdin.get_row());
                let lexer = Lexer::from(&mut self.stdin, it);

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
                    self.stdin.get_name()
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
                continue
            }

            for node in nodes {
                println!("{:?}", node);
            }
        }
    }
}

