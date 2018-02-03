//!
//! Standard Input Koak's Toolchain
//!

use std::io;

use lexer::Lexer;
use parser::Parser;
use input::{InputFeeder, LineInputFeeder};
use super::KoakToolChain;

pub struct StdinKoakToolChain {
    parser: Parser,
}

impl StdinKoakToolChain {
    pub fn new() -> Result<StdinKoakToolChain, (String, io::Error)> {
        Ok(StdinKoakToolChain {
            parser: Parser::new(Lexer::from(LineInputFeeder::new()?)),
        })
    }

    fn get_input_mut(&mut self) -> &mut Box<InputFeeder> {
        self.get_lexer_mut().get_feeder_mut()
    }

    fn get_lexer_mut(&mut self) -> &mut Lexer {
        self.parser.get_lexer_mut()
    }

}

impl KoakToolChain for StdinKoakToolChain {
    fn run(&mut self) {
        loop {
            self.get_lexer_mut().reset();
            while let Some(res) = self.parser.next() {
                match res {
                    Ok(node) => println!("Node: {:?}", node),
                    Err(se) => {
                        se.print_error();
                        break;
                    },
                }
            }
            if !self.get_input_mut().should_continue() {
                break
            }
        }
    }
}

