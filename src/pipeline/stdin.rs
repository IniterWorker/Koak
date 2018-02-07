//!
//! Standard Input Koak's Toolchain
//!

use lexer::Lexer;
use parser::Parser;
use codegen::{Context, SimpleModuleProvider};
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
        use iron_llvm::core::Value;

        let mut context = Context::new(self.stdin.get_name());
        let mut module_provider = SimpleModuleProvider::from("main");
        loop {
            if self.stdin.prompt() {
                break;
            }
            let mut errors = Vec::new();
            let mut tokens = Vec::new();
            let mut nodes = Vec::new();
            let mut ir = Vec::new();
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

            for node in nodes {
                use codegen::IRBuilder;
                match node.gen_ir(&mut context, &mut module_provider) {
                    Ok(i) => ir.push(i),
                    Err(se) => errors.push(se),
                }
            }

            if errors.len() != 0 {
                for se in errors {
                    se.print_error();
                }
                continue
            }

            for i in ir {
                i.dump();
            }
        }
    }
}

