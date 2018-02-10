//!
//! Stdin input source.
//!

use std::rc::Rc;

use super::SourceInput;

use rustyline::Editor;

pub struct StdinSourceInput {
    rl: Editor<()>,
}

impl StdinSourceInput {
    pub fn new() -> StdinSourceInput {
        StdinSourceInput {
            rl: Editor::<()>::new(),
        }
    }
}

impl Iterator for StdinSourceInput {
    type Item = Rc<String>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.rl.readline(">> ") {
            Ok(line) => {
                Some(Rc::new(line))
            }
            Err(_) => None
        }
    }
}

impl SourceInput for StdinSourceInput {
    fn get_name(&self) -> &str {
        "stdin"
    }
}
