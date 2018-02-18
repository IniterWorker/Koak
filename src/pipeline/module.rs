//!
//! Koak's module system
//!

use std::env;

use input::FileSourceInput;
use parser::{ASTNode, Parser};
use error::{SyntaxError, ErrorReason};
use lexer::{Token, Lexer};

pub struct ModuleManager {
    pub loaded_modules: Vec<String>,
}

impl ModuleManager {
    pub fn new() -> ModuleManager {
        ModuleManager {
            loaded_modules: Vec::new(),
        }
    }
}

fn run_module(mm: &mut ModuleManager, input: &mut FileSourceInput) -> Result<Vec<ASTNode>, Vec<SyntaxError>> {
    let mut tokens = Vec::new();
    let mut ast = Vec::new();
    let mut errors = Vec::new();

    // Iterate on lines
    for (row, line) in input.enumerate() {

        for r in Lexer::new(&line, row + 1) { // Lexe input
            match r {
                Ok(token) => tokens.push(token),
                Err(se) => errors.push(se),
            }
        }
    }

    for r in Parser::new(mm, tokens) {
        match r {
            Ok(mut nodes) => ast.append(&mut nodes),
            Err(mut ses) => errors.append(&mut ses),
        }
    }

    if errors.len() > 0 {
        Err(errors)
    } else {
        Ok(ast)
    }
}

///
/// Loads the given module and returns a vector of node representing it's content,
/// or a vector of syntax error if the module is not compiling properly.
///
pub fn load_module(mm: &mut ModuleManager, path_token: &Token, path: &str) -> Result<Vec<ASTNode>, Vec<SyntaxError>> {
    let new_path = path.to_string() + ".ks";
    let mut fsi = FileSourceInput::open(&new_path).map_err(|err| {
        vec![SyntaxError::from(path_token, ErrorReason::CantOpenModule(String::from(new_path), err.to_string()))]
    })?;

    if mm.loaded_modules.contains(&fsi.canon_path) { // Prevent recursive search
        return Ok(vec![]);
    }

    mm.loaded_modules.push(fsi.canon_path.clone());

    let cwd = env::current_dir().unwrap();
    assert!(env::set_current_dir(fsi.dir_path.clone()).is_ok()); // Switch current directory to the module's one.
    let r = run_module(mm, &mut fsi);
    assert!(env::set_current_dir(cwd).is_ok());

    r
}
