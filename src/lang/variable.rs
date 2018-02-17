//!
//! Koak's variables
//!

use std::rc::Rc;

use lang::value::Value;

///
/// Represents a named variable
///
pub struct Variable {
    pub name: Rc<String>,
    pub value: Box<Value>,
}

impl Variable {
    pub fn new(name: Rc<String>, value: Box<Value>) -> Variable {
        Variable {
            name: name,
            value: value,
        }
    }
}
