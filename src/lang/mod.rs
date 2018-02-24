//!
//! Koak's lang module
//!
//! All submodules implement a feature of the language. They both implement the parsing and the IR generation of it.
//!

pub mod expr;
pub mod function;
pub mod cond;
pub mod types;
pub mod for_loop;
pub mod while_loop;
pub mod block;
pub mod value;
pub mod anon;
