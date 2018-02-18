//!
//! Koak's pipeline.
//!

mod stdin;
mod file;
pub mod module;

pub use self::stdin::StdinPipeline as StdinPipeline;
pub use self::file::FilePipeline as FilePipeline;

use std::fmt;

#[inline]
fn print_vec<T: fmt::Debug>(errors: &Vec<T>) {
    for e in errors {
        println!("{:?}", e);
    }
}
