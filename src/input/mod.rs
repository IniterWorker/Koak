//!
//! Koak's input sources.
//!

mod stdin;
mod file;

pub use self::stdin::StdinSourceInput as StdinSourceInput;
pub use self::file::FileSourceInput as FileSourceInput;

use std::rc::Rc;

pub trait SourceInput: Iterator<Item=Rc<String>> {
    fn get_name(&self) -> &str;
}
