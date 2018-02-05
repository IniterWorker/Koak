//!
//! Input feeder
//!

use std::fmt::Debug;

mod file;
mod stdin;

pub use self::stdin::StdinInput as StdinInput;
pub use self::stdin::StdinInputIterator as StdinInputIterator;

pub use self::file::FileInput as FileInput;
pub use self::file::FileInputIterator as FileInputIterator;

pub trait InputFeeder: Debug {
    fn get_name(&self) -> &str;
    fn get_line(&self) -> &str;
}

pub trait InputFeederIterator: Iterator<Item=char> {
    fn get_row(&self) -> usize;
    fn get_col(&self) -> usize;
    fn peek(&mut self) -> Option<Self::Item>;
}
