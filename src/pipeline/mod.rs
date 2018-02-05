//!
//! Koak's pipeline
//!

mod stdin;
mod file;

pub use self::stdin::StdinKoakPipeLine as StdinKoakPipeLine;
pub use self::file::FileKoakPipeLine as FileKoakPipeLine;

pub trait KoakPipeLine {
    fn run(&mut self);
}
