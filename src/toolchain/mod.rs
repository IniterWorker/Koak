//!
//! Koak's toolchain
//!

mod stdin;
mod file;

pub use self::stdin::StdinKoakToolChain as StdinKoakToolChain;
pub use self::file::FileKoakToolChain as FileKoakToolChain;

pub trait KoakToolChain {
    fn run(&mut self);
}
