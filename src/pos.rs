//!
//! A position within a file
//!

#[derive(Debug, Copy, Clone)]
pub struct Pos {
    pub line: usize,
    pub col: usize,
}

impl Pos {
    pub fn new(l: usize, c: usize) -> Pos {
        Pos {
            line: l,
            col: c,
        }
    }
}
