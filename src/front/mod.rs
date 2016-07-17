pub mod ast;
pub mod manual_parser;

#[derive(PartialEq, Copy, Clone, Debug)]
pub struct Position {
    line: usize,
    col: usize,
}

impl Position {
    pub fn to_pair(&self) -> (usize, usize) {
        (self.line, self.col)
    }
}
