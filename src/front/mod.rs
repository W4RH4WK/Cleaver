pub mod ast;
pub mod rd_parser;

#[derive(PartialEq, Clone, Debug)]
pub struct Position {
    line: usize,
    col: usize,
}

impl Position {
    pub fn to_pair(&self) -> (usize, usize) {
        (self.line, self.col)
    }
}

impl Default for Position {
    fn default() -> Position {
        Position { line: 1, col: 1 }
    }
}
