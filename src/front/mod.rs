use std::fmt;
use std::rc::Rc;

pub mod ast;
pub mod manual_parser;

#[derive(PartialEq, Clone, Debug)]
pub struct Position {
    file: Rc<String>,
    line: usize,
    col: usize,
}

impl Position {
    pub fn new(file: Rc<String>) -> Position {
        Position {
            file: file,
            line: 1,
            col: 1,
        }
    }

    pub fn to_pair(&self) -> (usize, usize) {
        (self.line, self.col)
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}.{}", self.file, self.line, self.col)
    }
}
