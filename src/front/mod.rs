//! # The `.gib` Syntax
//!
//! Since there is only a single frontend at the moment the input language is defined by it. Head
//! on over to the [parser's source](../../src/cleaver/src/front/pest/mod.rs.html) which holds the
//! grammar. All of this is subject to change.

pub mod ast;
pub mod pest;
pub mod symbols;

use std::collections::HashMap;
use std::path::Path;

pub fn process(filepaths: &Vec<&Path>) -> HashMap<String, ast::Node<ast::Function>> {
    // run parser
    let mut functions = pest::parse_files(filepaths);

    // symbolize everything
    for (_, ref mut f) in &mut functions {
        symbols::symbolize(f);
    }

    // TODO type checks

    // TODO semantic checks

    functions
}

#[derive(PartialEq, Copy, Clone, Debug)]
pub struct Position {
    pub line: usize,
    pub col: usize,
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

impl From<(usize, usize)> for Position {
    fn from((line, col): (usize, usize)) -> Position {
        Position {
            line: line,
            col: col,
        }
    }
}
