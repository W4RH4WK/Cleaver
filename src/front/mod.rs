//! # The `.gib` Syntax
//!
//! Since there is only a single frontend at the moment the input language is defined by it. Head
//! on over to the [parser's source](../../src/cleaver/src/front/pest/mod.rs.html) which holds the
//! grammar. All of this is subject to change.

pub mod ast;
pub mod pest;
pub mod symbols;

use std::collections::HashMap;
use std::fs::File;
use std::io::Read;
use std::path::Path;

pub fn parse_file(filepath: &Path) -> HashMap<String, ast::Node<ast::Function>> {
    // get file content
    let mut content = String::new();
    {
        let mut file = File::open(filepath).unwrap();
        file.read_to_string(&mut content).unwrap();
    }

    // run parser
    let mut functions = pest::parse(&content);

    for (_, f) in &mut functions {
        f.node.filename = filepath.file_name().unwrap().to_str().unwrap().to_owned();
    }

    functions
}

pub fn process_files(files: &Vec<&Path>) -> HashMap<String, ast::Node<ast::Function>> {
    let mut functions = HashMap::new();

    // parse all input files
    for file in files {
        for (name, function) in parse_file(file) {
            functions.insert(name, function);
        }
    }

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
