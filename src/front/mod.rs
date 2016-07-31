//! # The `.gib` Syntax
//!
//! Since there is only a single frontend at the moment the input language is defined by it. Head
//! on over to the [parser's source](../../src/cleaver/src/front/pest/mod.rs.html) which holds the
//! grammar. All of this is subject to change.

pub mod ast;
pub mod pest;

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

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

impl Default for Position {
    fn default() -> Position {
        Position { line: 1, col: 1 }
    }
}

impl From<(usize, usize)> for Position {
    fn from((line, col): (usize, usize)) -> Position {
        Position { line: line, col: col }
    }
}

#[derive(PartialEq, Debug)]
pub struct SymbolTable {
    parent: Option<Rc<RefCell<SymbolTable>>>,
    vars: HashMap<String, Rc<ast::Variable>>,
}

impl SymbolTable {
    fn new(parent: Option<Rc<RefCell<SymbolTable>>>) -> SymbolTable {
        SymbolTable {
            parent: parent,
            vars: HashMap::new(),
        }
    }

    fn lookup(&self, var: &String) -> Option<Rc<ast::Variable>> {
        if let Some(v) = self.vars.get(var) {
            return Some(v.clone());
        }
        match self.parent {
            Some(ref p) => p.borrow().lookup(var),
            None => None,
        }
    }
}
