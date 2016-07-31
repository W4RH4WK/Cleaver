//! # The `.gib` Syntax
//!
//! Input files to Cleaver follow the grammar described here and are called gibs (singular: gib).
//! Expression written between slashes (`/`) are regular expressions (Javascript). Tokens are
//! written in double-quotes (`"`) and rules between angular brackets (`<`, `>`). Something between
//! brackets (`[`, `]`) is considered optional while something between braces (`{`, `}`) can occur
//! multiple times or not at all.
//!
//! ```text
//!     <identifier>        ::= /[A-Za-z][A-Za-z0-9_]*/
//!
//!     <bool_literal>      ::= "true" | "false"
//!
//!     <int_literal>       ::= /[0-9]+/
//!
//!     <float_literal>     ::= /[0-9]+\.[0-9]+/
//!
//!     <string_literal>    ::= /"[^"]"/
//!
//!     <type>              ::= "void" | "bool" | "int" | "float" | "string"
//!
//!     <un_op>             ::= "!" | "-"
//!
//!     <bin_op>            ::= "==" | "<" | "<=" | "+" | "-" | "*" | "/"
//!
//!     <variable>          ::= <identifier>
//!
//!     <expression>        ::= <bool_literal>
//!                           | <int_literal>
//!                           | <float_literal>
//!                           | <string_literal>
//!                           | <variable>
//!                           | "(" <expression> ")"
//!                           | <call_expression>
//!                           | <un_op> <expression>
//!                           | <expression> <bin_op> <expression>
//!
//!     <call_expression>   ::= <identifier> "(" [ <expression> { "," <expression> } ] ")"
//!
//!     <statement>         ::= <expression> ";"
//!                           | <declaration> ";"
//!                           | <assignment> ";"
//!                           | <if_statement>
//!                           | <while_statement>
//!                           | <return> ";"
//!                           | "{" { <statement } "}"
//!
//!     <declaration>       ::= <type> <variable>
//!
//!     <assignment>        ::= <variable> "=" <expression>
//!
//!     <if_statement>      ::= "if" "(" <expression> ")" <statement> [ "else" <statement> ]
//!
//!     <while_statement>   ::= "while" "(" <expression> ")" <statement>
//!
//!     <program>           ::= { <function> }
//!
//!     <function>          ::= <type> <identifier> "("
//!                                 [ <declaration> { "," <declaration> } ] ")" <statement>
//! ```

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
