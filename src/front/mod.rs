//! # The `.gib` Syntax
//!
//! Since there is only a single frontend at the moment the input language is defined by it. Head
//! on over to the [parser's source](../../src/cleaver/src/front/pest/mod.rs.html) which holds the
//! grammar. All of this is subject to change.

pub mod ast;
pub mod pest;
pub mod symbols;

use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::error::Error;

use ::analysis::semantic as sema;
use ::analysis::types as type_checker;
use ::diagnostics as diag;

pub fn process(filepaths: &[&Path]) -> Result<ast::Functions, String> {
    process_with_diag(filepaths, &None)
}

pub fn process_with_diag(filepaths: &[&Path],
                         config: &Option<diag::Config>)
                         -> Result<ast::Functions, String> {
    // run parser
    let mut functions = pest::parse_files(filepaths, config);

    // symbolize everything
    for (_, ref mut f) in &mut functions {
        try!(symbols::symbolize(f));
    }

    // write dot output for functions
    if config.as_ref().map_or(false, |c| c.dump_ast) {
        for (ref name, ref f) in &functions {
            // filepath
            let filepath = config.as_ref()
                .unwrap()
                .output_dir()
                .join(format!("ast_{}_{}.dot", f.node.filename, name));

            // dump
            Write::write_all(&mut File::create(filepath.as_path()).unwrap(),
                             diag::ast::printer::dot::function(f).as_bytes());

            // call dot
            diag::dot::run(filepath.as_path());
        }
    }

    // write symbol table for functions
    if config.as_ref().map_or(false, |c| c.dump_symbol_table) {
        for (ref name, ref f) in &functions {
            // filepath
            let filepath = config.as_ref()
                .unwrap()
                .output_dir()
                .join(format!("symbols_{}_{}.txt", f.node.filename, name));

            // dump
            Write::write_all(&mut File::create(filepath.as_path()).unwrap(),
                             diag::symbols::print(f).as_bytes());

        }
    }

    // all variables must be non-void
    for (_, ref f) in &functions {
        try!(sema::symbols::check_void_variable(f));
    }

    // check call expression targets
    for (_, ref f) in &functions {
        sema::calls::check_target(&functions, f);
    }

    // check those types
    for (_, ref f) in &functions {
        type_checker::check_function(&type_checker::Context {
                current: f,
                functions: &functions,
            })
            .expect("type_checker");
    }

    // TODO additional semantic checks

    Ok(functions)
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
