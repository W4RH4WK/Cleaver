//! # The `.gib` Syntax
//!
//! Since there is only a single frontend at the moment the input language is defined by it. Head
//! on over to the [parser's source](../../src/cleaver/src/front/pest/mod.rs.html) which holds the
//! grammar. All of this is subject to change.

pub mod ast;
pub mod error;
pub mod parser;
pub mod symbolizer;

use std::fs::File;
use std::io::Write;
use std::path::Path;
use std::result::Result;

use ::analysis::semantic as sema;
use ::analysis::types as type_checker;
use ::diag;
use self::error::CheckError;

/// `Position` represents a location within a certain source file. It is composed of a line- and column-number.
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

/// Runs the parser and symbolizer on all given files, returning an AST for each of them.
pub fn parse(filepaths: &[&Path]) -> Result<ast::Functions, symbolizer::SymbolError> {
    parse_with_diag(filepaths, &None)
}

pub fn parse_with_diag(filepaths: &[&Path],
                         config: &Option<diag::Config>)
                         -> Result<ast::Functions, symbolizer::SymbolError> {
    // run parser
    let mut functions = parser::parse_files(filepaths, config);

    // symbolize everything
    for f in functions.values_mut() {
        try!(symbolizer::symbolize_with_diag(f, config));
    }

    // write dot output for functions
    if config.as_ref().map_or(false, |c| c.dump_ast) {
        for (name, f) in &functions {
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

    Ok(functions)
}

/// Run checks on parsed input. This includes type checks as well as semantic checks.
pub fn check(functions: &ast::Functions) -> Result<(), CheckError> {
    check_with_diag(functions, &None)
}

pub fn check_with_diag<'a>(functions: &'a ast::Functions, config: &Option<diag::Config>) -> Result<(), CheckError<'a>> {
    functions.values()
        .fold(Ok(()), |prev, f| {
            prev.and(sema::symbols::check_void_variable(f).map_err(CheckError::from))
                .and(sema::calls::check_target(functions, f).map_err(CheckError::from))
                .and(type_checker::check_function_with_diag(functions, f, config).map_err(CheckError::from))
        })
}
