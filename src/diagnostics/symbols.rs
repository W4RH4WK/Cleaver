use std::iter::repeat;
use std::fs::File;
use std::io::Write;
use std::rc::Rc;

use ::diagnostics::ast::printer::simple as simple_printer;
use ::front::ast;

use super::Config;

#[derive(Debug)]
pub struct Log {
    pub out: File,
    pub indent: usize,
}

impl Log {
    pub fn new(function: &ast::Node<ast::Function>, config: &Option<Config>) -> Log {
        let filepath = config.as_ref()
            .unwrap()
            .output_dir()
            .join(format!("scopes_{}_{}.txt",
                          function.node.filename,
                          function.node.name));
        Log {
            out: File::create(filepath).expect("Scopes"),
            indent: 0,
        }
    }

    pub fn log(&mut self, data: &str) {
        self.out.write_all(data.as_bytes()).expect("Scopes log");
    }

    fn indent(&mut self) {
        let indent: String = repeat("\t").take(self.indent).collect();
        self.log(&indent);
    }

    pub fn function(&mut self, function: &ast::Node<ast::Function>) {
        self.log(&simple_printer::function(function));
        self.log("\n{\n");
        self.indent += 1;
    }

    pub fn insert(&mut self, var: &Rc<ast::Variable>) {
        self.indent();
        self.log(&simple_printer::variable(var));
        self.log("\n");
    }

    pub fn scope_open(&mut self, stmt: &ast::Node<ast::Statement>) {
        self.indent();
        self.log(&format!("{}:{} {{\n", simple_printer::statement(stmt), stmt.pos.line));
        self.indent += 1;
    }

    pub fn scope_close(&mut self) {
        self.indent -= 1;
        self.indent();
        self.log("}\n");
    }
}
