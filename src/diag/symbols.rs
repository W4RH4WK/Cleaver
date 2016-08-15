use std::iter::repeat;

use ::diag::ast::printer::simple as simple_printer;
use ::front::ast;

pub fn print(fun: &ast::Node<ast::Function>) -> String {
    let mut dump = simple_printer::function(fun);
    dump.push_str("\n");

    // add arguments
    for var in fun.node.symbols.borrow().iter() {
        dump.push_str(&format!("\t - {}\n", simple_printer::variable(&*var)));
    }

    statement(&mut dump, &fun.node.body, 1);

    dump
}

fn statement(dump: &mut String, stmt: &ast::Node<ast::Statement>, depth: usize) {
    let indent: String = repeat("\t").take(depth).collect();
    match stmt.node {
        ast::Statement::Compound { ref stmts, ref symbols } => {
            dump.push_str(&format!("{}{}:{}\n",
                                   indent,
                                   simple_printer::statement(stmt),
                                   stmt.pos.line));

            // add symbols
            for var in symbols.borrow().iter() {
                dump.push_str(&format!("{} - {}\n", indent, simple_printer::variable(&*var)));
            }

            // recursion
            for stmt in stmts {
                statement(dump, stmt, depth + 1);
            }
        }
        ast::Statement::If { ref on_true, ref on_false, .. } => {
            statement(dump, on_true, depth);
            if let Some(ref stmt) = *on_false {
                statement(dump, stmt, depth);
            }
        }
        ast::Statement::While { ref body, .. } => {
            statement(dump, body, depth);
        }
        _ => (),
    }
}
