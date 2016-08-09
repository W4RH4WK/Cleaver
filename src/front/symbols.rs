use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use super::ast;

#[derive(PartialEq, Debug)]
pub struct SymbolTable {
    parent: Option<Rc<RefCell<SymbolTable>>>,
    id_counter: u32,
    vars: HashMap<String, Rc<ast::Variable>>,
}

impl Default for SymbolTable {
    fn default() -> SymbolTable {
        SymbolTable::new(None, 1)
    }
}

impl SymbolTable {
    pub fn new(parent: Option<Rc<RefCell<SymbolTable>>>, id_counter: u32) -> SymbolTable {
        SymbolTable {
            parent: parent,
            id_counter: id_counter,
            vars: HashMap::new(),
        }
    }

    pub fn insert(&mut self, var: &mut Rc<ast::Variable>) -> Result<Rc<ast::Variable>, String> {
        // check for duplicate
        if self.vars.contains_key(&var.name) {
            return Err(format!("`{}` already declared", var.name));
        }

        // insert new one
        let new = Rc::new(ast::Variable {
            id: self.id_counter,
            type_: var.type_,
            name: var.name.clone(),
        });
        self.id_counter += 1;
        self.vars.insert(var.name.clone(), new.clone());

        // return reference to final variable
        Ok(new)
    }

    pub fn lookup(&self, var: &String) -> Result<Rc<ast::Variable>, String> {
        if let Some(v) = self.vars.get(var) {
            return Ok(v.clone());
        }

        // check previous scope
        match self.parent {
            Some(ref p) => p.borrow().lookup(var),
            None => Err(format!("`{}` unknown variable", var)),
        }
    }
}

pub fn symbolize(fun: &mut ast::Node<ast::Function>) -> Result<(), String> {
    // arguments
    for arg in fun.node.args.iter_mut() {
        *arg = try!(fun.node.symbols.borrow_mut().insert(arg));
    }

    symbolize_statement(fun.node.symbols.clone(), &mut fun.node.body)
}

fn symbolize_statement(symbols: Rc<RefCell<SymbolTable>>, stmt: &mut ast::Node<ast::Statement>) -> Result<(), String> {
    match stmt.node {
        ast::Statement::Expression { ref mut expr } => symbolize_expression(symbols, expr),
        ast::Statement::Declaration { ref mut var } => {
            *var = try!(symbols.borrow_mut().insert(var));
            Ok(())
        }
        ast::Statement::Assignment { ref mut var, ref mut expr } => {
            *var = try!(symbols.borrow().lookup(&var.name));
            symbolize_expression(symbols, expr)
        }
        ast::Statement::If { ref mut cond, ref mut on_true, ref mut on_false } => {
            try!(symbolize_expression(symbols.clone(), cond));
            try!(symbolize_statement(symbols.clone(), on_true));
            if let Some(ref mut stmt) = *on_false {
                symbolize_statement(symbols.clone(), stmt)
            } else {
                Ok(())
            }
        }
        ast::Statement::While { ref mut cond, ref mut body } => {
            try!(symbolize_expression(symbols.clone(), cond));
            symbolize_statement(symbols.clone(), body)
        }
        ast::Statement::Return { expr: Some(ref mut e) } => symbolize_expression(symbols, e),
        ast::Statement::Compound { ref mut stmts, symbols: ref mut next } => {
            *next = Rc::new(RefCell::new(SymbolTable::new(Some(symbols.clone()),
                                                          symbols.borrow().id_counter)));
            for stmt in stmts.iter_mut() {
                try!(symbolize_statement(next.clone(), stmt));
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn symbolize_expression(symbols: Rc<RefCell<SymbolTable>>, expr: &mut ast::Node<ast::Expression>) -> Result<(), String> {
    match expr.node {
        ast::Expression::Variable { ref mut var } => {
            *var = try!(symbols.borrow().lookup(&var.name));
            Ok(())
        }
        ast::Expression::Call { ref mut args, .. } => {
            for arg in args.iter_mut() {
                try!(symbolize_expression(symbols.clone(), arg));
            }
            Ok(())
        }
        ast::Expression::Unary { ref mut expr, .. } => symbolize_expression(symbols.clone(), expr),
        ast::Expression::Binary { ref mut left, ref mut right, .. } => {
            try!(symbolize_expression(symbols.clone(), left));
            symbolize_expression(symbols.clone(), right)
        }
        ast::Expression::Parenthesis { ref mut expr } => {
            symbolize_expression(symbols.clone(), expr)
        }
        _ => Ok(()),
    }
}
