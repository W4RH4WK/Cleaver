use std::cell::RefCell;
use std::collections::hash_map::Values;
use std::collections::HashMap;
use std::rc::Rc;

use super::ast;

#[derive(PartialEq, Debug)]
pub struct SymbolTable {
    parent: Option<Rc<RefCell<SymbolTable>>>,
    vars: HashMap<String, Rc<ast::Variable>>,
}

impl Default for SymbolTable {
    fn default() -> SymbolTable {
        SymbolTable::new(None)
    }
}

impl SymbolTable {
    pub fn new(parent: Option<Rc<RefCell<SymbolTable>>>) -> SymbolTable {
        SymbolTable {
            parent: parent,
            vars: HashMap::new(),
        }
    }

    pub fn insert(&mut self,
                  var: &mut Rc<ast::Variable>,
                  id: u32)
                  -> Result<Rc<ast::Variable>, String> {
        // check for duplicate
        if self.vars.contains_key(&var.name) {
            return Err(format!("`{}` already declared in this scope", var.name));
        }

        // insert new one
        let new = Rc::new(ast::Variable {
            id: id,
            type_: var.type_,
            name: var.name.clone(),
        });
        self.vars.insert(var.name.clone(), new.clone());

        // return reference to final variable
        Ok(new)
    }

    pub fn lookup(&self, var: &str) -> Result<Rc<ast::Variable>, String> {
        if let Some(v) = self.vars.get(var) {
            return Ok(v.clone());
        }

        // check previous scope
        match self.parent {
            Some(ref p) => p.borrow().lookup(var),
            None => Err(format!("`{}` unknown variable", var)),
        }
    }

    pub fn iter(&self) -> Values<String, Rc<ast::Variable>> {
        self.vars.values()
    }
}

pub fn symbolize(fun: &mut ast::Node<ast::Function>) -> Result<(), String> {
    let mut id_counter: u32 = 1;

    // arguments
    for arg in &mut fun.node.args {
        *arg = try!(fun.node.symbols.borrow_mut().insert(arg, id_counter));
        id_counter += 1;
    }

    symbolize_statement(fun.node.symbols.clone(),
                        &mut fun.node.body,
                        &mut id_counter)
}

fn symbolize_statement(symbols: Rc<RefCell<SymbolTable>>,
                       stmt: &mut ast::Node<ast::Statement>,
                       id_counter: &mut u32)
                       -> Result<(), String> {
    match stmt.node {
        ast::Statement::Expression { ref mut expr } => {
            symbolize_expression(symbols, expr, id_counter)
        }
        ast::Statement::Declaration { ref mut var } => {
            *var = try!(symbols.borrow_mut().insert(var, *id_counter));
            *id_counter += 1;
            Ok(())
        }
        ast::Statement::Assignment { ref mut var, ref mut expr } => {
            *var = try!(symbols.borrow().lookup(&var.name));
            symbolize_expression(symbols, expr, id_counter)
        }
        ast::Statement::If { ref mut cond, ref mut on_true, ref mut on_false } => {
            try!(symbolize_expression(symbols.clone(), cond, id_counter));
            try!(symbolize_statement(symbols.clone(), on_true, id_counter));
            if let Some(ref mut stmt) = *on_false {
                symbolize_statement(symbols.clone(), stmt, id_counter)
            } else {
                Ok(())
            }
        }
        ast::Statement::While { ref mut cond, ref mut body } => {
            try!(symbolize_expression(symbols.clone(), cond, id_counter));
            symbolize_statement(symbols.clone(), body, id_counter)
        }
        ast::Statement::Return { expr: Some(ref mut e) } => {
            symbolize_expression(symbols, e, id_counter)
        }
        ast::Statement::Compound { ref mut stmts, symbols: ref mut next } => {
            *next = Rc::new(RefCell::new(SymbolTable::new(Some(symbols.clone()))));
            for stmt in stmts.iter_mut() {
                try!(symbolize_statement(next.clone(), stmt, id_counter));
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

fn symbolize_expression(symbols: Rc<RefCell<SymbolTable>>,
                        expr: &mut ast::Node<ast::Expression>,
                        id_counter: &mut u32)
                        -> Result<(), String> {
    match expr.node {
        ast::Expression::Variable { ref mut var } => {
            *var = try!(symbols.borrow().lookup(&var.name));
            Ok(())
        }
        ast::Expression::Call { ref mut args, .. } => {
            for arg in args.iter_mut() {
                try!(symbolize_expression(symbols.clone(), arg, id_counter));
            }
            Ok(())
        }
        ast::Expression::Binary { ref mut left, ref mut right, .. } => {
            try!(symbolize_expression(symbols.clone(), left, id_counter));
            symbolize_expression(symbols.clone(), right, id_counter)
        }
        ast::Expression::Unary { ref mut expr, .. } |
        ast::Expression::Parenthesis { ref mut expr } => {
            symbolize_expression(symbols.clone(), expr, id_counter)
        }
        _ => Ok(()),
    }
}
