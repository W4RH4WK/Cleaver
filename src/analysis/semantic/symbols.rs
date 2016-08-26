use std::marker::PhantomData;
use std::rc::Rc;

use ::front::ast;
use ::front::ast::Visitable;

struct VoidVariableChecker<'a> {
    result: Result<(), String>,
    phantom: PhantomData<&'a ()>,
}

impl<'a> VoidVariableChecker<'a> {
    fn check_var(&mut self, var: &Rc<ast::Variable>) {
        if var.type_ == ast::Type::Void {
            self.result = Err(format!("Variable `{}` must not be of type void", var.name));
        }
    }
}

impl<'a> Default for VoidVariableChecker<'a> {
    fn default() -> VoidVariableChecker<'a> {
        VoidVariableChecker {
            result: Ok(()),
            phantom: PhantomData,
        }
    }
}

impl<'a> ast::Visitor<'a> for VoidVariableChecker<'a> {
    fn cont(&self) -> bool {
        self.result.is_ok()
    }

    fn visit_stmt(&mut self, stmt: &'a ast::Node<ast::Statement>) {
        match stmt.node {
            ast::Statement::Declaration { ref var } |
            ast::Statement::Assignment { ref var, .. } => self.check_var(var),
            _ => (),
        }
    }

    fn visit_expr(&mut self, expr: &'a ast::Node<ast::Expression>) {
        if let ast::Expression::Variable { ref var } = expr.node {
            self.check_var(var);
        }
    }
}

pub fn check_void_variable(fun: &ast::Node<ast::Function>) -> Result<(), String> {
    let mut checker = VoidVariableChecker::default();
    fun.visit(&mut checker);
    checker.result
}
