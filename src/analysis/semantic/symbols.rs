use std::rc::Rc;

use ::front::ast;

pub fn check_void_variable(fun: &ast::Node<ast::Function>) -> Result<(), String> {
    let mut res = Ok(());
    let mut cont = true;

    {
        let mut check_var = |var: &Rc<ast::Variable>| {
            if var.type_ == ast::Type::Void {
                res = Err(format!("Variable `{}` must not be of type void", var.name));
                false
            } else {
                true
            }
        };

        // check arguments
        for arg in &fun.node.args {
            cont = check_var(arg);
            if !cont {
                break;
            }
        }

        if cont {
            cont = fun.node.body.walk_expr(&mut |expr| {
                match expr.node {
                    ast::Expression::Variable { ref var } => check_var(var),
                    _ => true,
                }
            });
        }

        if cont {
            fun.node.body.walk_stmt(&mut |stmt| {
                match stmt.node {
                    ast::Statement::Declaration { ref var } |
                    ast::Statement::Assignment { ref var, .. } => check_var(var),
                    _ => true,
                }
            });
        }
    }

    res
}
