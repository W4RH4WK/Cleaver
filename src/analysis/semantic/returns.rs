use ::fe::ast;

pub fn check(function: &ast::Node<ast::Function>) -> bool {
    check_statement(&function.node.body)
}

fn check_statement(stmt: &ast::Node<ast::Statement>) -> bool {
    match stmt.node {
        ast::Statement::Compound { ref stmts } => stmts.last().map_or(false, check_statement),
        ast::Statement::If { ref on_true, on_false: Some(ref on_false), .. } => {
            check_statement(on_true) && check_statement(on_false)
        }
        ast::Statement::Return { .. } => true,
        _ => false,
    }
}
