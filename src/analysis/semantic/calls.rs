use ::front::ast;

pub fn check_target(functions: &ast::Functions, fun: &ast::Node<ast::Function>) {
    fun.node.body.visit_expr(&mut |expr| {
        match expr.node {
            ast::Expression::Call { ref function, .. } => assert!(functions.contains_key(function)),
            _ => (),
        };
        true
    });
}
