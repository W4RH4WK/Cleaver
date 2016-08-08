
pub mod print_simple {
    use ::front::ast;

    pub fn function(fun: &ast::Node<ast::Function>) -> String {
        // arguments
        let args = fun.node
            .args
            .iter()
            .map(|ref arg| format!("{:?} {}", arg.type_, arg.name))
            .collect::<Vec<String>>()
            .join(", ");

        format!("{}:{}\t{} ({}) -> {:?}",
                fun.node.filename,
                fun.pos.line,
                fun.node.name,
                args,
                fun.node.ret_type)
    }

    pub fn statement(stmt: &ast::Node<ast::Statement>) -> String {
        match stmt.node {
            ast::Statement::Expression { .. } => "Expression".to_owned(),
            ast::Statement::Declaration { .. } => "Declaration".to_owned(),
            ast::Statement::Assignment { .. } => "Assignment".to_owned(),
            ast::Statement::If { .. } => "If".to_owned(),
            ast::Statement::While { .. } => "While".to_owned(),
            ast::Statement::Return { .. } => "Return".to_owned(),
            ast::Statement::Compound { .. } => "Compound".to_owned(),
        }
    }

    pub fn expression(expr: &ast::Node<ast::Expression>) -> String {
        match expr.node {
            ast::Expression::Literal { ref lit } => literal(lit),
            ast::Expression::Variable { ref var } => variable(var.as_ref()),
            ast::Expression::Call { ref function, .. } => format!("{}(..)", function),
            ast::Expression::Unary { ref op, .. } => format!("{:?} ", op),
            ast::Expression::Binary { ref op, .. } => format!(" {:?} ", op),
            ast::Expression::Parenthesis { .. } => "(  )".to_owned(),
        }
    }

    pub fn literal(lit: &ast::Literal) -> String {
        match *lit {
            ast::Literal::Bool(ref v) => format!("{}", v).to_owned(),
            ast::Literal::Int(ref v) => format!("{}", v).to_owned(),
            ast::Literal::Float(ref v) => format!("{}f", v).to_owned(),
            ast::Literal::Str(ref v) => format!("\"{}\"", v).to_owned(),
        }
    }

    pub fn variable(var: &ast::Variable) -> String {
        format!("{}_{}({:?})", var.id, var.name, var.type_)
    }
}
