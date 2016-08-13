pub mod simple {
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
        format!("{}_{} ({:?})", var.id, var.name, var.type_)
    }
}

pub mod dot {
    use ::front::ast;
    use super::simple;

    pub fn function(fun: &ast::Node<ast::Function>) -> String {
        let options = vec!["\tnodesep=0.6".to_owned()];
        format!("digraph \"{}_{}\" {{\n{}\n{}{}{}}}\n",
                fun.node.filename,
                fun.node.name,
                options.join("\n"),
                node(fun, &simple::function(fun)),
                edge(fun, &fun.node.body, "body"),
                statement(&fun.node.body))
    }

    fn statement(stmt: &ast::Node<ast::Statement>) -> String {
        let mut ret = node(stmt, &simple::statement(stmt));
        match stmt.node {
            ast::Statement::Expression { ref expr } => {
                ret.push_str(&expression(expr));
                ret.push_str(&edge(stmt, expr, "expr"));
            }
            ast::Statement::Declaration { ref var } => {
                ret.push_str(&node_var(var));
                ret.push_str(&edge_to_var(stmt, var))
            }
            ast::Statement::Assignment { ref var, ref expr } => {
                ret.push_str(&node_var(var));
                ret.push_str(&edge_to_var(stmt, var));
                ret.push_str(&expression(expr));
                ret.push_str(&edge(stmt, expr, "expr"));
            }
            ast::Statement::If { ref cond, ref on_true, ref on_false } => {
                ret.push_str(&expression(cond));
                ret.push_str(&edge(stmt, cond, "cond"));
                ret.push_str(&statement(on_true));
                ret.push_str(&edge(stmt, on_true, "on_true"));
                if let Some(ref else_part) = *on_false {
                    ret.push_str(&statement(else_part));
                    ret.push_str(&edge(stmt, else_part, "on_false"));
                }
            }
            ast::Statement::While { ref cond, ref body } => {
                ret.push_str(&expression(cond));
                ret.push_str(&edge(stmt, cond, "cond"));
                ret.push_str(&statement(body));
                ret.push_str(&edge(stmt, body, "body"));
            }
            ast::Statement::Return { expr: Some(ref expr) } => {
                ret.push_str(&expression(expr));
                ret.push_str(&edge(stmt, expr, "expr"));
            }
            ast::Statement::Compound { ref stmts, .. } => {
                for (i, s) in stmts.iter().enumerate() {
                    ret.push_str(&statement(s));
                    ret.push_str(&edge(stmt, s, &*i.to_string()));
                }
            }
            _ => (),
        }
        ret
    }

    fn expression(expr: &ast::Node<ast::Expression>) -> String {
        let mut ret = node(expr, &simple::expression(expr));
        match expr.node {
            ast::Expression::Call { ref args, .. } => {
                for (i, arg) in args.iter().enumerate() {
                    ret.push_str(&expression(arg));
                    ret.push_str(&edge(expr, arg, &*i.to_string()));
                }
            }
            ast::Expression::Unary { expr: ref e, .. } |
            ast::Expression::Parenthesis { expr: ref e } => {
                ret.push_str(&expression(e));
                ret.push_str(&edge(expr, e, "expr"));
            }
            ast::Expression::Binary { ref left, ref right, .. } => {
                ret.push_str(&expression(left));
                ret.push_str(&edge(expr, left, "left"));
                ret.push_str(&expression(right));
                ret.push_str(&edge(expr, right, "right"));
            }
            _ => (),
        }
        ret
    }

    fn node<N>(node: &ast::Node<N>, label: &str) -> String {
        format!("\t\"{:?}\" [shape=box, label=\"{}\", fontname=\"Monospace\"];\n",
                node as *const ast::Node<N>,
                label)
    }

    fn node_var(var: &ast::Variable) -> String {
        format!("\t\"{:?}\" [shape=box, label=\"{}\"];\n",
                var as *const ast::Variable,
                simple::variable(var))
    }

    fn edge<N, M>(from: &ast::Node<N>, to: &ast::Node<M>, label: &str) -> String {
        format!("\t\"{:?}\" -> \"{:?}\" [label=\"{}\"];\n",
                from as *const ast::Node<N>,
                to as *const ast::Node<M>,
                label)
    }

    fn edge_to_var<N>(from: &ast::Node<N>, to: &ast::Variable) -> String {
        format!("\t\"{:?}\" -> \"{:?}\" [label=\"var\"];\n",
                from as *const ast::Node<N>,
                to as *const ast::Variable)
    }
}
