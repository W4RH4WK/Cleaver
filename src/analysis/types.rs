use ::front::ast;

pub struct Context<'a> {
    pub current: &'a ast::Node<ast::Function>,
    pub functions: &'a ast::Functions,
}

pub fn deduce(ctx: &Context, expr: &ast::Node<ast::Expression>) -> Result<ast::Type, String> {
    match expr.node {
        ast::Expression::Literal { ref lit } => Ok(lit.get_type()),
        ast::Expression::Variable { ref var } => Ok(var.type_),
        ast::Expression::Call { ref function, .. } => {
            Ok(ctx.functions.get(function).unwrap().node.ret_type)
        }
        ast::Expression::Unary { ref op, ref expr } => {
            let e = try!(deduce(ctx, expr));
            match *op {
                ast::UnaryOp::NOT => {
                    assert_eq!(ast::Type::Bool, e);
                    Ok(ast::Type::Bool)
                }
                ast::UnaryOp::MINUS => {
                    match e {
                        ast::Type::Int | ast::Type::Float => Ok(e),
                        _ => Err("not a number".to_owned()),
                    }
                }
            }
        }
        ast::Expression::Binary { ref op, ref left, ref right } => {
            let l = try!(deduce(ctx, left));
            let r = try!(deduce(ctx, right));
            assert_eq!(l, r);
            match *op {
                ast::BinaryOp::ADD | ast::BinaryOp::SUB | ast::BinaryOp::MUL |
                ast::BinaryOp::DIV => {
                    match l {
                        ast::Type::Int | ast::Type::Float => Ok(l),
                        _ => Err("not a number".to_owned()),
                    }
                }
                ast::BinaryOp::LT | ast::BinaryOp::LE => {
                    match l {
                        ast::Type::Int | ast::Type::Float => Ok(ast::Type::Bool),
                        _ => Err("not a number".to_owned()),
                    }
                }
                ast::BinaryOp::EQ => {
                    match l {
                        ast::Type::Bool | ast::Type::Int | ast::Type::Float => Ok(ast::Type::Bool),
                        _ => Err("Only bool, int, float support eq".to_owned()),
                    }
                }
            }
        }
        ast::Expression::Parenthesis { ref expr } => deduce(ctx, expr),
    }
}

pub fn check_function(ctx: &Context) -> Result<(), String> {
    // check all calls
    ctx.current.node.body.visit_expr(&mut |e| {
        match e.node {
            ast::Expression::Call { .. } => check_call(ctx, e),
            _ => Ok(()),
        };
        true
    });

    // type check remaining stuff
    check_statement(ctx, &ctx.current.node.body)
}

pub fn check_statement(ctx: &Context, stmt: &ast::Node<ast::Statement>) -> Result<(), String> {
    match stmt.node {
        ast::Statement::Assignment { ref var, ref expr } => {
            assert_eq!(var.type_, deduce(ctx, expr).unwrap());
        }
        ast::Statement::Return { ref expr } => {
            if let Some(ref expr) = *expr {
                assert_eq!(ctx.current.node.ret_type, deduce(ctx, expr).unwrap());
            } else {
                assert_eq!(ast::Type::Void, ctx.current.node.ret_type);
            }
        }
        ast::Statement::If { ref cond, ref on_true, ref on_false } => {
            assert_eq!(ast::Type::Bool, deduce(ctx, cond).unwrap());
            try!(check_statement(ctx, on_true));
            if let Some(ref stmt) = *on_false {
                try!(check_statement(ctx, stmt));
            }
        }
        ast::Statement::While { ref cond, ref body } => {
            assert_eq!(ast::Type::Bool, deduce(ctx, cond).unwrap());
            try!(check_statement(ctx, body));
        }
        ast::Statement::Compound { ref stmts, .. } => {
            for stmt in stmts {
                try!(check_statement(ctx, stmt));
            }
        }
        _ => (),
    }
    Ok(())
}

pub fn check_call(ctx: &Context, expr: &ast::Node<ast::Expression>) -> Result<(), String> {
    if let ast::Expression::Call { ref function, ref args } = expr.node {
        let target = &ctx.functions.get(function).unwrap().node;
        assert_eq!(target.args.len(), args.len());
        for (target_arg, arg) in target.args.iter().zip(args.iter()) {
            assert_eq!(target_arg.type_, deduce(ctx, arg).unwrap());
        }
        Ok(())
    } else {
        Err("Supplied expression is not a call".to_owned())
    }
}
