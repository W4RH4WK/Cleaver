use std::error::Error;
use std::fmt;
use std::result;

// TODO use ::diag;
use ::front::ast;

pub type Result<'a, T> = result::Result<T, TypeError<'a>>;

pub struct Context<'a> {
    pub current: &'a ast::Node<ast::Function>,
    pub functions: &'a ast::Functions,
}

pub fn deduce<'a>(ctx: &Context, expr: &'a ast::Node<ast::Expression>) -> Result<'a, ast::Type> {
    match expr.node {
        ast::Expression::Literal { ref lit } => Ok(lit.get_type()),

        ast::Expression::Variable { ref var } => Ok(var.type_),

        ast::Expression::Call { ref function, .. } => {
            // check if call is valid (argument count and types)
            try!(check_call(ctx, expr));

            // use target function's return type
            Ok(ctx.functions.get(function).expect("call to unknown function").node.ret_type)
        }

        ast::Expression::Unary { ref op, ref expr } => {
            let e = try!(deduce(ctx, expr));
            match *op {
                ast::UnaryOp::NOT if e == ast::Type::Bool => Ok(e),

                ast::UnaryOp::MINUS if [ast::Type::Int, ast::Type::Float].contains(&e) => Ok(e),

                _ => {
                    Err(TypeError::UnsupportedOperator {
                        expr: expr,
                        op: ast::Op::UnaryOp(*op),
                        type_: e,
                    })
                }
            }
        }

        ast::Expression::Binary { ref op, ref left, ref right } => {
            let t = try!(deduce(ctx, left));

            try!(expect(ctx, right, t));

            match *op {
                // `+`, `-`, `*`, `/` supported for numbers
                ast::BinaryOp::ADD | ast::BinaryOp::SUB | ast::BinaryOp::MUL |
                ast::BinaryOp::DIV if [ast::Type::Int, ast::Type::Float].contains(&t) => Ok(t),

                // `<`, `<=` supported for numbers
                ast::BinaryOp::LT | ast::BinaryOp::LE if [ast::Type::Int, ast::Type::Float]
                    .contains(&t) => Ok(ast::Type::Bool),

                // `==` supported for bool and numbers
                ast::BinaryOp::EQ if [ast::Type::Bool, ast::Type::Int, ast::Type::Float]
                    .contains(&t) => Ok(ast::Type::Bool),

                _ => {
                    Err(TypeError::UnsupportedOperator {
                        expr: expr,
                        op: ast::Op::BinaryOp(*op),
                        type_: t,
                    })
                }
            }
        }

        ast::Expression::Parenthesis { ref expr } => deduce(ctx, expr),
    }
}

pub fn expect<'a>(ctx: &Context,
                  expr: &'a ast::Node<ast::Expression>,
                  expected: ast::Type)
                  -> Result<'a, ()> {
    let deduced = try!(deduce(ctx, expr));
    if deduced != expected {
        Err(TypeError::TypeMismatch {
            expr: expr,
            expected: expected,
            actual: deduced,
        })
    } else {
        Ok(())
    }
}

pub fn check_function<'a>(ctx: &Context<'a>) -> Result<'a, ()> {
    check_statement(ctx, &ctx.current.node.body)
}

pub fn check_statement<'a>(ctx: &Context, stmt: &'a ast::Node<ast::Statement>) -> Result<'a, ()> {
    match stmt.node {
        ast::Statement::Assignment { ref var, ref expr } => expect(ctx, expr, var.type_),

        ast::Statement::Return { ref expr } => {
            if let Some(ref expr) = *expr {
                expect(ctx, expr, ctx.current.node.ret_type)
            } else if ctx.current.node.ret_type != ast::Type::Void {
                Err(TypeError::InvalidReturnValue { stmt: stmt })
            } else {
                Ok(())
            }
        }

        ast::Statement::If { ref cond, ref on_true, ref on_false } => {
            try!(expect(ctx, cond, ast::Type::Bool));
            try!(check_statement(ctx, on_true));
            if let Some(ref stmt) = *on_false {
                try!(check_statement(ctx, stmt));
            }
            Ok(())
        }

        ast::Statement::While { ref cond, ref body } => {
            try!(expect(ctx, cond, ast::Type::Bool));
            check_statement(ctx, body)
        }

        ast::Statement::Compound { ref stmts, .. } => {
            for stmt in stmts {
                try!(check_statement(ctx, stmt));
            }
            Ok(())
        }
        _ => Ok(()),
    }
}

pub fn check_call<'a>(ctx: &Context, expr: &'a ast::Node<ast::Expression>) -> Result<'a, ()> {
    match expr.node {
        ast::Expression::Call { ref function, ref args } => {
            // call target checks should be done in a previous phase
            let target = &ctx.functions.get(function).expect("call to unknown function").node;

            // check count
            if target.args.len() != args.len() {
                return Err(TypeError::WrongArgumentCount {
                    call: expr,
                    expected: target.args.len(),
                    actual: args.len(),
                });
            }

            for (target_arg, arg) in target.args.iter().zip(args.iter()) {
                // check type
                try!(expect(ctx, arg, target_arg.type_))
            }
            Ok(())
        }

        // don't care about other nodes
        _ => Ok(()),
    }
}

#[derive(PartialEq, Debug)]
pub enum TypeError<'a> {
    TypeMismatch {
        expr: &'a ast::Node<ast::Expression>,
        expected: ast::Type,
        actual: ast::Type,
    },
    WrongArgumentCount {
        call: &'a ast::Node<ast::Expression>,
        expected: usize,
        actual: usize,
    },
    UnsupportedOperator {
        expr: &'a ast::Node<ast::Expression>,
        op: ast::Op,
        type_: ast::Type,
    },
    InvalidReturnValue {
        stmt: &'a ast::Node<ast::Statement>,
    },
}

impl<'a> fmt::Display for TypeError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TypeError::TypeMismatch { ref expr, ref expected, ref actual } => {
                write!(f,
                       "{}.{} TypeMismatch: expected `{:?}` got `{:?}`",
                       expr.pos.line,
                       expr.pos.col,
                       expected,
                       actual)
            }
            TypeError::WrongArgumentCount { ref call, ref expected, ref actual } => {
                write!(f,
                       "{}.{} WrongArgumentCount: expected `{}` got `{}`",
                       call.pos.line,
                       call.pos.col,
                       expected,
                       actual)
            }
            TypeError::UnsupportedOperator { ref expr, ref op, ref type_ } => {
                write!(f,
                       "{}.{} UnsupportedOperator: `{:?}` for `{:?}`",
                       expr.pos.line,
                       expr.pos.col,
                       op,
                       type_)
            }
            TypeError::InvalidReturnValue { ref stmt } => {
                write!(f,
                       "{}.{} InvalidReturnValue: Return value given in void function",
                       stmt.pos.line,
                       stmt.pos.col)
            }
        }
    }
}

impl<'a> Error for TypeError<'a> {
    fn description(&self) -> &str {
        match *self {
            TypeError::TypeMismatch { .. } => "encountered unexpected type",
            TypeError::WrongArgumentCount { .. } => "number of arguments does not match",
            TypeError::UnsupportedOperator { .. } => "operator cannot be used with given type",
            TypeError::InvalidReturnValue { .. } => "return value given in void function",
        }
    }
}
