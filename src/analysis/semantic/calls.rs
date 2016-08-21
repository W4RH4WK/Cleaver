use std::error::Error;
use std::fmt;
use std::result;

use ::front::ast;

pub type Result<'a> = result::Result<(), CallsUnknownFunction<'a>>;

pub fn check_target<'a>(functions: &'a ast::Functions,
                        fun: &'a ast::Node<ast::Function>)
                        -> Result<'a> {
    let mut res = Ok(());

    fun.node.body.visit_expr(&mut |expr| {
        match expr.node {
            ast::Expression::Call { ref function, .. } if !functions.contains_key(function) => {
                res = Err(CallsUnknownFunction { call: expr });
                false
            }
            _ => true,
        }
    });

    res
}

#[derive(PartialEq, Debug)]
pub struct CallsUnknownFunction<'a> {
    call: &'a ast::Node<ast::Expression>,
}

impl<'a> fmt::Display for CallsUnknownFunction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.call.node {
            ast::Expression::Call { ref function, .. } => {
                write!(f,
                       "{}.{} CallsUnknownFunction: `{}`",
                       self.call.pos.line,
                       self.call.pos.col,
                       function)
            }
            _ => panic!("expression not an Expression::Call"),
        }
    }
}

impl<'a> Error for CallsUnknownFunction<'a> {
    fn description(&self) -> &str {
        "call to unknown function"
    }
}
