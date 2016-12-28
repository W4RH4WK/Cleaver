use std::error::Error;
use std::fmt;
use std::result;

use ::fe::ast;
use ::fe::error::CheckError;
use ::fe::Position;

pub type Result<'a> = result::Result<(), CallsUnknownFunction<'a>>;

pub fn check_target<'a>(functions: &'a ast::Functions,
                        fun: &'a ast::Node<ast::Function>)
                        -> Result<'a> {
    let mut res = Ok(());

    fun.node.body.visit_expr(&mut |expr| {
        match expr.node {
            ast::Expression::Call { ref function, .. } if !functions.contains_key(function) => {
                res = Err(CallsUnknownFunction {
                    call: expr,
                    filename: fun.node.filename.clone(),
                });
                false
            }
            _ => true,
        }
    });

    res
}

#[derive(PartialEq, Debug)]
pub struct CallsUnknownFunction<'a> {
    pub call: &'a ast::Node<ast::Expression>,
    pub filename: String,
}

impl<'a> fmt::Display for CallsUnknownFunction<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.call.node {
            ast::Expression::Call { ref function, .. } => {
                write!(f, "CallsUnknownFunction: `{}`", function)
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

impl<'a> From<CallsUnknownFunction<'a>> for CheckError<'a> {
    fn from(err: CallsUnknownFunction<'a>) -> CheckError<'a> {
        CheckError::Call(err)
    }
}
