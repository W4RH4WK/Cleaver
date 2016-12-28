use std::error::Error;
use std::convert::From;
use std::fmt;

use ::analysis::semantic::calls::CallsUnknownFunction;
use ::analysis::types::TypeError;

#[derive(PartialEq, Debug)]
pub enum CheckError<'a> {
    Return(String),
    Call(CallsUnknownFunction<'a>),
    Type(TypeError<'a>),
}

impl<'a> fmt::Display for CheckError<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            CheckError::Return(ref s) => write!(f, "{}", s),
            CheckError::Call
    (ref e) => write!(f, "{}", e),
            CheckError::Type(ref e) => write!(f, "{}", e),
        }
    }
}

impl<'a> Error for CheckError<'a> {
    fn description(&self) -> &str {
        match *self {
            CheckError::Return(..) => "Something about return statements",
            CheckError::Call
    (ref e) => e.description(),
            CheckError::Type(ref e) => e.description(),
        }
    }

    fn cause(&self) -> Option<&Error> {
        match *self {
            CheckError::Return(..) => None,
            CheckError::Call
    (ref e) => Some(e),
            CheckError::Type(ref e) => Some(e),
        }
    }
}

// TODO remove
impl<'a> From<String> for CheckError<'a> {
    fn from(s: String) -> CheckError<'a> {
        CheckError::Return(s)
    }
}
