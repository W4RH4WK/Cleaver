#![allow(dead_code)]

#![recursion_limit = "200"]
#[macro_use]
extern crate pest;

pub mod diag;
pub mod front;

pub fn my_name() -> &'static str {
    "Cleaver"
}
