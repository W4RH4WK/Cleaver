#![allow(dead_code)]

#![recursion_limit = "200"]
#[macro_use]
extern crate pest;
extern crate rand;

pub mod diag;
pub mod front;
pub mod utils;

pub fn my_name() -> &'static str {
    "Cleaver"
}
