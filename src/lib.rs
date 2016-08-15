#![allow(dead_code)]
#![cfg_attr(feature = "clippy", allow(unknown_lints))]

#![recursion_limit = "200"]
#[macro_use]
extern crate pest;
extern crate rand;

pub mod analysis;
pub mod diag;
pub mod front;
pub mod utils;

pub fn my_name() -> &'static str {
    "Cleaver"
}
