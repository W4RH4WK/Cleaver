#![allow(dead_code)]

// clippy
#![cfg_attr(feature="clippy", feature(plugin))]
#![cfg_attr(feature="clippy", plugin(clippy))]

// pest
#![recursion_limit = "200"]
#[macro_use]
extern crate pest;

// misc
extern crate rand;

pub mod analysis;
pub mod diagnostics;
pub mod front;
pub mod utils;
