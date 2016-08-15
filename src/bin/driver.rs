extern crate cleaver;
use cleaver::front;
use cleaver::diag;

extern crate clap;
use clap::{App, Arg};

use std::path::Path;

fn main() {
    // commandline arguments
    let matches = App::new("Cleaver")
        .about("Cleaver is a toy / research compiler, it takes .gib files as input.")
        .version("0.1")
        .author("Alex Hirsch <W4RH4WK@bluephoenix.at>")
        .arg(Arg::with_name("diagnostics")
            .short("d")
            .help("create various diagnostic outputs during compilation"))
        .arg(Arg::with_name("input")
            .required(true)
            .multiple(true))
        .get_matches();

    // initiate diagnostics config
    let config = if matches.is_present("diagnostics") {
        Some(diag::Config::default())
    } else {
        None
    };

    // list of input files
    let inputs: Vec<_> = matches.values_of("input").unwrap().map(Path::new).collect();

    // run frontend
    // TODO time process time (diagnostics)
    let functions = front::process_with_diag(&inputs, &config).expect("Frontend");
}
