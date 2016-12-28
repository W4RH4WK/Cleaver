extern crate cleaver;
use cleaver::fe;
use cleaver::diag;

extern crate clap;
use clap::{App, Arg};

extern crate time;
use time::PreciseTime;

use std::path::Path;
use std::process::exit;

fn main() {
    // commandline arguments
    let arguments = App::new("Cleaver")
        .about("Cleaver is a toy / research compiler, it takes .gib files as input.")
        .version("0.1")
        .author("Alex Hirsch <W4RH4WK@bluephoenix.at>")
        .arg(Arg::with_name("diagnostics")
            .short("d")
            .help("create various diagnostic outputs during compilation"))
        .arg(Arg::with_name("timings")
            .short("t")
            .help("print timings after each phase"))
        .arg(Arg::with_name("input")
            .required(true)
            .multiple(true))
        .get_matches();

    // initiate diagnostics config
    let config = if arguments.is_present("diagnostics") {
        Some(diag::Config::default())
    } else {
        None
    };

    // list of input files
    let inputs: Vec<_> = arguments.values_of("input").unwrap().map(Path::new).collect();

    // run frontend
    let start = PreciseTime::now();
    let functions = fe::parse_with_diag(&inputs, &config).unwrap();
    fe::check_with_diag(&functions, &config).unwrap();
    let end = PreciseTime::now();
    if arguments.is_present("timings") {
        println!("Frontend time: {}", start.to(end));
    }

    // TODO error hanlding
}
