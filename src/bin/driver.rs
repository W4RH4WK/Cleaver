extern crate cleaver;
use cleaver::front;
use cleaver::diag;

extern crate clap;
use clap::{App, Arg};

use std::fs::{DirBuilder, File};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::Command;

fn main() {
    // commandline arguments
    let matches = App::new("Cleaver")
        .about("Cleaver is a toy / research compiler, it takes .gib files as input.")
        .version("0.1")
        .author("Alex Hirsch <W4RH4WK@bluephoenix.at>")
        .arg(Arg::with_name("diagnostic")
            .short("d")
            .help("create various diagnostic outputs during compilation"))
        .arg(Arg::with_name("input")
            .required(true)
            .multiple(true))
        .get_matches();

    // list of input files
    let inputs: Vec<_> = matches.values_of("input").unwrap().map(Path::new).collect();

    // run frontend
    // TODO time process time (diagnostic)
    let functions = front::process_files(&inputs);

    // diagnostics output directory
    let diag_path = PathBuf::from("diag");
    if matches.is_present("diagnostic") {
        DirBuilder::new().create(diag_path.as_path());
    }

    // write dot output for functions
    if matches.is_present("diagnostic") {
        for (ref name, ref f) in &functions {
            // create file
            let filename = format!("ast_{}_{}.dot", f.node.filename, name);
            let mut path = diag_path.clone();
            path.push(filename);
            let mut file = File::create(path.as_path()).unwrap();

            file.write_all(diag::ast::printer::dot::function(f).as_bytes());

            // try calling dot
            Command::new("dot")
                .arg("-Tpng")
                .arg("-O")
                .arg(path.as_path().to_str().unwrap())
                .spawn();
        }
    }
}
