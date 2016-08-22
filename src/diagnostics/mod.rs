pub mod ast;
pub mod symbols;

use std::fs::DirBuilder;
use std::path::PathBuf;

pub struct Config {
    output_dir: PathBuf,
    pub dump_tokens: bool,
    pub dump_ast: bool,
    pub dump_symbol_table: bool,
}

impl Config {
    pub fn new(output_dir: &str) -> Config {
        let output_dir = PathBuf::from(output_dir);

        // create output directory
        DirBuilder::new().create(output_dir.as_path()).expect("Diagnostics Directory");

        Config {
            output_dir: PathBuf::from(output_dir),
            dump_tokens: true,
            dump_ast: true,
            dump_symbol_table: true,
        }
    }

    pub fn output_dir(&self) -> PathBuf {
        self.output_dir.clone()
    }
}

impl Default for Config {
    fn default() -> Config {
        Config::new("diag")
    }
}

pub mod dot {
    use std::path::Path;
    use std::process::Command;

    /// Try to run the `dot` command, fails silently.
    pub fn run(filepath: &Path) {
        Command::new("dot")
            .arg("-Tpng")
            .arg("-O")
            .arg(filepath.to_str().unwrap())
            .spawn()
            .ok();
    }
}
