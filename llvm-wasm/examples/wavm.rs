#[macro_use]
extern crate log;
#[macro_use]
extern crate failure;

use std::env;
use std::process;

use failure::Error;
use getopts::{Matches, Options};

fn parse_cmdline(program: &str, args: &[String]) -> Result<Option<Matches>, Error> {
    let mut opts = Options::new();

    opts.optopt(
        "f",
        "function",
        "main",
        "function name to run in module rather than main",
    );
    opts.optflag("c", "check", "exit after checking that the program is valid");
    opts.optflag("h", "help", "print this help menu");

    let matches = opts.parse(args)?;

    if matches.opt_present("h") {
        let brief = format!("Usage: {} [options] FILE", program);

        print!("{}", opts.usage(&brief));

        Ok(None)
    } else {
        Ok(Some(matches))
    }
}

fn main() {
    pretty_env_logger::init();

    let args: Vec<String> = env::args().collect();
    let (program, args) = args.split_first().unwrap();

    let matches = match parse_cmdline(program, args) {
        Ok(Some(matches)) => matches,
        Ok(None) => {
            process::exit(0);
        }
        Err(err) => {
            eprintln!("Fail to parse command line, {}", err);
            process::exit(-1);
        }
    };

    let (program, _args) = match matches.free.as_slice().split_first() {
        Some((program, args)) => {
            info!("loading module from {}", program);

            (program, args)
        }
        None => {
            eprintln!("missed program file (.wast | .wasm)");
            process::exit(-2);
        }
    };

    let _module = match parity_wasm::deserialize_file(program) {
        Ok(module) => {
            debug!("loaded module: {:?}", module);

            module
        }
        Err(err) => {
            eprintln!("fail to load module, {}", err);
            process::exit(-3);
        }
    };

    if matches.opt_present("check") {
        process::exit(0);
    }
}
