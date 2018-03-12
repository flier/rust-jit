#[macro_use]
extern crate log;
extern crate pretty_env_logger;
#[macro_use]
extern crate failure;
extern crate getopts;

use std::env;

use failure::Error;
use getopts::{Matches, Options};

fn parse_cmdline(program: &str, args: &[String]) -> Result<Option<Matches>, Error> {
    let mut opts = Options::new();

     opts.optflag("h", "help", "print this help menu");

    let matches = opts.parse(args)?;

    if matches.opt_present("h") {
        let brief = format!("Usage: {} FILE [options]", program);

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

    match parse_cmdline(program, args) {
        Ok(Some(opts)) => {
        }
        Ok(None) => {}
        Err(err) => eprintln!("Fail to parse command line, {}", err),
    }
}
