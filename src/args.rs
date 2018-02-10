//!
//! Koak's argument parsing
//!

use std::env;
use std::process;

use getopts::Options;

#[derive(Debug)]
pub struct Args {
    pub input: Vec<String>,
    pub stop_after_lexer: bool,
    pub stop_after_parser: bool,
    pub optimization: bool,
}

impl Args {
    pub fn parse_args() -> Args {
        let args = env::args().collect::<Vec<String>>();
        let prog_name = args[0].clone();

        let mut opts = Options::new();
        opts.optflag("h", "help", "print this help menu");
        opts.optflag("v", "version", "print koak's version");
        opts.optflag("l", "lexer", "stops after lexing, dumping the generated tokens");
        opts.optflag("p", "parser", "stops after parsing, dumping the generated AST");
        opts.optflag("O", "optimization", "enables various optimizations");

        let matches = match opts.parse(&args[1..]) {
            Ok(m) => m,
            Err(e) => {
                eprintln!("{}", e);
                process::exit(1);
            }
        };

        // Quick help & version handlers
        if matches.opt_present("h") {
            let f = format!("{} [options] [file]", prog_name);
            println!("{}", opts.usage(&f));
            process::exit(0);
        }
        else if matches.opt_present("v") {
            println!("Koak - Kind Of Alternative Kompiler");
            println!("Written by Benjamin Grange, Jakob Kellendonk and Walter Bonetti");
            process::exit(0);
        }

        let input = matches.free.clone();

        Args {
            input: input,
            stop_after_lexer: matches.opt_present("l"),
            stop_after_parser: matches.opt_present("p"),
            optimization: matches.opt_present("O"),
        }
    }
}
