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
    pub tiny_errors: bool,
    pub preloaded_modules: Vec<String>,
    pub link: bool,
    pub out: Option<String>,
}

impl Args {
    pub fn parse_args() -> Args {
        let args = env::args().collect::<Vec<String>>();
        let prog_name = args[0].clone();

        let mut opts = Options::new();
        opts.optflag("h", "help", "print this help menu");
        opts.optflag("v", "version", "print koak's version");
        opts.optmulti("m", "module", "preloads the given module", "MODULE");
        opts.optflag("c", "", "compile and assemble, but do not link");
        opts.optopt("o", "out", "sets the output file name", "NAME");
        opts.optflag("O", "optimization", "enables various optimizations");
        opts.optflag("l", "lexer", "stops after lexing, dumping the generated tokens");
        opts.optflag("p", "parser", "stops after parsing, dumping the generated AST");
        opts.optflag("t", "tiny-errors", "makes errors less verbose");

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
            tiny_errors: matches.opt_present("t"),
            preloaded_modules: matches.opt_strs("m"),
            link: !matches.opt_present("c"),
            out: matches.opt_str("o"),
        }
    }
}
