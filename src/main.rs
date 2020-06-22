#![allow(non_upper_case_globals)]

pub mod hasher;
pub mod input;
pub mod parser;
pub mod variable;
pub mod assignment;
pub mod chunkdb;
pub mod clausedb;
pub mod unitpropagation;
pub mod chaindb;
pub mod results;
pub mod checker;

use std::{
    io::{self},
    convert::{TryFrom},
    path::{PathBuf},
};

use clap::{
    Arg, App, ArgMatches, Error as ClapError, ErrorKind as ClapErrorKind,
};

use crate::{
    checker::{CheckerConfig, AsrChecker, CheckerInputConfig, CheckingResult},
    input::{CompressionFormat},
};

#[derive(Debug)]
pub struct AppConfig {
	pub cnf_path: Option<PathBuf>,
	pub asr_path: Option<PathBuf>,
	pub cnf_compression: CompressionFormat,
	pub asr_compression: CompressionFormat,
	pub cnf_binary: bool,
	pub asr_binary: bool,
	pub check_core: bool,
	pub check_derivation: bool,
	pub check_refutation: bool,
	pub permissive: bool,
	pub print_stats: bool,
}
impl<'a> From<ArgMatches<'a>> for AppConfig {
    fn from(args: ArgMatches<'a>) -> AppConfig {
        match (args.value_of("FORMULA"), args.is_present("CNF_COMPRESSION"), args.is_present("CNF_BINARY")) {
            (Some("*"), true, _) => {
                ClapError::with_description("The argument '--cnf-compression' cannot be used when the argument '<FORMULA>' has the dummy value '*'.", ClapErrorKind::ArgumentConflict).exit();
            },
            (Some("*"), _, true) => {
                ClapError::with_description("The argument '--cnf-binary' cannot be used when the argument '<FORMULA>' has the dummy value '*'.", ClapErrorKind::ArgumentConflict).exit();
            }
            _ => (),
        }
        if args.is_present("PROOF") && args.is_present("ASR_COMPRESSION") {
            ClapError::with_description("The argument '--asr-compression' requires the argument '<PROOF>'.", ClapErrorKind::ArgumentConflict).exit();
        }
        let cnf_file = {
            let path = args.value_of("FORMULA").unwrap();
            if path == "*" {
                None
            } else {
                let mut pb = PathBuf::new();
                pb.push(path);
                Some(pb)
            }
        };
        let asr_file = match args.value_of("PROOF") {
            None => None,
            Some(path) => {
                let mut pb = PathBuf::new();
                pb.push(path);
                Some(pb)
            }
        };
        let cnf_binary = args.is_present("CNF_BINARY");
        let asr_binary = args.is_present("ASR_BINARY");
        let cnf_compression = CompressionFormat::try_from(args.value_of("CNF_COMPRESSION")).unwrap();
        let asr_compression = CompressionFormat::try_from(args.value_of("ASR_COMPRESSION")).unwrap();
        let permissive = args.is_present("PERMISSIVE");
        let check = args.is_present("TASK");
        let mut check_core = !check;
        let mut check_derivation = !check;
        let mut check_refutation = !check;
        if check {
            let it = args.values_of("TASK").unwrap();
            for val in it {
                if val == "core" {
                    check_core = true;
                } else if val == "derivation" {
                    check_derivation = true;
                } else if val == "refutation" {
                    check_refutation = true;
                }
            }
        }
        let stats = args.is_present("STATS");
        AppConfig {
            cnf_path: cnf_file,
            asr_path: asr_file,
            cnf_compression: cnf_compression,
            asr_compression: asr_compression,
            cnf_binary: cnf_binary,
            asr_binary: asr_binary,
            permissive: permissive,
            check_core: check_core,
            check_derivation: check_derivation,
            check_refutation: check_refutation,
            print_stats: stats,
        }
    }
}

fn main() {
    let matches = App::new(env!("CARGO_PKG_NAME"))
        .version(env!("CARGO_PKG_VERSION"))
        .author(env!("CARGO_PKG_AUTHORS"))
        .about(env!("CARGO_PKG_DESCRIPTION"))
        .arg(Arg::with_name("CNF_BINARY")
            .long("cnf-binary")
            .takes_value(false)
            .help("reads CNF formula in binary format"))
        .arg(Arg::with_name("ASR_BINARY")
            .long("asr-binary")
            .takes_value(false)
            .help("reads ASR formula in binary format"))
        .arg(Arg::with_name("CNF_COMPRESSION")
            .long("cnf-compression")
            .value_name("FORMAT")
            .takes_value(true)
            .number_of_values(1u64)
            .possible_values(&["plain", "zst", "gz", "bz2", "xz", "lz4"])
            .help("reads CNF formula in the given compression format"))
        .arg(Arg::with_name("ASR_COMPRESSION")
            .long("asr-compression")
            .value_name("FORMAT")
            .takes_value(true)
            .number_of_values(1u64)
            .possible_values(&["plain", "zst", "gz", "bz2", "xz", "lz4"])
            .help("reads ASR proof in the given compression format"))
        .arg(Arg::with_name("PERMISSIVE")
            .long("permissive")
            .takes_value(false)
            .help("admits some format variations not belonging to the ASR standard"))
        .arg(Arg::with_name("TASK")
            .long("task")
            .value_name("TASK")
            .takes_value(true)
            .multiple(true)
            .number_of_values(1u64)
            .possible_values(&["core", "derivation", "refutation"])
            .help("performs a specific task instead of a full proof check; use separate flags to specify several tasks"))
        .arg(Arg::with_name("STATS")
            .long("stats")
            .takes_value(false)
            .help("prints proof statistics after running"))
        .arg(Arg::with_name("FORMULA")
            .index(1u64)
            .required(true)
            .help("path to the CNF formula file; \"*\" specifies an empty CNF formula"))
        .arg(Arg::with_name("PROOF")
            .index(2u64)
            .help("path to the ASR core & proof file; standard input is read if omitted"))
        .get_matches();
    let config = AppConfig::from(matches);
    let result = check(&config);
    report(&result, &config);
}

fn check(config: &AppConfig) -> CheckingResult {
    let cnf = CheckerInputConfig {
        path: config.cnf_path.clone(),
        compression: config.cnf_compression,
        binary: config.cnf_binary,
    };
    let asr = CheckerInputConfig {
        path: config.asr_path.clone(),
        compression: config.asr_compression,
        binary: config.asr_binary,
    };
    let chconfig = CheckerConfig {
        permissive: config.permissive,
        check_core: config.check_core,
        check_derivation: config.check_derivation,
        check_refutation: config.check_refutation,
    };
    let stdin = io::stdin();
    AsrChecker::<'_>::process(cnf, asr, &stdin, chconfig)
}

fn report(result: &CheckingResult, config: &AppConfig) {
    match &result.final_result() {
        Some(true) => println!("s Verified"),
        Some(false) => println!("s Incorrect"),
        None => println!("s Error"),
    }
    match &result.result {
        Ok(()) => (),
        Err(err) => if err.binary(config.cnf_binary, config.asr_binary) {
            println!("{}", err)
        } else {
            println!("{:b}", err)
        },
    }
    if config.print_stats {
        if result.result.is_err() {
            println!("Warning: statistics are only measured up to the failure point.")
        }
        println!("{}", &result.stats)
    }
}