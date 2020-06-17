#![allow(non_upper_case_globals)]

pub mod hasher;
pub mod input;
pub mod lexer;
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
    convert::{TryFrom},
    path::{PathBuf},
};

use clap::{
    Arg, App, SubCommand, ArgMatches,
};

use crate::{
    checker::{CheckerConfig},
    input::{CompressionFormat},
};

fn main() {
    let matches = App::new("tasr")
        .version("1.0")
        .author("Adrian Rebola-Pardo <arpj.rebola@gmail.com>")
        .about("ASR proof checker")
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
            .help("path to the CNF formula file"))
        .arg(Arg::with_name("PROOF")
            .index(2u64)
            .required(true)
            .help("path to the ASR core & proof file"))
        .get_matches();
    let config = config_from_matches(matches);
    println!("{:?}", config);
}

fn config_from_matches(args: ArgMatches) -> CheckerConfig {
    let mut cnf_file = PathBuf::new();
    cnf_file.push(args.value_of("FORMULA").unwrap());
    let mut asr_file = PathBuf::new();
    asr_file.push(args.value_of("PROOF").unwrap());
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
    CheckerConfig {
        cnf_file: cnf_file,
        asr_file: asr_file,
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
