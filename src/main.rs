#![allow(non_upper_case_globals)]

#[macro_use]
extern crate lazy_static;

pub mod hasher;
pub mod bst;
pub mod input;
#[macro_use]
pub mod logger;
pub mod variable;
pub mod basic;
pub mod parser;
pub mod assignment;
pub mod clausedb;
pub mod chaindb;
pub mod unitpropagation;
pub mod results;
pub mod checker;

use std::{
    io::{self, Stdin},
    convert::{TryFrom},
    path::{PathBuf},
    process::{self},
    panic::{self},
    sync::{Mutex},
};

use colored::{
    Colorize,
};

use crate::{
    logger::{LoggerId, LoggerSlot, Logger, LoggerLevel, LoggerThreshold},
    input::{CompressionFormat, OutputStream, InputStream},
    checker::{Trimming, AsrChecker, CheckerConfig, CheckerStats},
    parser::{DimacsParser, AsrParser, VbeParser},
};

use clap::{
    Arg, App, AppSettings, Error as ClapError, ErrorKind as ClapErrorKind, Result as ClapResult,
};

lazy_static! {
    static ref PanicInfo: Mutex<String> = {
        Mutex::new(String::new())
    };
}


#[derive(Debug)]
pub struct AppConfig {
	pub cnf_path: Option<PathBuf>,
    pub asr_path: Option<PathBuf>,
    pub fix_path: Option<PathBuf>,
	pub cnf_compression: CompressionFormat,
    pub asr_compression: CompressionFormat,
    pub fix_compression: CompressionFormat,
	pub cnf_binary: bool,
    pub asr_binary: bool,
    pub fix_binary: bool,
	pub check_core: bool,
	pub check_derivation: bool,
    pub check_refutation: bool,
    pub core_limit: usize,
    pub proof_limit: usize,
    pub trimming: Trimming,
    pub print_stats: bool,
}

fn main() {
    // panic::set_hook(Box::new(|info| {
    //     PanicInfo.lock().unwrap().push_str(&format!("{}", info));
    // }));
    let result = panic::catch_unwind(run);
    match result {
        Ok(_) => process::exit(0),
        Err(pain) => {
            match pain.downcast::<String>() {
                Ok(msg) => eprintln!("{} {}", "Fatal error:".red().bold().underline(), msg),
                Err(_) => {
                    eprintln!("{} {}", "Fatal error:".red().bold().underline(), "unexpected runtime error.");
                    eprintln!("Info: {}", PanicInfo.lock().unwrap());
                },
            }
            process::exit(1);
        },
    }
}

fn run() -> Option<CheckerStats> {
    let app = build_cli_parser();
    let config_result = parse_cli_arguments(app);
    let (mut logger, output_slot, error_slot, _fix_slot) = setup_logger(&config_result);
    let config = match &config_result {
        Ok(config) => config,
        Err(err) if err.use_stderr() => {
            let n = match err.message.chars().enumerate().find(|&(_, c)| c.is_ascii_whitespace()) {
                Some((n, _)) => n + 1usize,
                None => 0usize,
            };
            log_error!(logger.handle(error_slot), target: "Error"; "{}", &err.message[n..]);
            return None;
        },
        Err(err) => {
            log_error!(logger.handle(output_slot); "{}", err);
            return None;
        }
    };
    let check_result = {
        let stdin = io::stdin();
        let (mut cnf_input, mut asr_input) = setup_inputs(&config, &stdin);
        check_proof(&config, &mut *cnf_input, &mut *asr_input, &mut logger, error_slot)
    };
    output_results(&config, &check_result, &mut logger, output_slot);
    logger.flush(output_slot);
    logger.flush(error_slot);
    Some(check_result)
}

fn build_cli_parser<'a, 'b>() -> App<'a, 'b> {
    App::new(env!("CARGO_PKG_NAME"))
        .setting(AppSettings::ColoredHelp)
        .set_term_width(0usize)
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
            .help("reads ASR proof in binary format"))
        .arg(Arg::with_name("OUTPUT_BINARY")
            .long("output-binary")
            .requires("OUTPUT")
            .takes_value(false)
            .help("generates output ASR proof in binary format"))
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
        .arg(Arg::with_name("OUTPUT_COMPRESSION")
            .long("output-compression")
            .value_name("FORMAT")
            .takes_value(true)
            .number_of_values(1u64)
            .requires("OUTPUT")
            .possible_values(&["plain", "zst", "gz", "bz2", "xz", "lz4"])
            .help("writes output ASR proof in the given compression format"))
        .arg(Arg::with_name("ADMIT_CORE")
            .long("admit-core")
            .takes_value(false)
            .help("skips ASR core checking"))
        .arg(Arg::with_name("ADMIT_DERIVATION")
            .long("admit-derivation")
            .takes_value(false)
            .help("skips ASR proof inference correctness checking"))
        .arg(Arg::with_name("ADMIT_REFUTATION")
            .long("admit-refutation")
            .takes_value(false)
            .help("skips ASR proof contradiction checking"))
        .arg(Arg::with_name("PARTIAL_CORE")
            .long("partial-core")
            .value_name("N")
            .takes_value(true)
            .help("considers only the first N core clauses"))
        .arg(Arg::with_name("PARTIAL_PROOF")
            .long("partial-proof")
            .value_name("N")
            .takes_value(true)
            .help("considers only the first N proof instruction"))
        .arg(Arg::with_name("TRIM")
            .long("trim")
            .takes_value(false)
            .requires("OUTPUT")
            .conflicts_with("CLEANUP")
            .help("outputs a trimmed and cleaned-up ASR proof"))
        .arg(Arg::with_name("CLEANUP")
            .long("cleanup")
            .takes_value(false)
            .requires("OUTPUT")
            .help("outputs a cleaned-up ASR proof (i.e. tries to fix uncompliant proofs)"))
        .arg(Arg::with_name("OUTPUT")
            .long("output")
            .value_name("FILE")
            .takes_value(true)
            .number_of_values(1u64)
            .help("file path to output a processed ASR proof"))
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
}

fn parse_cli_arguments<'a, 'b>(app: App<'a, 'b>) -> ClapResult<AppConfig> {
    let args = app.get_matches_safe()?;
    match (args.value_of("FORMULA"), args.is_present("CNF_COMPRESSION"), args.is_present("CNF_BINARY")) {
        (Some("*"), true, _) => {
            Err(ClapError::with_description("The argument '--cnf-compression' cannot be used when the argument '<FORMULA>' has the dummy value '*'.", ClapErrorKind::ArgumentConflict))?
        },
        (Some("*"), _, true) => {
            Err(ClapError::with_description("The argument '--cnf-binary' cannot be used when the argument '<FORMULA>' has the dummy value '*'.", ClapErrorKind::ArgumentConflict))?
        }
        _ => (),
    }
    if args.is_present("PROOF") && args.is_present("ASR_COMPRESSION") {
        Err(ClapError::with_description("The argument '--asr-compression' requires the argument '<PROOF>'.", ClapErrorKind::ArgumentConflict))?
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
    let asr_file = args.value_of("PROOF").map(|path| {
        let mut pb = PathBuf::new();
        pb.push(path);
        pb
    });
    let fix_file = args.value_of("OUTPUT").map(|path| {
        let mut pb = PathBuf::new();
        pb.push(path);
        pb
    });
    let cnf_binary = args.is_present("CNF_BINARY");
    let asr_binary = args.is_present("ASR_BINARY");
    let fix_binary = args.is_present("OUTPUT_BINARY");
    let cnf_compression = CompressionFormat::try_from(args.value_of("CNF_COMPRESSION")).unwrap();
    let asr_compression = CompressionFormat::try_from(args.value_of("ASR_COMPRESSION")).unwrap();
    let fix_compression = CompressionFormat::try_from(args.value_of("OUTPUT_COMPRESSION")).unwrap();
    let check_core = !args.is_present("ADMIT_CORE");
    let check_derivation = !args.is_present("ADMIT_DERIVATION");
    let check_refutation = !args.is_present("ADMIT_REFUTATION");
    let stats = args.is_present("STATS");
    let core_limit = match args.value_of("PARTIAL_CORE") {
        None => usize::max_value(),
        Some(num) => match num.parse::<usize>() {
            Ok(n) => n,
            Err(_) => Err(ClapError::with_description("The argument '--partial-core' requires a non-negative number as argument.", ClapErrorKind::InvalidValue))?,
        },
    };
    let proof_limit = match args.value_of("PARTIAL_PROOF") {
        None => usize::max_value(),
        Some(num) => match num.parse::<usize>() {
            Ok(n) => n,
            Err(_) => Err(ClapError::with_description("The argument '--partial-proof' requires a non-negative number as argument.", ClapErrorKind::InvalidValue))?,
        },
    };
    let trimming = if args.is_present("TRIM") {
        Trimming::Trimming
    } else if args.is_present("CLEANUP") {
        Trimming::Cleanup
    } else {
        Trimming::Nothing
    };
    Ok(AppConfig {
        cnf_path: cnf_file,
        asr_path: asr_file,
        fix_path: fix_file,
        cnf_compression: cnf_compression,
        asr_compression: asr_compression,
        fix_compression: fix_compression,
        cnf_binary: cnf_binary,
        asr_binary: asr_binary,
        fix_binary: fix_binary,
        check_core: check_core,
        check_derivation: check_derivation,
        check_refutation: check_refutation,
        core_limit: core_limit,
        proof_limit: proof_limit,
        trimming: trimming,
        print_stats: stats,
    })
}

fn setup_logger<'a, 'b: 'a>(config_result: &'b Result<AppConfig, ClapError>) -> (Logger<'a>, LoggerId, LoggerId, LoggerId) {
    let fix_file = match config_result {
        Ok(config) => match &config.fix_path {
            Some(path) => Some((path.as_path(), config.fix_binary, config.fix_compression)),
            None => None,
        },
        Err(_) => None,
    };
    let mut logger = Logger::<'a>::new(LoggerThreshold::Info);
    let output_slot = logger.insert(LoggerSlot::new(
        OutputStream::new(io::stdout()), LoggerThreshold::Info, logger_format!(md, msg {
            md.target() == "Result" => ("{} {}\n", "Result:".green().bold().underline(), format!("{}", msg).bold())
            md.target() == "Stats" => ("{}\n{}\n", "Stats:".blue().bold().underline(), msg)
            md.target() == "Warning" =>  ("{} {}\n", "Warning:".yellow().bold().underline(), msg)
            md.target() == "Error" =>  ("{} {}\n", "Error:".red().bold().underline(), msg)
            _ => ("{}\n", msg)
        })
    ));
    let error_slot = logger.insert(LoggerSlot::new(
        OutputStream::new(io::stderr()), LoggerThreshold::Info, logger_format!(md, msg {
            md.target() == "Warning" =>  ("{} {}\n", "Warning:".yellow().bold().underline(), msg)
            md.target() == "Error" =>  ("{} {}\n", "Error:".red().bold().underline(), msg)
            _ => ("{}\n", msg)
        })
    ));
    let fix_slot = logger.insert(match &fix_file {
        Some((path, true, format)) => LoggerSlot::new(OutputStream::open(path, *format), LoggerThreshold::Output, logger_format!(_, msg {
            _ => ("{}", msg)
        })),
        Some((path, false, format)) => LoggerSlot::new(OutputStream::open(path, *format), LoggerThreshold::Output, logger_format!(_, msg {
            _ => ("{}\n", msg)
        })),
        None => LoggerSlot::null(),
    });
    (logger, output_slot, error_slot, fix_slot)
}

fn setup_inputs<'a, 'b: 'a>(config: &'b AppConfig, stdin: &'b Stdin) -> (Box<dyn 'a + AsrParser>, Box<dyn 'a + AsrParser>) {
    let cnf_input = match &config.cnf_path {
        Some(path) => InputStream::<'_>::open(&path, config.cnf_compression, config.cnf_binary),
        None => InputStream::<'_>::string("p cnf 0 0", "(dummy)", false),
    };
    let cnf_parser: Box<dyn AsrParser> = if config.cnf_binary && config.cnf_path.is_some() {
        Box::new(VbeParser::<'_>::new(cnf_input, "CNF"))
    } else {
        Box::new(DimacsParser::<'_>::new(cnf_input, "CNF"))
    };
    let asr_input = match &config.asr_path {
        Some(path) => InputStream::<'_>::open(&path, config.asr_compression, config.asr_binary),
        None => InputStream::<'_>::stdin(stdin, config.asr_binary),
    };
    let asr_parser: Box<dyn AsrParser> = if config.asr_binary {
        Box::new(VbeParser::<'_>::new(asr_input, "ASR"))
    } else {
        Box::new(DimacsParser::<'_>::new(asr_input, "ASR"))
    };
    (cnf_parser, asr_parser)
}

fn check_proof(config: &AppConfig, cnf: &mut dyn AsrParser, asr: &mut dyn AsrParser, logger: &mut Logger, error: LoggerId) -> CheckerStats {
    let checker_config = CheckerConfig {
        check_core: config.check_core,
        check_derivation: config.check_derivation,
        check_refutation: config.check_refutation,
        core_limit: config.core_limit,
        proof_limit: config.proof_limit,
        trimming: config.trimming,
        output: error,
    };
    let checker = AsrChecker::<'_, '_>::new(cnf, asr, logger, checker_config);
    checker.check()
}

fn output_results(config: &AppConfig, stats: &CheckerStats, logger: &mut Logger, output: LoggerId) {
    let (warnings, errors) = stats.errors.count();
    if errors == 0usize {
        log_output!(logger.handle(output), target: "Result"; "{} (checked in {}s with {} errors and {} warnings)",
        format!("{}", "verified proof".bold()),
        stats.time_trimming.duration_since(stats.time_start).as_secs(),
        errors,
        warnings);
    } else {
        log_output!(logger.handle(output), target: "Result"; "{} [checked in {}s with {} errors and {} warnings]",
        format!("{}", "incorrect proof".bold()),
        stats.time_trimming.duration_since(stats.time_start).as_secs(),
        errors,
        warnings);
    }
    if config.print_stats {
        log_output!(logger.handle(output), target: "Stats"; "{}", stats);
    }
}