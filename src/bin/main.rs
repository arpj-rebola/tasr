#![feature(generic_associated_types)]
#![feature(map_first_last)]
#![feature(maybe_uninit_ref)]
#![feature(ptr_offset_from)]
#![feature(test)]
#![allow(non_upper_case_globals)]
#![allow(incomplete_features)]
#![allow(dead_code)]
#![allow(unused_macros)]

#[macro_use]
extern crate lazy_static;

use std::{
    process::{self},
    sync::{Mutex},
    panic::{self},
    path::{PathBuf},
};

use clap::{
    Arg, App, AppSettings, Error as ClapError, ErrorKind as ClapErrorKind, Result as ClapResult, SubCommand,
};

use tasr::{
    progress, fatal, panick, create_message, headed_message, append, breakline,
    io::{PrintedPanic},
    app::{PreprocessingConfig, CheckingConfig},
};

lazy_static! {
    static ref PanicMessage: Mutex<String> = {
        Mutex::new(String::new())
    };
}

pub enum AppConfig {
    Preprocess(PreprocessingConfig),
    Check(CheckingConfig),
}
impl AppConfig {
    pub fn run(self) -> bool {
        match self {
            AppConfig::Preprocess(mut config) => {
                progress!("checking file integrity...", lock, {
                    append!(lock, "Checking the CNF file {} and raw ASR file {} for format errors and undefined references.", config.cnf.display(), config.asr.display());
                });
                config.integrity();
                progress!("preprocessing ASR proof...", lock, {
                    append!(lock, "Preprocessing the raw ASR file {} into the preprocessed ASR file {} to reuse indices, trim unnecessary inferences, and reduce complex proof steps.",
                        config.asr.display(), config.output.as_ref().map(|x| x.display()).unwrap_or_else(|| config.asr.display()));
                });
                config.preprocess();
                config.print_integrity_stats();
                config.print_preprocessing_stats();
                config.print_final_result();
                config.total_errors() == 0usize
            },
            AppConfig::Check(mut config) => {
                config.integrity();
                config.correctness();
                config.print_integrity_stats();
                config.print_correctness_stats();
                config.print_final_result();
                config.total_errors() == 0usize
            },
        }
    }
}

struct Tasr {
    config: AppConfig,
}
impl Tasr {
    pub fn parse_cli() -> ClapResult<Tasr> {
        let app = Tasr::build_cli_parser();
        let config = Tasr::parse_cli_options(app)?;
        Ok(Tasr {
            config: config,
        })
    }
    pub fn run(self) -> bool {
        self.config.run()
    }
    fn build_cli_parser<'a, 'b>() -> App<'a, 'b> {
        App::new(env!("CARGO_PKG_NAME"))
        .setting(AppSettings::ColoredHelp)
        .setting(AppSettings::SubcommandRequired)
        .set_term_width(80usize)
        .version(env!("CARGO_PKG_VERSION"))
        .author(env!("CARGO_PKG_AUTHORS"))
        .about(env!("CARGO_PKG_DESCRIPTION"))
        .subcommand(SubCommand::with_name("preprocess")
            .setting(AppSettings::ColoredHelp)
            .about("preprocesses an ASR proof for more efficient checking")
            .arg(Arg::with_name("BATCHSIZE")
                .long("batchsize")
                .takes_value(true)
                .value_name("N")
                .number_of_values(1u64)
                .help("number of instructions per preprocessing batch (recommended for large proofs)"))
            .arg(Arg::with_name("TEMPDIR")
                .long("temp")
                .takes_value(true)
                .value_name("TEMP")
                .number_of_values(1u64)
                .help("path for temporary files (proof directory is used if missing)"))
            .arg(Arg::with_name("OUTPUT")
                .long("out")
                .takes_value(true)
                .value_name("OUTPUT")
                .number_of_values(1u64)
                .help("path for output preprocessed file (proof file is overwriten if missing)"))
            .arg(Arg::with_name("STATS")
                .long("stats")
                .help("prints statistics"))
            .arg(Arg::with_name("FORMULA")
                .index(1u64)
                .required(true)
                .help("path to the input CNF formula file"))
            .arg(Arg::with_name("PROOF")
                .index(2u64)
                .required(true)
                .help("path to the input ASR proof file")))
        .subcommand(SubCommand::with_name("check")
            .setting(AppSettings::ColoredHelp)
            .about("checks an ASR proof (or a fragment thereof)")
            .arg(Arg::with_name("PART")
                .long("--part")
                .takes_value(true)
                .value_names(&["N", "TOTAL"])
                .number_of_values(2u64)
                .help("checks only part N of TOTAL parts"))
            .arg(Arg::with_name("STATS")
                .long("stats")
                .help("prints statistics"))
            .arg(Arg::with_name("PERMISSIVE")
                .long("--permissive")
                .help("disables warnings for correct but redundant RUP chains"))
            .arg(Arg::with_name("FORMULA")
                .index(1u64)
                .required(true)
                .help("path to the input CNF formula file"))
            .arg(Arg::with_name("PROOF")
                .index(2u64)
                .required(true)
                .help("path to the input ASR proof file")))
    }
    fn parse_cli_options(app: App<'_, '_>) -> ClapResult<AppConfig> {
        let matches = app.get_matches_safe()?;
        if let Some(matches) = matches.subcommand_matches("preprocess") {
            let batchsize = if matches.is_present("BATCHSIZE") {
                match matches.value_of("BATCHSIZE").unwrap().parse::<u64>() {
                    Ok(num) if num > 0u64 => num,
                    _ => Err(ClapError::with_description(&format!(
                        "The value for the argument '--batchsize' must be an integer within [{}..{}].",
                        1u64, u64::max_value()
                    ), ClapErrorKind::InvalidValue))?,
                }
            } else {
                u64::max_value()
            };
            let formula = {
                let mut pb = PathBuf::new();
                pb.push(matches.value_of("FORMULA").unwrap());
                pb
            };
            let proof = {
                let mut pb = PathBuf::new();
                pb.push(matches.value_of("PROOF").unwrap());
                pb
            };
            let temp = match matches.value_of("TEMPDIR") {
                Some(val) => {
                    let mut pb = PathBuf::new();
                    pb.push(val);
                    pb
                },
                None => PathBuf::from(formula.parent().unwrap_or(&formula)),
            };
            let output = match matches.value_of("OUTPUT") {
                Some(val) => {
                    let mut pb = PathBuf::new();
                    pb.push(val);
                    Some(pb)
                },
                None => None,
            };
            let stats = matches.is_present("STATS");
            Ok(AppConfig::Preprocess(PreprocessingConfig {
                cnf: formula,
                cnf_binary: false,
                asr: proof,
                asr_binary: false,
                temp: temp,
                output: output,
                chunk: batchsize,
                stats: stats,
                integrity: None,
                data: None,
                preprocessing: None,
            }))
        } else if let Some(matches) = matches.subcommand_matches("check") {
            let (part, total) = match matches.values_of("PART") {
                Some(mut it) => {
                    let part = match it.next().unwrap().parse::<u64>() {
                        Ok(num) => num,
                        Err(_) => Err(ClapError::with_description(&format!(
                            "The first value for the argument '--part' must be an integer within [{}..{}].", 0u64, u64::max_value()),
                            ClapErrorKind::InvalidValue))?,
                    };
                    let total = match it.next().unwrap().parse::<u64>() {
                        Ok(num) if num >= 1u64 => num,
                        _ => Err(ClapError::with_description(&format!(
                            "The first value for the argument '--part' must be an integer within [{}..{}].", 1u64, u64::max_value()),
                            ClapErrorKind::InvalidValue))?,
                    };
                    if part >= total {
                        Err(ClapError::with_description(
                            "The first value for the argument '--part' must be strictly smaller than the second value.", ClapErrorKind::InvalidValue))?;
                    }
                    (part, total)
                },
                None => (0u64, 1u64),
            };
            let stats = matches.is_present("STATS");
            let permissive = matches.is_present("PERMISSIVE");
            let formula = {
                let mut pb = PathBuf::new();
                pb.push(matches.value_of("FORMULA").unwrap());
                pb
            };
            let proof = {
                let mut pb = PathBuf::new();
                pb.push(matches.value_of("PROOF").unwrap());
                pb
            };
            Ok(AppConfig::Check(CheckingConfig {
                cnf: formula,
                cnf_binary: false,
                asr: proof,
                asr_binary: false,
                permissive: permissive,
                select: part,
                parts: total,
                stats: stats,
                integrity: None,
                data: None,
                correctness: None,
            }))
        } else {
            Err(ClapError::with_description(&format!("No subcommand was given.",), ClapErrorKind::InvalidValue))
        }
    }
}

fn main() {
    panic::set_hook(Box::new(|info| {
        *PanicMessage.lock().unwrap() = format!("{}", info)
    }));
    match panic::catch_unwind(run) {
        Ok(success) => if success {
            process::exit(0)
        } else {
            process::exit(1)
        },
        Err(pain) => {
            match pain.downcast::<PrintedPanic>() {
                Ok(pp) => eprint!("{}", pp),
                Err(_) => {
                    fatal!("unexpected runtime error", lock, {
                        append!(lock, "{}", PanicMessage.lock().unwrap());
                    })
                },
            }
            process::exit(101)
        }
    }
}

fn run() -> bool {
    match Tasr::parse_cli() {
        Ok(cfg) => cfg.run(),
        Err(err) => if err.use_stderr() {
            panick!("command-line argument error", lock, {
                let s = format!("{}", err);
                let n = match err.message.chars().enumerate().find(|&(_, c)| c.is_ascii_whitespace()) {
                    Some((n, _)) => n + 1usize,
                    None => 0usize,
                };
                append!(lock, "{}", &s[n ..]);
            })
        } else {
            err.exit()
        },
    }
}




// use std::path::{Path};

// fn run_benchmark(path: &str) {
//     run_preprocessing(path);
//     run_checking(path);
// }

// fn run_preprocessing(basename: &str) {
//     let cnf_string = format!("benches/{}.cnf", basename);
//     let raw_string = format!("benches/{}.raw", basename);
//     let asr_string = format!("temp/{}.asr", basename);
//     let mut pp = PreprocessingConfig {
//         cnf: Path::new(&cnf_string).to_path_buf(),
//         cnf_binary: false,
//         asr: Path::new(&raw_string).to_path_buf(),
//         asr_binary: false,
//         temp: Path::new("temp/").to_path_buf(),
//         output: Some(Path::new(&asr_string).to_path_buf()),
//         chunk: 1000000u64,
//         stats: true,
//         data: None,
//         integrity: None,
//         preprocessing: None,
//     };
//     pp.integrity();
//     pp.preprocess();
// }
// fn run_checking(basename: &str) {
//     let cnf_string = format!("benches/{}.cnf", basename);
//     let asr_string = format!("temp/{}.asr", basename);
//     let mut chk = CheckingConfig {
//         cnf: Path::new(&cnf_string).to_path_buf(),
//         cnf_binary: false,
//         asr: Path::new(&asr_string).to_path_buf(),
//         asr_binary: false,
//         permissive: true,
//         select: 0u64,
//         parts: 1u64,
//         stats: true,
//         data: None,
//         integrity: None,
//         correctness: None,
//     };
//     chk.integrity();
//     chk.correctness();
// }

// fn main() {
//     let list = vec![
//         "3bitadd_32.cnf.gz.CP3-cnfmiter",
//         "6s153",
//         "baseballcover15with25",
//         "dlx1c.ucl.sat.chaff.4.1.bryant",
//         "f6bidw",
//         "fclqcolor-18-14-11.cnf.gz.CP3-cnfmiter",
//         "fclqcolor-20-15-12.cnf.gz.CP3-cnfmiter",
//         "jkkk-one-one-10-30-unsat",
//         "ls15-normalized.cnf.gz.CP3-cnfmiter",
//         "ps_200_300_70",
//         "ps_200_301_70",
//         "ps_200_305_70",
//         "ps_200_306_70",
//         "ps_200_316_70",
//         "ps_200_317_70",
//         "Steiner-15-7-bce",
//         "Steiner-27-10-bce",
//         "Steiner-45-16-bce",
//         "Steiner-9-5-bce"
//     ];
//     for item in &list {
//         println!("running {}", item);
//         run_benchmark(item);
//     }
// }
