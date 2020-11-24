#![feature(generic_associated_types)]
#![feature(map_first_last)]
#![feature(maybe_uninit_ref)]
#![feature(ptr_offset_from)]
#![allow(non_upper_case_globals)]
#![allow(incomplete_features)]
#![allow(dead_code)]
#![allow(unused_macros)]

#[macro_use]
extern crate lazy_static;

#[macro_use]
mod io;
mod textparser;
mod basic;
mod database;
mod substitution;
mod mapping;
mod clause;
mod chain;
mod formula;
mod clauseset;
mod proof;
mod tempfile;
mod display;
mod integrity;
mod split;
mod trim;
mod propagation;
mod checker;
mod testgen;

use std::{
    convert::{TryFrom},
    fs::{OpenOptions},
    process::{self},
    sync::{Mutex},
    panic::{self},
    path::{PathBuf, Path},
    io::{Write, Error as IoError},
    num::{NonZeroU64},
};
use clap::{
    Arg, App, AppSettings, Error as ClapError, ErrorKind as ClapErrorKind, Result as ClapResult, SubCommand,
};
use crate::{
    basic::{InstructionNumber, InstructionNumberKind},
    io::{PrintedPanic, InputReader},
    integrity::{IntegrityVerifier, IntegrityStats},
    textparser::{TextAsrParser},
    split::{Splitter, PreprocessingStats, SplittingData},
    trim::{Trimmer},
    tempfile::{SplittingFiles, TrimmingFiles},
    checker::{Checker, CheckingStats},
};

lazy_static! {
    static ref PanicMessage: Mutex<String> = {
        Mutex::new(String::new())
    };
}

enum AppConfig {
    Preprocess(PreprocessingConfig),
    Check(CheckingConfig),
}

struct PreprocessingConfig {
    cnf_path: PathBuf,
    asr_path: PathBuf,
    temp_path: PathBuf,
    output_path: Option<PathBuf>,
    chunk_size: Option<NonZeroU64>,
    print_stats: bool,
}
impl PreprocessingConfig {
    pub fn run(&self) -> bool {
        let intg_stats = self.integrity_check();
        let cnf_lapse = intg_stats.cnf_time.duration_since(intg_stats.start_time);
        let asr_lapse = intg_stats.asr_time.duration_since(intg_stats.cnf_time);
        if self.print_stats {
            info!("Integrity check stats", lock, {
                append!(lock, "{:.<30} {}", "errors", intg_stats.errors.len());
                breakline!(lock);
                append!(lock, "{:.<30} {}ms", "CNF checking runtime", cnf_lapse.as_millis());
                breakline!(lock);
                append!(lock, "{:.<30} {}ms", "ASR checking runtime", asr_lapse.as_millis());
                breakline!(lock);
                append!(lock, "{:.<30} {}", "maximum variable", intg_stats.max_variable.text());
                breakline!(lock);
                append!(lock, "{:.<30} {}", "maximum clause id", intg_stats.max_clauseid.text());
                breakline!(lock);
                append!(lock, "{:.<30} {}", "premises", intg_stats.num_premises.number());
                breakline!(lock);
                append!(lock, "{:.<30} {}", "core clauses", intg_stats.num_cores.number());
                breakline!(lock);
                append!(lock, "{:.<30} {}", "proof instructions", intg_stats.num_instructions.number());
                breakline!(lock);
                append!(lock, "{:.<30} {}", "RUP inferences", intg_stats.num_rup);
                breakline!(lock);
                append!(lock, "{:.<30} {}", "WSR inferences", intg_stats.num_wsr);
                breakline!(lock);
                append!(lock, "{:.<30} {}", "clause deletions", intg_stats.num_del);
                breakline!(lock);
                match intg_stats.first_qed {
                    None => { append!(lock, "{:.<30} {}", "first QED instruction", "not found"); },
                    Some(qed) => { append!(lock, "{:.<30} {} #{}", "first QED instruction", qed.kind(), qed.number()); },
                }
            });
        }
        if intg_stats.is_ok() {
            info!("Integrity check succeeded", lock, {
                append!(lock, "Checked integrity in {}ms, no errors found.", (cnf_lapse + asr_lapse).as_millis());
            });
        } else {
            error!("integrity check failed", lock, {
                append!(lock, "Checked integrity in {}ms, found {} errors.", (cnf_lapse + asr_lapse).as_millis(), intg_stats.errors.len());
            });
            return false;
        }
        let (outpath, pproc_stats) = self.preprocess(intg_stats.first_qed.unwrap());
        let split_lapse = pproc_stats.splitting_time.duration_since(pproc_stats.start_time);
        let trim_lapse = pproc_stats.trimming_time.duration_since(pproc_stats.splitting_time);
        if self.print_stats {
            info!("Preprocessing stats", lock, {
                match pproc_stats.chunk_size {
                    Some(s) => { append!(lock, "{:.<30} {}", "chunk length", s); },
                    None => { append!(lock, "{:.<30} {}", "chunk length", "not specified"); },
                }
                breakline!(lock);
                append!(lock, "{:.<30} {}", "proof chunks", pproc_stats.num_chunks);
                breakline!(lock);
                append!(lock, "{:.<30} {}ms", "splitting runtime", split_lapse.as_millis());
                breakline!(lock);
                append!(lock, "{:.<30} {}ms", "trimming runtime", trim_lapse.as_millis());
                breakline!(lock);
                append!(lock, "{:.<30} {}", "core clauses", pproc_stats.num_cores);
                breakline!(lock);
                append!(lock, "{:.<30} {}", "proof inferences", pproc_stats.num_intros);
                breakline!(lock);
                append!(lock, "{:.<30} {}", "clause deletions", pproc_stats.num_dels);
                breakline!(lock);
                append!(lock, "{:.<30} {}", "preprocessed instructions", pproc_stats.num_instructions);
            });
        }
        info!("Splitting and trimming succeeded", lock, {
            append!(lock, "Split and trimmed proof in {}ms.", (split_lapse + trim_lapse).as_millis());
        });
        success!("proof was successfully preprocessed", lock, {
            append!(lock, "Raw ASR proof '{}' with CNF formula '{}' was preprocessed in {}ms.",
                self.cnf_path.display(), self.asr_path.display(), (cnf_lapse + asr_lapse + split_lapse + trim_lapse).as_millis());
            breakline!(lock);
            append!(lock, "Output preprocessed ASR proof '{}' contains {} instructions", outpath.display(), pproc_stats.num_instructions);
        });
        true
    }
    fn integrity_check(&self) -> IntegrityStats {
        let cnf_parser = {
            let cnf_file = PreprocessingConfig::open_input_file(&self.cnf_path, false, "CNF");
            TextAsrParser::new(cnf_file)
        };
        let asr_parser = {
            let asr_file = PreprocessingConfig::open_input_file(&self.asr_path, false, "ASR");
            TextAsrParser::new(asr_file)
        };
        let intg = IntegrityVerifier::new(None);
        intg.check(cnf_parser, asr_parser).0
    }
    fn preprocess(&self, qed: InstructionNumber) -> (&Path, PreprocessingStats) {
        let mut temp_split = SplittingFiles::new(&self.temp_path, false);
        let splitting_data = self.split(qed, &mut temp_split);
        let temp_trim = temp_split.trimming_files();
        self.trim(splitting_data, temp_trim)
    }
    fn split(&self, qed: InstructionNumber, temp_split: &mut SplittingFiles) -> SplittingData {
        let mut asr_parser = {
            let asr_file = PreprocessingConfig::open_input_file(&self.asr_path, false, "ASR");
            TextAsrParser::new(asr_file)
        };
        let mut splitter = Splitter::new(qed, self.chunk_size, &mut asr_parser).unwrap();
        {
            let mut iter = splitter.split(&mut asr_parser);
            while let Some(chunk) = iter.next() {
                let mut out = temp_split.get();
                chunk.dump(&mut out);
                out.flush().unwrap_or_else(|err| panic!(format!("{}", err)));
            }
        }
        splitter.extract().unwrap()
    }
    fn trim(&self, data: SplittingData, mut temp_trim: TrimmingFiles) -> (&Path, PreprocessingStats) {
        let mut trimmer = Trimmer::new(data);
        while let Some((input, mut output)) = temp_trim.get() {
            let parser = TextAsrParser::new(input);
            let chunk = trimmer.trim_fragment(parser);
            chunk.dump(&mut output);
            output.flush().unwrap_or_else(|err| panic!(format!("{}", err)));
        }
        let output_path = match &self.output_path {
            Some(path) => path,
            None => &self.asr_path,
        };
        let mut temp_out = temp_trim.output(output_path);
        {
            let mut output = temp_out.get();
            write!(&mut output, "p core\n").unwrap_or_else(|err| panic!(format!("{}", err)));
            let core = trimmer.core();
            core.dump(&mut output);
            output.flush().unwrap_or_else(|err| panic!(format!("{}", err)));
            write!(&mut output, "p proof\n").unwrap_or_else(|err| panic!(format!("{}", err)));
        }
        temp_out.flush();
        (output_path, trimmer.extract())
    }
    fn open_input_file<'a>(path: &'a Path, binary: bool, kind: &str) -> InputReader<'a> {
        let file = OpenOptions::new().read(true).open(&path).unwrap_or_else(|err| PreprocessingConfig::opening_error(&path, err, kind));
        InputReader::new(file, path, binary)
    }
    fn opening_error(path: &Path, err: IoError, kind: &str) -> ! {
        panick!("unable to open input file", lock, {
            append!(lock, "Could not open input {} file {}:", kind, path.to_str().unwrap());
            breakline!(lock);
            append!(lock, "{}", err);
        })
    }
}

struct CheckingConfig {
    cnf_path: PathBuf,
    asr_path: PathBuf,
    begin_interval: InstructionNumber,
    end_interval: Option<InstructionNumber>,
    print_stats: bool,
}
impl CheckingConfig {
    fn run(&self) -> bool {
        let (intg_stats, load) = self.integrity_check();
        let cnf_lapse = intg_stats.cnf_time.duration_since(intg_stats.start_time);
        let asr_lapse = intg_stats.asr_time.duration_since(intg_stats.cnf_time);
        if self.print_stats {
            info!("Integrity check stats", lock, {
                append!(lock, "{:.<30} {}", "errors", intg_stats.errors.len());
                breakline!(lock);
                append!(lock, "{:.<30} {}ms", "CNF checking runtime", cnf_lapse.as_millis());
                breakline!(lock);
                append!(lock, "{:.<30} {}ms", "ASR checking runtime", asr_lapse.as_millis());
                breakline!(lock);
                append!(lock, "{:.<30} {}", "maximum variable", intg_stats.max_variable.text());
                breakline!(lock);
                append!(lock, "{:.<30} {}", "maximum clause id", intg_stats.max_clauseid.text());
                breakline!(lock);
                append!(lock, "{:.<30} {}", "premises", intg_stats.num_premises.number());
                breakline!(lock);
                append!(lock, "{:.<30} {}", "core clauses", intg_stats.num_cores.number());
                breakline!(lock);
                append!(lock, "{:.<30} {}", "proof instructions", intg_stats.num_instructions.number());
                breakline!(lock);
                append!(lock, "{:.<30} {}", "RUP inferences", intg_stats.num_rup);
                breakline!(lock);
                append!(lock, "{:.<30} {}", "WSR inferences", intg_stats.num_wsr);
                breakline!(lock);
                append!(lock, "{:.<30} {}", "clause deletions", intg_stats.num_del);
                breakline!(lock);
                match intg_stats.first_qed {
                    None => { append!(lock, "{:.<30} {}", "first QED instruction", "not found"); },
                    Some(qed) => { append!(lock, "{:.<30} {} #{}", "first QED instruction", qed.kind(), qed.number()); },
                }
            });
        }
        if intg_stats.is_ok() {
            info!("Integrity check succeeded", lock, {
                append!(lock, "Checked integrity in {}ms, no errors found.", (cnf_lapse + asr_lapse).as_millis());
            });
        } else {
            error!("integrity check failed", lock, {
                append!(lock, "Checked integrity in {}ms, found {} errors.", (cnf_lapse + asr_lapse).as_millis(), intg_stats.errors.len());
            });
            return false;
        }
        let check_stats = self.correctness_check(load);
        let rewind_lapse = check_stats.rewind_time.duration_since(check_stats.start_time);
        let check_lapse = check_stats.check_time.duration_since(check_stats.rewind_time);
        if self.print_stats {
            info!("Correctness check stats", lock, {
                append!(lock, "{:.<30} {}", "warnings", check_stats.warnings.len());
                breakline!(lock);
                append!(lock, "{:.<30} {}", "errors", check_stats.errors.len());
                breakline!(lock);
                append!(lock, "{:.<30} {}ms", "rewinding runtime", rewind_lapse.as_millis());
                breakline!(lock);
                append!(lock, "{:.<30} {}ms", "proof checking runtime", check_lapse.as_millis());
                breakline!(lock);
                append!(lock, "{:.<30} {}", "checked core clauses", check_stats.cores);
                breakline!(lock);
                append!(lock, "{:.<30} {}", "first checked proof instruction", check_stats.first);
                breakline!(lock);
                append!(lock, "{:.<30} {}", "last checked proof instruction", check_stats.last);
            });
        }
        if check_stats.errors.is_empty() {
            success!("Correctness check succeeded", lock, {
                append!(lock, "Checked proof correctness for ASR proof '{}' with CNF formula '{}' in {}ms, ",
                    self.cnf_path.display(), self.asr_path.display(), (rewind_lapse + check_lapse).as_millis());
                if check_stats.warnings.is_empty() {
                    append!(lock, "no errors or warnings found.");
                } else {
                    append!(lock, "no errors found but {} warnings occurred.", check_stats.warnings.len());
                }
            });
            true
        } else {
            error!("Correctness check failed", lock, {
                append!(lock, "Checked proof correctness for ASR proof '{}' with CNF formula '{}' in {}ms, ",
                    self.cnf_path.display(), self.asr_path.display(), (rewind_lapse + check_lapse).as_millis());
                append!(lock, "{} errors and {} warnings found.", check_stats.errors.len(), check_stats.warnings.len());
            });
            false
        }
    }
    fn integrity_check(&self) -> (IntegrityStats, Vec<InstructionNumber>) {
        let cnf_parser = {
            let cnf_file = PreprocessingConfig::open_input_file(&self.cnf_path, false, "CNF");
            TextAsrParser::new(cnf_file)
        };
        let asr_parser = {
            let asr_file = PreprocessingConfig::open_input_file(&self.asr_path, false, "ASR");
            TextAsrParser::new(asr_file)
        };
        let intg = IntegrityVerifier::new(None);
        intg.check(cnf_parser, asr_parser)
    }
    fn correctness_check(&self, load: Vec<InstructionNumber>) -> CheckingStats {
        let mut cnf_parser = {
            let cnf_file = PreprocessingConfig::open_input_file(&self.cnf_path, false, "CNF");
            TextAsrParser::new(cnf_file)
        };
        let mut asr_parser = {
            let asr_file = PreprocessingConfig::open_input_file(&self.asr_path, false, "ASR");
            TextAsrParser::new(asr_file)
        };
        let checker = Checker::new(&mut cnf_parser, &mut asr_parser, self.begin_interval, self.end_interval, load);
        checker.check(&mut asr_parser)
    }
}

fn main() {
    // let ph = crate::testgen::PigeonHole::new(3);
    // println!("{}", ph);
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
    let app = build_cli_parser();
    let cfg = match parse_cli_arguments(app) {
        Ok(cfg) => cfg,
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
        }
    };
    match cfg {
        AppConfig::Preprocess(subcfg) => subcfg.run(),
        AppConfig::Check(subcfg) => subcfg.run(),
    }
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
                .long("batch_size")
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
            .arg(Arg::with_name("FROM")
                .long("--from")
                .takes_value(true)
                .value_name("N")
                .number_of_values(1u64)
                .help("only checks proof instructions including and after the N-th instruction (use 0 or skip to check the core as well)"))
            .arg(Arg::with_name("UNTIL")
                .long("--until")
                .takes_value(true)
                .value_name("N")
                .number_of_values(1u64)
                .help("only checks proof instructions strictly before the N-th instruction (skip to check up to the end of the proof)"))
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
}

fn parse_cli_arguments<'a, 'b>(app: App<'a, 'b>) -> ClapResult<AppConfig> {
    let matches = app.get_matches_safe()?;
    if let Some(matches) = matches.subcommand_matches("preprocess") {
        let batchsize = if matches.is_present("BATCHSIZE") {
            match matches.value_of("BATCHSIZE").unwrap().parse::<u64>() {
                Ok(num) if num > 0u64 => Ok(NonZeroU64::new(num)),
                _ => Err(ClapError::with_description(&format!(
                    "The value for the argument '--batch_size' must be an integer within [{}..{}].",
                    1u64, u64::max_value()
                ), ClapErrorKind::InvalidValue)),
            }?
        } else {
            None
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
            cnf_path: formula,
            asr_path: proof,
            temp_path: temp,
            output_path: output,
            chunk_size: batchsize,
            print_stats: stats,
        }))
    } else if let Some(matches) = matches.subcommand_matches("check") {
        let from = match matches.value_of("FROM") {
            Some(sfrom) => match sfrom.parse::<u64>() {
                Ok(num) => match InstructionNumber::try_from((InstructionNumberKind::Proof, num)) {
                    Ok(inum) => Ok(inum),
                    Err(_) => Err(ClapError::with_description(&format!(
                        "The value for the argument '--from' must be an integer within [{}..{}].",
                        0u64, InstructionNumber::MaxValue
                    ), ClapErrorKind::InvalidValue)),
                },
                Err(_) => Err(ClapError::with_description(&format!(
                    "The value for the argument '--from' must be an integer within [{}..{}].",
                    0u64, InstructionNumber::MaxValue
                ), ClapErrorKind::InvalidValue)),
            }?,
            None => InstructionNumber::new(InstructionNumberKind::Proof),
        };
        let until = match matches.value_of("UNTIL") {
            Some(suntil) => match suntil.parse::<u64>() {
                Ok(num) => match InstructionNumber::try_from((InstructionNumberKind::Proof, num)) {
                    Ok(inum) => Ok(Some(inum)),
                    Err(_) => Err(ClapError::with_description(&format!(
                        "The value for the argument '--until' must be an integer within [{}..{}].",
                        0u64, InstructionNumber::MaxValue
                    ), ClapErrorKind::InvalidValue)),
                },
                Err(_) => Err(ClapError::with_description(&format!(
                    "The value for the argument '--until' must be an integer within [{}..{}].",
                    0u64, InstructionNumber::MaxValue
                ), ClapErrorKind::InvalidValue)),
            }?,
            None => None,
        };
        let stats = matches.is_present("STATS");
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
            cnf_path: formula,
            asr_path: proof,
            begin_interval: from,
            end_interval: until,
            print_stats: stats,
        }))
    } else {
        Err(ClapError::with_description(&format!("No subcommand.",), ClapErrorKind::InvalidValue))
    }
}
