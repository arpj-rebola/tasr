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
mod proof;
mod tempfile;
mod display;
mod integrity;
mod split;
mod trim;

use std::{
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
    basic::{InstructionNumber},
    io::{PrintedPanic, InputReader},
    integrity::{IntegrityVerifier, IntegrityStats},
    textparser::{TextAsrParser},
    split::{Splitter, PreprocessingStats, SplittingData},
    trim::{Trimmer},
    tempfile::{SplittingFiles, TrimmingFiles},
};

lazy_static! {
    static ref PanicMessage: Mutex<String> = {
        Mutex::new(String::new())
    };
}

enum AppConfig {
    Preprocess(PreprocessingConfig),
}

struct PreprocessingConfig {
    cnf_path: PathBuf,
    asr_path: PathBuf,
    temp_path: PathBuf,
    output_path: Option<PathBuf>,
    chunk_size: Option<NonZeroU64>,
}
impl PreprocessingConfig {
    pub fn run(&self) -> bool {
        let intg_stats = self.integrity_check();
        let pproc_stats = self.preprocess(intg_stats.first_qed);
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
        let intg = IntegrityVerifier::new();
        intg.check(cnf_parser, asr_parser)
    }
    fn preprocess(&self, qed: InstructionNumber) -> PreprocessingStats {
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
        splitter.extract()
    }
    fn trim(&self, data: SplittingData, mut temp_trim: TrimmingFiles) -> PreprocessingStats {
        let mut trimmer = Trimmer::new(data).unwrap();
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
            let core = trimmer.core();
            core.dump(&mut output);
            output.flush().unwrap_or_else(|err| panic!(format!("{}", err)));
        }
        temp_out.flush();
        trimmer.extract()
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

fn main() {
    panic::set_hook(Box::new(|info| PanicMessage.lock().unwrap().push_str(&format!("{}", info))));
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
    }
}

fn build_cli_parser<'a, 'b>() -> App<'a, 'b> {
    App::new(env!("CARGO_PKG_NAME"))
        .setting(AppSettings::ColoredHelp)
        .setting(AppSettings::SubcommandRequired)
        .set_term_width(0usize)
        .version(env!("CARGO_PKG_VERSION"))
        .author(env!("CARGO_PKG_AUTHORS"))
        .about(env!("CARGO_PKG_DESCRIPTION"))
        .subcommand(SubCommand::with_name("preprocess")
            .about("preprocesses an ASR proof for more efficient checking")
            .setting(AppSettings::ColoredHelp)
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
                    "The value for the argument '--batch_size' must be an integer within [{}..{}]",
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
        Ok(AppConfig::Preprocess(PreprocessingConfig {
            cnf_path: formula,
            asr_path: proof,
            temp_path: temp,
            output_path: output,
            chunk_size: batchsize,
        }))
    } else {
        Err(ClapError::with_description(&format!("No subcommand.",), ClapErrorKind::InvalidValue))
    }
}
