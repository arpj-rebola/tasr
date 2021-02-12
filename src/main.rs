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
mod model;
mod idflags;
mod idmap;
mod formula;
mod display;
mod checkerdb;
mod clauseset;
mod buffer;
mod chain;
mod proof;
mod tempfile;
mod propagation;
mod integrity;
mod split;
mod trim;
mod correctness;

use std::{
    fs::{OpenOptions},
    process::{self},
    sync::{Mutex},
    panic::{self},
    path::{Path, PathBuf},
    io::{Write, Error as IoError},
    time::{Duration},
};

use clap::{
    Arg, App, AppSettings, Error as ClapError, ErrorKind as ClapErrorKind, Result as ClapResult, SubCommand,
};

use crate::{
    integrity::{IntegrityConfig, IntegrityData, IntegrityVerifier, IntegrityStats},
    io::{InputReader, OutputWriter, PrintedPanic},
    textparser::{TextAsrParser},
    split::{SplitterConfig, PreprocessingStats, Splitter},
    correctness::{CorrectnessConfig, CorrectnessChecker, CorrectnessStats},
    tempfile::{TempFiles},
    trim::{Trimmer},
};

lazy_static! {
    static ref PanicMessage: Mutex<String> = {
        Mutex::new(String::new())
    };
}

pub struct PreprocessingResult {
    pub cnf: PathBuf,
    pub asr: PathBuf,
    pub output: PathBuf,
    pub success: bool,
    pub integrity: IntegrityStats,
    pub preprocessing: Option<PreprocessingStats>,
    pub stats: bool,
}
impl PreprocessingResult {
    fn print_integrity_stats(&self) {
        if self.stats {
            info!("Integrity check stats", lock, {
                append!(lock, "{:.<30} {}", "errors", self.integrity.errors.len());
                breakline!(lock);
                append!(lock, "{:.<30} {}ms", "CNF checking runtime",
                    self.integrity.cnf_time.unwrap_or(Duration::from_secs(0u64)).as_millis());
                breakline!(lock);
                append!(lock, "{:.<30} {}ms", "ASR checking runtime",
                    self.integrity.asr_time.unwrap_or(Duration::from_secs(0u64)).as_millis());
                breakline!(lock);
                append!(lock, "{:.<30} {}", "maximum variable", self.integrity.max_var.get());
                breakline!(lock);
                append!(lock, "{:.<30} {}", "maximum clause id", self.integrity.max_id.get());
                breakline!(lock);
                append!(lock, "{:.<30} {}", "premises", self.integrity.num_premises);
                breakline!(lock);
                append!(lock, "{:.<30} {}", "core clauses", self.integrity.num_cores);
                breakline!(lock);
                append!(lock, "{:.<30} {}", "proof instructions", self.integrity.num_instructions);
                breakline!(lock);
                append!(lock, "{:.<30} {}", "RUP inferences", self.integrity.num_rup);
                breakline!(lock);
                append!(lock, "{:.<30} {}", "WSR inferences", self.integrity.num_wsr);
                breakline!(lock);
                append!(lock, "{:.<30} {}", "clause deletions", self.integrity.num_del);
                breakline!(lock);
                match self.integrity.first_qed {
                    None => { append!(lock, "{:.<30} {}", "first QED instruction", "not found"); },
                    Some((num, id)) => { append!(lock, "{:.<30} id {} at {}", "first QED instruction", id, num); },
                }
            });
        }
    }
    fn print_preprocessing_stats(&self) {
        if self.stats {
            if let Some(pp) = &self.preprocessing {
                info!("Proof preprocessing stats", lock, {
                    append!(lock, "{:.<30} {}ms", "splitting runtime",
                        pp.split_time.unwrap_or(Duration::from_secs(0u64)).as_millis());
                    breakline!(lock);
                    append!(lock, "{:.<30} {}ms", "trimming runtime",
                        pp.trim_time.unwrap_or(Duration::from_secs(0u64)).as_millis());
                    breakline!(lock);
                    append!(lock, "{:.<30} {}", "proof batches", pp.num_chunks);
                    breakline!(lock);
                    append!(lock, "{:.<30} {}", "core clauses", pp.num_cores);
                    breakline!(lock);
                    append!(lock, "{:.<30} {}", "proof instructions", pp.num_instructions);
                    breakline!(lock);
                    append!(lock, "{:.<30} {}", "clause introductions", pp.num_intros);
                    breakline!(lock);
                    append!(lock, "{:.<30} {}", "clause deletions", pp.num_dels);
                });
            }
        }
    }
    fn total_time(&self) -> Duration {
        let d1 = self.integrity.cnf_time.unwrap_or(Duration::from_secs(0u64)) +
            self.integrity.asr_time.unwrap_or(Duration::from_secs(0u64));
        let d2 = if let Some(pp) = &self.preprocessing {
            pp.split_time.unwrap_or(Duration::from_secs(0u64)) +
                pp.split_time.unwrap_or(Duration::from_secs(0u64))
        } else {
            Duration::from_secs(0u64)
        };
        d1 + d2
    }
    fn total_errors(&self) -> usize {
        self.integrity.errors.len()
    }
    fn print_final_result(&self) {
        if self.success {
            success!("Proof preprocessing succeeded", lock, {
                append!(lock, "Raw ASR refutation {} of CNF formula {} was successfully preprocessed into ASR refutation {} in {}ms.",
                    &self.asr.display(), &self.cnf.display(), &self.output.display(), self.total_time().as_millis());
            });
        } else {
            error!("Proof preprocessing failed", lock, {
                append!(lock, "Failed to preprocess raw ASR refutation {} of CNF formula {} due to {} errors within {}ms.",
                    &self.asr.display(), &self.cnf.display(), self.total_errors(), self.total_time().as_millis());
            });
        }
    }
}

pub struct PreprocessingConfig {
    pub cnf: PathBuf,
    pub cnf_binary: bool,
    pub asr: PathBuf,
    pub asr_binary: bool,
    pub temp: PathBuf,
    pub output: Option<PathBuf>,
    pub chunk: u64,
    pub stats: bool,
}
impl PreprocessingConfig {
    pub fn run(self) -> PreprocessingResult {
        progress!("checking file integrity...", lock, {
            append!(lock, "Checking the CNF file {} and raw ASR file {} for format errors and undefined references.", self.cnf.display(), self.asr.display());
        });
        let (integrity, opt_data) = self.check_integrity();
        progress!("preprocessing ASR proof...", lock, {
            append!(lock, "Preprocessing the raw ASR file {} into the preprocessed ASR file {} to reuse indices, trim unnecessary inferences, and reduce complex proof steps.",
                self.asr.display(), self.output.as_ref().map(|x| x.display()).unwrap_or_else(|| self.asr.display()));
        });
        let (preprocessing, result) = if let Some(data) = opt_data {
            (Some(self.preprocess(data)), true)
        } else {
            (None, false)
        };
        let out = match self.output {
            Some(x) => x,
            None => self.asr.clone(),
        };
        PreprocessingResult {
            cnf: self.cnf,
            asr: self.asr,
            output: out,
            success: result,
            integrity: integrity,
            preprocessing: preprocessing,
            stats: self.stats,
        }
    }
    fn check_integrity(&self) -> (IntegrityStats, Option<IntegrityData>) {
        let cnf_parser = PreprocessingConfig::open_input(&self.cnf, self.cnf_binary, "premise CNF formula");
        let asr_parser = PreprocessingConfig::open_input(&self.asr, self.asr_binary, "raw ASR proof");
        let mut verifier = IntegrityVerifier::new(self.integrity());
        verifier.check(cnf_parser, asr_parser);
        verifier.data()
    }
    fn preprocess(&self, data: IntegrityData) -> PreprocessingStats {
        let mut temp_files = TempFiles::new(&self.temp);
        let data = {
            let asr_parser = PreprocessingConfig::open_input(&self.asr, self.asr_binary, "raw ASR proof");
            let mut split_files = temp_files.split();
            let mut buffer = Vec::<u8>::with_capacity((1usize << 16) - 1usize);
            let mut splitter = Splitter::new(self.splitter(data), asr_parser, &mut buffer);
            while let Some(fragment) = splitter.next() {
                let (split_path, split_file) = split_files.get();
                let mut split_out = OutputWriter::with_capacity(split_file, split_path, (1usize << 16) - 1usize);
                fragment.dump(&mut split_out);
                split_out.flush().unwrap();
            }
            splitter.extract()
        };
        let stats = {
            let mut trim_files = temp_files.trimmed();
            let mut trimmer = Trimmer::new(data);
            while let Some((split_path, trim_path, trim_file)) = trim_files.get() {
                let parser = PreprocessingConfig::open_input(split_path, false, "transitional split ASR proof");
                let fragment = trimmer.process(parser);
                let mut trim_out = OutputWriter::with_capacity(trim_file, trim_path, (1usize << 16) - 1usize);
                fragment.dump(&mut trim_out);
                trim_out.flush().unwrap();
            }
            let (core_path, core_file) = trim_files.core();
            let mut core_out = OutputWriter::with_capacity(core_file, core_path, (1usize << 16) - 1usize);
            let stats = trimmer.core(&mut core_out);
            core_out.flush().unwrap();
            stats
        };
        let output_path = match &self.output {
            Some(path) => path,
            None => &self.asr,
        };
        let output_file = OpenOptions::new().create(true).write(true).truncate(true).open(&output_path)
            .unwrap_or_else(|err| PreprocessingConfig::opening_output_error(&output_path, err, "preprocessed ASR proof"));
        let mut output = OutputWriter::with_capacity(output_file, output_path, (1usize << 16) - 1usize);
        temp_files.conflate(&self.asr, stats.instruction_count(), &mut output);
        stats
    }
    fn integrity(&self) -> IntegrityConfig {
        IntegrityConfig {
            preprocessing: true,
            select: 0u64,
            parts: 1u64,
        }
    }
    fn splitter(&self, data: IntegrityData) -> SplitterConfig {
        SplitterConfig {
            chunk_size: self.chunk,
            end: data.end,
        }
    }
    fn open_input<'a>(path: &'a Path, binary: bool, kind: &str) -> TextAsrParser<'a> {
        let file = OpenOptions::new().read(true).open(&path).unwrap_or_else(|err| PreprocessingConfig::opening_input_error(&path, err, kind));
        let input = InputReader::new(file, path, binary);
        TextAsrParser::<'_>::new(input)
    }
    fn opening_input_error(path: &Path, err: IoError, kind: &str) -> ! {
        panick!("unable to open input file", lock, {
            append!(lock, "Could not open input {} file {}:", kind, path.to_str().unwrap());
            breakline!(lock);
            append!(lock, "{}", err);
        })
    }
    fn opening_output_error(path: &Path, err: IoError, kind: &str) -> ! {
        panick!("unable to open output file", lock, {
            append!(lock, "Could not open output {} file {}:", kind, path.to_str().unwrap());
            breakline!(lock);
            append!(lock, "{}", err);
        })
    }
}

pub struct CheckingConfig {
    pub cnf: PathBuf,
    pub cnf_binary: bool,
    pub asr: PathBuf,
    pub asr_binary: bool,
    pub permissive: bool,
    pub select: u64,
    pub parts: u64,
    pub stats: bool,
}
impl CheckingConfig {
    pub fn run(self) -> CheckingResult {
        progress!("Checking file integrity...", lock, {
            append!(lock, "Checking the CNF file {} and preprocessed ASR file {} for format errors and undefined references.", self.cnf.display(), self.asr.display());
        });
        let (integrity, opt_data) = self.check_integrity();
        progress!("Checking proof correctness...", lock, {
            append!(lock, "Checking that inferences in the preprocessed ASR file {} are correct with respect to the CNF file {}.", self.asr.display(), self.cnf.display());
        });
        let opt_correctness = if let Some(data) = opt_data {
            Some(self.check_correctness(data))
        } else {
            None
        };
        let result = opt_correctness.as_ref().map(|correctness| correctness.errors.is_empty()).unwrap_or(false);
        CheckingResult {
            cnf: self.cnf,
            asr: self.asr,
            success: result,
            integrity: integrity,
            correctness: opt_correctness,
            stats: self.stats,
        }
    }
    fn check_integrity(&self) -> (IntegrityStats, Option<IntegrityData>) {
        let cnf_parser = PreprocessingConfig::open_input(&self.cnf, self.cnf_binary, "premise CNF formula");
        let asr_parser = PreprocessingConfig::open_input(&self.asr, self.asr_binary, "preprocessed ASR proof");
        let mut verifier = IntegrityVerifier::new(self.integrity());
        verifier.check(cnf_parser, asr_parser);
        verifier.data()
    }
    fn check_correctness(&self, data: IntegrityData) -> CorrectnessStats {
        let cnf_parser = PreprocessingConfig::open_input(&self.cnf, self.cnf_binary, "premise CNF formula");
        let asr_parser = PreprocessingConfig::open_input(&self.asr, self.asr_binary, "preprocessed ASR proof");
        let checker = CorrectnessChecker::new(self.checker(data));
        checker.check(cnf_parser, asr_parser)
    }
    fn integrity(&self) -> IntegrityConfig {
        IntegrityConfig {
            preprocessing: false,
            select: self.select,
            parts: self.parts,
        }
    }
    fn checker(&self, data: IntegrityData) -> CorrectnessConfig {
        CorrectnessConfig {
            select: self.select,
            parts: self.parts,
            insertions: data.insertions,
            permissive: self.permissive,
        }
    }
    fn opening_input_error(path: &Path, err: IoError, kind: &str) -> ! {
        panick!("unable to open input file", lock, {
            append!(lock, "Could not open input {} file {}:", kind, path.to_str().unwrap());
            breakline!(lock);
            append!(lock, "{}", err);
        })
    }
}

pub struct CheckingResult {
    pub cnf: PathBuf,
    pub asr: PathBuf,
    pub success: bool,
    pub integrity: IntegrityStats,
    pub correctness: Option<CorrectnessStats>,
    pub stats: bool,
}
impl CheckingResult {
    fn print_integrity_stats(&self) {
        if self.stats {
            info!("Integrity check stats", lock, {
                append!(lock, "{:.<30} {}", "errors", self.integrity.errors.len());
                breakline!(lock);
                append!(lock, "{:.<30} {}ms", "CNF checking runtime",
                    self.integrity.cnf_time.unwrap_or(Duration::from_secs(0u64)).as_millis());
                breakline!(lock);
                append!(lock, "{:.<30} {}ms", "ASR checking runtime",
                    self.integrity.asr_time.unwrap_or(Duration::from_secs(0u64)).as_millis());
                breakline!(lock);
                append!(lock, "{:.<30} {}", "maximum variable", self.integrity.max_var.get());
                breakline!(lock);
                append!(lock, "{:.<30} {}", "maximum clause id", self.integrity.max_id.get());
                breakline!(lock);
                append!(lock, "{:.<30} {}", "premises", self.integrity.num_premises);
                breakline!(lock);
                append!(lock, "{:.<30} {}", "core clauses", self.integrity.num_cores);
                breakline!(lock);
                append!(lock, "{:.<30} {}", "proof instructions", self.integrity.num_instructions);
                breakline!(lock);
                append!(lock, "{:.<30} {}", "RUP inferences", self.integrity.num_rup);
                breakline!(lock);
                append!(lock, "{:.<30} {}", "WSR inferences", self.integrity.num_wsr);
                breakline!(lock);
                append!(lock, "{:.<30} {}", "clause deletions", self.integrity.num_del);
                breakline!(lock);
                match self.integrity.first_qed {
                    None => { append!(lock, "{:.<30} {}", "first QED instruction", "not found"); },
                    Some((num, id)) => { append!(lock, "{:.<30} id {} at {}", "first QED instruction", id, num); },
                }
            });
        }
    }
    fn print_correctness_stats(&self) {
        if self.stats {
            if let Some(pp) = &self.correctness {
                info!("Correctness check stats", lock, {
                    append!(lock, "{:.<30} {}", "errors", pp.errors.len());
                    breakline!(lock);
                    append!(lock, "{:.<30} {}", "warnings", pp.warnings.len());
                    breakline!(lock);
                    append!(lock, "{:.<30} {}ms", "rewinding runtime",
                        pp.rewind_time.unwrap_or(Duration::from_secs(0u64)).as_millis());
                    breakline!(lock);
                    append!(lock, "{:.<30} {}ms", "checking runtime",
                        pp.check_time.unwrap_or(Duration::from_secs(0u64)).as_millis());
                    breakline!(lock);
                    append!(lock, "{:.<30} {} / {}", "part", pp.part.0 + 1u64, pp.part.1);
                    breakline!(lock);
                    append!(lock, "{:.<30} {}", "checked instructions", pp.processed);
                });
            }
        }
    }
    fn total_time(&self) -> Duration {
        let d1 = self.integrity.cnf_time.unwrap_or(Duration::from_secs(0u64)) +
            self.integrity.asr_time.unwrap_or(Duration::from_secs(0u64));
        let d2 = if let Some(pp) = &self.correctness {
            pp.rewind_time.unwrap_or(Duration::from_secs(0u64)) +
                pp.check_time.unwrap_or(Duration::from_secs(0u64))
        } else {
            Duration::from_secs(0u64)
        };
        d1 + d2
    }
    fn total_errors(&self) -> usize {
        self.integrity.errors.len() + self.correctness.as_ref().map(|stats| stats.errors.len()).unwrap_or(0usize)
    }
    fn total_warnings(&self) -> usize {
        self.correctness.as_ref().map(|stats| stats.warnings.len()).unwrap_or(0usize)
    }
    fn print_final_result(&self) {
        if self.success {
            success!("Proof checking succeeded", lock, {
                let (part, total) = self.correctness.as_ref().unwrap().part;
                if total > 1u64 {
                    append!(lock, "Part {} / {} in preprocessed", part + 1u64, total); 
                } else {
                    append!(lock, "Preprocessed");
                }
                append!(lock, " ASR refutation {} of CNF formula {} was successfully checked correct in {}ms",
                    &self.asr.display(), &self.cnf.display(), self.total_time().as_millis());
                let warnings = self.total_warnings();
                if warnings > 0usize {
                    append!(lock, " with {} warnings", warnings);
                }
                append!(lock, ".");
            });
        } else {
            error!("Proof checking failed", lock, {
                append!(lock, "Preprocessed ASR refutation {} of CNF formula {} was found incorrect due to {} errors",
                    &self.asr.display(), &self.cnf.display(), self.total_errors());
                let warnings = self.total_warnings();
                if warnings > 0usize {
                    append!(lock, " and {} warnings", warnings);
                }
                append!(lock, " within {}ms.", self.total_time().as_millis());
            });
        }
    }
}

pub enum AppConfig {
    Preprocess(PreprocessingConfig),
    Check(CheckingConfig),
}
impl AppConfig {
    pub fn run(self) -> bool {
        match self {
            AppConfig::Preprocess(config) => {
                let result = config.run();
                result.print_integrity_stats();
                result.print_preprocessing_stats();
                result.print_final_result();
                result.success
            },
            AppConfig::Check(config) => {
                let result = config.run();
                result.print_integrity_stats();
                result.print_correctness_stats();
                result.print_final_result();
                result.success
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