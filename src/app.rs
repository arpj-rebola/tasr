use std::{
    path::{PathBuf, Path},
    io::{Error as IoError, Write},
    fs::{OpenOptions},
    time::{Duration},
};

use crate::{
    integrity::{IntegrityVerifier, IntegrityStats, IntegrityConfig, IntegrityData},
    split::{SplitterConfig, Splitter, PreprocessingStats},
    trim::{Trimmer},
    correctness::{CorrectnessChecker, CorrectnessStats, CorrectnessConfig},
    tempfile::{TempFiles},
    textparser::{TextAsrParser},
    io::{OutputWriter, InputReader},
};

pub struct PreprocessingConfig {
    pub cnf: PathBuf,
    pub cnf_binary: bool,
    pub asr: PathBuf,
    pub asr_binary: bool,
    pub temp: PathBuf,
    pub output: Option<PathBuf>,
    pub chunk: u64,
    pub stats: bool,
    pub integrity: Option<IntegrityStats>,
    pub preprocessing: Option<PreprocessingStats>,
    pub data: Option<IntegrityData>,
}
impl PreprocessingConfig {
    pub fn integrity(&mut self) {
        let cnf_parser = PreprocessingConfig::open_input(&self.cnf, self.cnf_binary, "premise CNF formula");
        let asr_parser = PreprocessingConfig::open_input(&self.asr, self.asr_binary, "raw ASR proof");
        let mut verifier = IntegrityVerifier::new(self.integrity_config());
        verifier.check(cnf_parser, asr_parser);
        let (intg, opt_data) = verifier.data();
        self.integrity = Some(intg);
        self.data = opt_data;
    }
    pub fn preprocess(&mut self) {
        if self.data.is_some() {
            let mut temp_files = TempFiles::new(&self.temp);
            let data = {
                let config = self.splitter_config();
                let asr_parser = PreprocessingConfig::open_input(&self.asr, self.asr_binary, "raw ASR proof");
                let mut split_files = temp_files.split();
                let mut buffer = Vec::<u8>::with_capacity((1usize << 16) - 1usize);
                let mut splitter = Splitter::new(config, asr_parser, &mut buffer);
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
            self.preprocessing = Some(stats);
        }
    }
    pub fn print_integrity_stats(&self) {
        if self.stats {
            if let Some(intg) = &self.integrity {
                info!("Integrity check stats", lock, {
                    append!(lock, "{:.<30} {}", "errors", intg.errors.len());
                    breakline!(lock);
                    append!(lock, "{:.<30} {}ms", "CNF checking runtime",
                        intg.cnf_time.unwrap_or(Duration::from_secs(0u64)).as_millis());
                    breakline!(lock);
                    append!(lock, "{:.<30} {}ms", "ASR checking runtime",
                        intg.asr_time.unwrap_or(Duration::from_secs(0u64)).as_millis());
                    breakline!(lock);
                    append!(lock, "{:.<30} {}", "maximum variable", intg.max_var.get());
                    breakline!(lock);
                    append!(lock, "{:.<30} {}", "maximum clause id", intg.max_id.get());
                    breakline!(lock);
                    append!(lock, "{:.<30} {}", "premises", intg.num_premises);
                    breakline!(lock);
                    append!(lock, "{:.<30} {}", "core clauses", intg.num_cores);
                    breakline!(lock);
                    append!(lock, "{:.<30} {}", "proof instructions", intg.num_instructions);
                    breakline!(lock);
                    append!(lock, "{:.<30} {}", "RUP inferences", intg.num_rup);
                    breakline!(lock);
                    append!(lock, "{:.<30} {}", "WSR inferences", intg.num_wsr);
                    breakline!(lock);
                    append!(lock, "{:.<30} {}", "clause deletions", intg.num_del);
                    breakline!(lock);
                    match intg.first_qed {
                        None => { append!(lock, "{:.<30} {}", "first QED instruction", "not found"); },
                        Some((num, id)) => { append!(lock, "{:.<30} id {} at {}", "first QED instruction", id, num); },
                    }
                });
            }
        }
    }
    pub fn print_preprocessing_stats(&self) {
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
    pub fn print_final_result(&self) {
        if self.integrity.as_ref().map(|intg| intg.errors.is_empty()).unwrap_or(false) {
            success!("Proof preprocessing succeeded", lock, {
                append!(lock, "Raw ASR refutation {} of CNF formula {} was successfully preprocessed into ASR refutation {} in {}ms.",
                    &self.asr.display(), &self.cnf.display(), &self.asr.display(), self.total_time().as_millis());
            });
        } else {
            error!("Proof preprocessing failed", lock, {
                append!(lock, "Failed to preprocess raw ASR refutation {} of CNF formula {} due to {} errors within {}ms.",
                    &self.asr.display(), &self.cnf.display(), self.total_errors(), self.total_time().as_millis());
            });
        }
    }
    fn total_time(&self) -> Duration {
        let d1 = if let Some(intg) = &self.integrity {
            intg.cnf_time.unwrap_or(Duration::from_secs(0u64)) + intg.asr_time.unwrap_or(Duration::from_secs(0u64))
        } else {
            Duration::from_secs(0u64)
        };
        let d2 = if let Some(pp) = &self.preprocessing {
            pp.split_time.unwrap_or(Duration::from_secs(0u64)) + pp.split_time.unwrap_or(Duration::from_secs(0u64))
        } else {
            Duration::from_secs(0u64)
        };
        d1 + d2
    }
    pub fn total_errors(&self) -> usize {
        self.integrity.as_ref().map(|stats| stats.errors.len()).unwrap_or(0usize)
    }
    fn integrity_config(&self) -> IntegrityConfig {
        IntegrityConfig {
            preprocessing: true,
            select: 0u64,
            parts: 1u64,
        }
    }
    fn splitter_config(&mut self) -> SplitterConfig {
        SplitterConfig {
            chunk_size: self.chunk,
            end: self.data.take().unwrap().end,
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
    pub integrity: Option<IntegrityStats>,
    pub data: Option<IntegrityData>,
    pub correctness: Option<CorrectnessStats>,
}
impl CheckingConfig {
    pub fn integrity(&mut self) {
        let cnf_parser = PreprocessingConfig::open_input(&self.cnf, self.cnf_binary, "premise CNF formula");
        let asr_parser = PreprocessingConfig::open_input(&self.asr, self.asr_binary, "preprocessed ASR proof");
        let mut verifier = IntegrityVerifier::new(self.integrity_config());
        verifier.check(cnf_parser, asr_parser);
        let (stats, opt_data) = verifier.data();
        self.integrity = Some(stats);
        self.data = opt_data;
    }
    pub fn correctness(&mut self) {
        let config = self.checker_config();
        let cnf_parser = PreprocessingConfig::open_input(&self.cnf, self.cnf_binary, "premise CNF formula");
        let asr_parser = PreprocessingConfig::open_input(&self.asr, self.asr_binary, "preprocessed ASR proof");
        let checker = CorrectnessChecker::new(config);
        let stats = checker.check(cnf_parser, asr_parser);
        self.correctness = Some(stats);
    }
    pub fn print_integrity_stats(&self) {
        if self.stats {
            if let Some(intg) = &self.integrity {
                info!("Integrity check stats", lock, {
                    append!(lock, "{:.<30} {}", "errors", intg.errors.len());
                    breakline!(lock);
                    append!(lock, "{:.<30} {}ms", "CNF checking runtime",
                        intg.cnf_time.unwrap_or(Duration::from_secs(0u64)).as_millis());
                    breakline!(lock);
                    append!(lock, "{:.<30} {}ms", "ASR checking runtime",
                        intg.asr_time.unwrap_or(Duration::from_secs(0u64)).as_millis());
                    breakline!(lock);
                    append!(lock, "{:.<30} {}", "maximum variable", intg.max_var.get());
                    breakline!(lock);
                    append!(lock, "{:.<30} {}", "maximum clause id", intg.max_id.get());
                    breakline!(lock);
                    append!(lock, "{:.<30} {}", "premises", intg.num_premises);
                    breakline!(lock);
                    append!(lock, "{:.<30} {}", "core clauses", intg.num_cores);
                    breakline!(lock);
                    append!(lock, "{:.<30} {}", "proof instructions", intg.num_instructions);
                    breakline!(lock);
                    append!(lock, "{:.<30} {}", "RUP inferences", intg.num_rup);
                    breakline!(lock);
                    append!(lock, "{:.<30} {}", "WSR inferences", intg.num_wsr);
                    breakline!(lock);
                    append!(lock, "{:.<30} {}", "clause deletions", intg.num_del);
                    breakline!(lock);
                    match intg.first_qed {
                        None => { append!(lock, "{:.<30} {}", "first QED instruction", "not found"); },
                        Some((num, id)) => { append!(lock, "{:.<30} id {} at {}", "first QED instruction", id, num); },
                    }
                });
            }
        }
    }
    pub fn print_correctness_stats(&self) {
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
    pub fn print_final_result(&self) {
        if self.correctness.as_ref().map(|correctness| correctness.errors.is_empty()).unwrap_or(false) {
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
    fn total_time(&self) -> Duration {
        let d1 = if let Some(intg) = &self.integrity {
            intg.cnf_time.unwrap_or(Duration::from_secs(0u64)) + intg.asr_time.unwrap_or(Duration::from_secs(0u64))
        } else {
            Duration::from_secs(0u64)
        };
        let d2 = if let Some(pp) = &self.correctness {
            pp.rewind_time.unwrap_or(Duration::from_secs(0u64)) + pp.check_time.unwrap_or(Duration::from_secs(0u64))
        } else {
            Duration::from_secs(0u64)
        };
        d1 + d2
    }
    pub fn total_errors(&self) -> usize {
        self.integrity.as_ref().map(|stats| stats.errors.len()).unwrap_or(0usize) + self.correctness.as_ref().map(|stats| stats.errors.len()).unwrap_or(0usize)
    }
    fn total_warnings(&self) -> usize {
        self.correctness.as_ref().map(|stats| stats.warnings.len()).unwrap_or(0usize)
    }
    fn integrity_config(&self) -> IntegrityConfig {
        IntegrityConfig {
            preprocessing: false,
            select: self.select,
            parts: self.parts,
        }
    }
    fn checker_config(&mut self) -> CorrectnessConfig {
        CorrectnessConfig {
            select: self.select,
            parts: self.parts,
            insertions: self.data.take().unwrap().insertions,
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