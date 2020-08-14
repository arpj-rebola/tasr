use std::{
	fmt::{self, Formatter, Display},
	time::{Instant},
};

use colored::{
	Colorize,
};

use crate::{
	assignment::{InsertionTest, Block, Substitution, LiteralSet},
	basic::{CnfHeaderStats, WitnessContainer, ClauseIndex, ClauseContainer},
	chaindb::{ChainDb},
	clausedb::{ClauseDb, ClauseSet},
	input::{Positioned, StreamPosition},
	parser::{AsrParser, AsrInstructionKind},
	results::{VerificationFailure, ErrorStorage, ClauseIssuesBuilder, ChainIssuesBuilder, PropagationIssues, WitnessIssuesBuilder},
	unitpropagation::{UnitPropagator},
	variable::{MaybeVariable},
	logger::{Logger, LoggerId},
};

#[derive(Debug, Copy, Clone)]
pub enum Trimming {
    Nothing,
    Cleanup,
    Trimming,
}

pub struct CheckerStats {
	pub max_variable: MaybeVariable,
	pub max_index: Option<ClauseIndex>,
	pub num_premises: usize,
	pub num_cores: usize,
	pub num_rups: usize,
	pub num_wsrs: usize,
	pub num_deletions: usize,
	pub num_dels: usize,
	pub num_contradictions: usize,
	pub errors: ErrorStorage,
	pub time_start: Instant,
	pub time_core: Instant,
	pub time_derivation: Instant,
	pub time_trimming: Instant,
}
impl CheckerStats {
	fn new() -> CheckerStats {
		let now = Instant::now();
		CheckerStats {
			max_variable: MaybeVariable::None,
			max_index: None,
			num_premises: 0usize,
			num_cores: 0usize,
			num_rups: 0usize,
			num_wsrs: 0usize,
			num_deletions: 0usize,
			num_dels: 0usize,
			num_contradictions: 0usize,
			errors: ErrorStorage::new(),
			time_start: now,
			time_core: now,
			time_derivation: now,
			time_trimming: now,
		}
	}
	fn num_proof_instructions(&self) -> usize {
		self.num_rups + self.num_wsrs + self.num_dels
	}
	fn set_time_core(&mut self) {
		let now = Instant::now();
		self.time_core = now;
		self.time_derivation = now;
		self.time_trimming = now;
	}
	fn set_time_derivation(&mut self) {
		let now = Instant::now();
		self.time_derivation = now;
		self.time_trimming = now;
	}
	fn set_time_trimming(&mut self) {
		let now = Instant::now();
		self.time_trimming = now;
	}
}
impl Display for CheckerStats {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		let (warnings, errors) = self.errors.count();
		write!(f, "    {:.<50} {}\n",  format!("{}", "errors".blue().bold()), errors)?;
		write!(f, "    {:.<50} {}\n",  format!("{}", "warnings".blue().bold()), warnings)?;
		write!(f, "    {:.<50} {}\n",  format!("{}", "maximum variable".blue().bold()), self.max_variable)?;
		write!(f, "    {:.<50} {}\n",  format!("{}", "maximum clause identifier".blue().bold()), self.max_index.map(|x| x.index()).unwrap_or(0usize))?;
		write!(f, "    {:.<50} {}\n",  format!("{}", "premise clauses".blue().bold()), self.num_premises)?;
		write!(f, "    {:.<50} {}\n",  format!("{}", "core clauses".blue().bold()), self.num_cores)?;
		write!(f, "    {:.<50} {}\n",  format!("{}", "RUP inferences".blue().bold()), self.num_rups)?;
		write!(f, "    {:.<50} {}\n",  format!("{}", "WSR inferences".blue().bold()), self.num_wsrs)?;
		write!(f, "    {:.<50} {}\n",  format!("{}", "deletion instructions".blue().bold()), self.num_dels)?;
		write!(f, "    {:.<50} {}\n",  format!("{}", "deleted clauses".blue().bold()), self.num_deletions)?;
		write!(f, "    {:.<50} {}\n",  format!("{}", "total instructions".blue().bold()), self.num_proof_instructions())?;
		write!(f, "    {:.<50} {}\n",  format!("{}", "final contradictions".blue().bold()), self.num_contradictions)?;
		write!(f, "    {:.<50} {}s\n", format!("{}", "core checking runtime".blue().bold()), self.time_core.duration_since(self.time_start).as_secs())?;
		write!(f, "    {:.<50} {}s\n", format!("{}", "proof checking runtime".blue().bold()), self.time_derivation.duration_since(self.time_core).as_secs())?;
		write!(f, "    {:.<50} {}s\n", format!("{}", "trimming runtime".blue().bold()), self.time_trimming.duration_since(self.time_derivation).as_secs())?;
		write!(f, "    {:.<50} {}s\n", format!("{}", "total runtime".blue().bold()), self.time_trimming.duration_since(self.time_start).as_secs())?;
		Ok(())
    }
}

#[derive(Debug)]
pub struct CheckerConfig {
	pub check_core: bool,
	pub check_derivation: bool,
	pub check_refutation: bool,
	pub core_limit: usize,
	pub proof_limit: usize,
	pub trimming: Trimming,
	pub output: LoggerId,
}

pub struct AsrChecker<'a, 'b: 'a> {
	cnf: &'a mut dyn AsrParser,
	asr: &'a mut dyn AsrParser,
	config: CheckerConfig,
	logger: &'a mut Logger<'b>,
	db: ClauseDb,
	stats: CheckerStats,
}
impl<'a, 'b: 'a> AsrChecker<'a, 'b> {
	const CnfHeader: [u8; 8] = [b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8];
	const CoreHeader: [u8; 8] = [b'c', b'o', b'r', b'e', 0u8, 0u8, 0u8, 0u8];
	const ProofHeader: [u8; 8] = [b'p', b'r', b'o', b'o', b'f', 0u8, 0u8, 0u8];
	pub fn new<'d: 'a, 'e: 'a, 'f: 'a>(
		cnf: &'d mut dyn AsrParser,
		asr: &'e mut dyn AsrParser,
		logger: &'f mut Logger<'b>,
		config: CheckerConfig,
	) -> AsrChecker<'a, 'b> {
		AsrChecker::<'a, 'b> {
			cnf: cnf,
			asr: asr,
			config: config,
			logger: logger,
			db: ClauseDb::new(),
			stats: CheckerStats::new(),
		}
	}
	pub fn check(mut self) -> CheckerStats {
		{
			let mut set = ClauseSet::new();
			{
				let premise_checker = AsrPremiseChecker::<'_, '_> {
					cnf: &mut *self.cnf,
					config: &mut self.config,
					logger: &mut *self.logger,
					set: &mut set,
					lits: LiteralSet::new(),
					stats: &mut self.stats,
				};
				premise_checker.check();
			}
			{
				let core_checker = AsrCoreChecker::<'_, '_> {
					asr: &mut *self.asr,
					config: &mut self.config,
					logger: &mut *self.logger,
					db: &mut self.db,
					set: &mut set,
					block: LiteralSet::new(),
					stats: &mut self.stats,
				};
				core_checker.check();
			}
		}
		self.stats.set_time_core();
		{
			let proof_checker = AsrProofChecker::<'_, '_> {
				asr: &mut *self.asr,
				config: &mut self.config,
				logger: &mut *self.logger,
				db: &mut self.db,
				stats: &mut self.stats,
				block: Block::new(),
				lits: LiteralSet::new(),
				chain: ChainDb::new(),
				subst: Substitution::new(),
			};
			proof_checker.check();
		}
		self.stats.set_time_derivation();
		self.stats
	}
}

pub struct AsrPremiseChecker<'a, 'b: 'a> {
	cnf: &'a mut dyn AsrParser,
	config: &'a CheckerConfig,
	logger: &'a mut Logger<'b>,
	set: &'a mut ClauseSet,
	lits: LiteralSet,
	stats: &'a mut CheckerStats,
}
impl<'a, 'b: 'a> AsrPremiseChecker<'a, 'b> {
	pub fn check(mut self) {
		let mut hdfound = false;
		loop { 
			match &self.cnf.skip_to_header() {
				Positioned(Some(hd), pos) if hd == &AsrChecker::<'_, '_>::CnfHeader => if !hdfound {
					hdfound = true;
					let hdstats = &self.cnf.parse_cnf_header();
					while let Positioned(Some(()), pos) = &self.cnf.parse_formula() {
						self.stats.num_premises += 1usize;
						if let Some(bx) = self.parse_clause() {
							self.error_invalid_clause(pos, bx);
						}
						self.logger.flush(self.config.output);
					}
					if &self.stats.max_variable > &hdstats.variables {
						self.error_incorrect_num_variables(pos, hdstats);
					}
					if &self.stats.num_premises != &hdstats.clauses {
						self.error_incorrect_num_clauses(pos, hdstats);
					}
				} else {
					self.error_duplicated_section(pos);
				},
				Positioned(None, pos) => {
					if !hdfound {
						self.error_missing_section(pos);
					}
					break;
				}
				_ => (),
			}
			self.logger.flush(self.config.output);
		}
		self.logger.flush(self.config.output);
	}
	fn parse_clause(&mut self) -> Option<Box<ClauseContainer>> {
		let mut issues = ClauseIssuesBuilder::new();
		let mut writer = self.set.open(&mut self.lits);
		while let Some(lit) = &self.cnf.parse_clause() {
			self.stats.max_variable |= unsafe { lit.variable_unchecked() };
			let test = writer.write(*lit);
			if !test {
				issues.set(|| writer.extract());
			}
			issues.push_literal(*lit);
		}
		writer.close();
		issues.extract()
	}
	fn process_error(&mut self, err: VerificationFailure) {
		let handle = self.logger.handle(self.config.output);
		self.stats.errors.push_error(handle, err);
	}
	fn error_duplicated_section(&mut self, pos: &StreamPosition) {
		let pos = pos.source(self.cnf.name());
		let err = VerificationFailure::duplicated_cnf_section(pos);
		self.process_error(err);
	}
	fn error_missing_section(&mut self, pos: &StreamPosition) {
		let pos = pos.source(self.cnf.name());
		let err = VerificationFailure::missing_cnf_section(pos);
		self.process_error(err);
	}
	fn error_incorrect_num_variables(&mut self, pos: &StreamPosition, hdstats: &CnfHeaderStats) {
		let vars = self.stats.max_variable;
		let stat = hdstats.variables;
		let pos = pos.source(self.cnf.name());
		let err = VerificationFailure::incorrect_num_variables(pos, vars, stat);
		self.process_error(err);
	}
	fn error_incorrect_num_clauses(&mut self, pos: &StreamPosition, hdstats: &CnfHeaderStats) {
		let vars = self.stats.num_premises;
		let stat = hdstats.clauses;
		let pos = pos.source(self.cnf.name());
		let err = VerificationFailure::incorrect_num_clauses(pos, vars, stat);
		self.process_error(err);
	}
	fn error_invalid_clause(&mut self, pos: &StreamPosition, issues: Box<ClauseContainer>) {
		let num = self.stats.num_premises;
		let pos = pos.source(self.cnf.name());
		let err = VerificationFailure::invalid_premise(pos, num, issues);
		self.process_error(err);
	}
}

pub struct AsrCoreChecker<'a, 'b: 'a> {
	asr: &'a mut dyn AsrParser,
	config: &'a CheckerConfig,
	logger: &'a mut Logger<'b>,
	db: &'a mut ClauseDb,
	set: &'a mut ClauseSet,
	stats: &'a mut CheckerStats,
	block: LiteralSet,
}
impl<'a, 'b: 'a>  AsrCoreChecker<'a, 'b> {
	pub fn check(mut self) {
		loop {
			match &self.asr.skip_to_header() {
				Positioned(Some(hd), _) if hd == &AsrChecker::<'_, '_>::CoreHeader => {
					while let Positioned(Some(id), pos) = &self.asr.parse_core() {
						self.stats.num_cores += 1usize;
						if self.stats.num_cores <= self.config.core_limit {
							let cls = match self.parse_clause(*id) {
								Some(None) => true,
								Some(Some(bx)) => {
									self.error_invalid_clause(pos, bx);
									true
								},
								None => {
									self.error_conflict_id(pos, *id);
									false
								},
							};
							if cls && self.config.check_core {
								let rf = self.db.retrieve(*id).unwrap();
								let del = self.set.remove(&rf, &mut self.block);
								if del.is_none() {
									self.error_incorrect_core(pos, *id);
								}
							}
						} else {
							while let Some(_) = &self.asr.parse_clause() {
							}
						}
						self.logger.flush(self.config.output);
					}
					break;
				},
				Positioned(Some(hd), pos) if hd == &AsrChecker::<'_, '_>::ProofHeader => {
					self.error_misplaced_section(pos);
				}
				Positioned(None, pos) => {
					self.error_missing_section(pos);
					break;
				}
				_ => (),
			}
			self.logger.flush(self.config.output);
		}
		self.logger.flush(self.config.output);
	}
	fn parse_clause(&mut self, id: ClauseIndex) -> Option<Option<Box<ClauseContainer>>> {
		let mut issues = ClauseIssuesBuilder::new();
		match self.db.open(&mut self.block, id) {
			Some(mut writer) => {
				while let Some(lit) = &self.asr.parse_clause() {
					self.stats.max_variable |= unsafe { lit.variable_unchecked() };
					let test = writer.write(*lit);
					if !test {
						issues.set(|| writer.extract());
					}
					issues.push_literal(*lit);
				}
				let report = writer.close();
				if report.is_contradiction() {
					self.stats.num_contradictions += 1usize;
				}
				Some(issues.extract())
			},
			None => {
				while let Some(_) = &self.asr.parse_clause() {
				}
				None
			},
		}
	}
	fn process_error(&mut self, err: VerificationFailure) {
		let handle = self.logger.handle(self.config.output);
		self.stats.errors.push_error(handle, err);
	}
	fn error_missing_section(&mut self, pos: &StreamPosition) {
		let pos = pos.source(self.asr.name());
		let err = VerificationFailure::missing_core_section(pos);
		self.process_error(err);
	}	
	fn error_misplaced_section(&mut self, pos: &StreamPosition) {
		let pos = pos.source(self.asr.name());
		let err = VerificationFailure::misplaced_proof_section(pos);
		self.process_error(err);
	}
	fn error_incorrect_core(&mut self, pos: &StreamPosition, id: ClauseIndex) {
		let pos = pos.source(self.asr.name());
		let cls = self.db.extract(id).unwrap();
		let num = self.stats.num_cores;
		let err = VerificationFailure::incorrect_core(pos, num, id, ClauseContainer(cls));
		self.process_error(err);
	}
	fn error_conflict_id(&mut self, pos: &StreamPosition, id: ClauseIndex) {
		let pos = pos.source(self.asr.name());
		let cls = self.db.extract(id).unwrap();
		let num = self.stats.num_cores;
		let err = VerificationFailure::conflict_core_id(pos, num, id, ClauseContainer(cls));
		self.process_error(err);
	}
	fn error_invalid_clause(&mut self, pos: &StreamPosition, issues: Box<ClauseContainer>) {
		let num = self.stats.num_premises;
		let pos = pos.source(self.asr.name());
		let err = VerificationFailure::invalid_core(pos, num, issues);
		self.process_error(err);
	}
}

pub struct AsrProofChecker<'a, 'b: 'a> {
	asr: &'a mut dyn AsrParser,
	config: &'a CheckerConfig,
	logger: &'a mut Logger<'b>,
	db: &'a mut ClauseDb,
	stats: &'a mut CheckerStats,
	lits: LiteralSet,
	block: Block,
	chain: ChainDb,
	subst: Substitution,
}
#[allow(unused_assignments)]
impl<'a, 'b: 'a>  AsrProofChecker<'a, 'b> {
	pub fn check(mut self) {
		let mut hdfound = false;
		loop {
			match &self.asr.skip_to_header() {
				Positioned(Some(hd), hdpos) if hd == &AsrChecker::<'_, '_>::ProofHeader => {
					if hdfound {
						self.error_duplicated_section(hdpos);
					} else {
						hdfound = true;
						loop {
							match &self.asr.parse_proof() {
								Positioned(Some(AsrInstructionKind::Rup(id)), pos) => self.check_inference(*id, pos, false),
								Positioned(Some(AsrInstructionKind::Wsr(id)), pos) => self.check_inference(*id, pos, true),
								Positioned(Some(AsrInstructionKind::Del), pos) => self.check_del(pos),
								Positioned(None, pos) => {
									if self.config.check_refutation && self.stats.num_contradictions == 0usize {
										self.error_unrefuted(pos);
									}
									break;
								},
							}
							self.logger.flush(self.config.output);
						}
					}
				},
				Positioned(Some(hd), pos) if hd == &AsrChecker::<'_, '_>::CoreHeader => {
					self.error_misplaced_section(pos);
				}
				Positioned(None, pos) => {
					if !hdfound {
						self.error_missing_section(pos);
					}
					break;
				}
				_ => (),
			}
			self.logger.flush(self.config.output);
		}
		self.logger.flush(self.config.output);
	}
	fn check_inference(&mut self, id: ClauseIndex, pos: &StreamPosition, wsr: bool) {
		if wsr {
			self.stats.num_wsrs += 1usize;
		} else {
			self.stats.num_rups += 1usize;
		}
		if self.stats.num_proof_instructions() <= self.config.proof_limit {
			let cls = match self.parse_clause(id) {
				Some(None) => true,
				Some(Some(bx)) => {
					self.error_invalid_clause(pos, bx, wsr);
					true
				},
				None => {
					self.error_conflict_id(pos, id, wsr);
					false
				},
			};
			let wtn = if wsr {
				match self.parse_witness() {
					None => true,
					Some(bx) => {
						let ok = bx.1;
						self.error_invalid_witness(pos, bx);
						ok
					},
				}
			} else {
				true
			};
			if cls {
				match self.parse_chain(id, wsr) {
					None => (),
					Some(bx) => self.error_invalid_laterals(pos, id, bx),
				}
			} else {
				self.skip_chain(wsr);
			}
			let result = {
				let mut up = UnitPropagator::<'_>::new(&mut self.block, &mut self.chain, &self.db, &mut self.subst);
				if cls && wtn && self.config.check_derivation {
					up.propagate_inference(id, wsr);
					up.extract()
				} else {
					None
				}
			};
			if let Some(bx) = result {
				self.error_incorrect_inference(pos, id, bx, wsr);
			}
		} else {
			while let Some(_) = &self.asr.parse_clause() {
			}
			if wsr { while let Some(_) = &self.asr.parse_witness() {
			} }
			self.skip_chain(wsr);
		}
	}
	fn check_del(&mut self, pos: &StreamPosition) {
		self.stats.num_dels += 1usize;
		if self.stats.num_proof_instructions() <= self.config.proof_limit {
			while let Some(id) = &self.asr.parse_chain() {
				let rf = self.db.retrieve(*id);
				match rf {
					Some(r) => {
						self.stats.num_deletions += 1usize;
						if r.size() == 0usize {
							self.stats.num_contradictions -= 1usize;
						}
					},
					None => self.error_missing_deletion(pos, *id),
				}
				self.db.delete(*id);
			}
		} else {
			while let Some(_) = &self.asr.parse_chain() {
			}
		}
	}
	fn parse_clause(&mut self, id: ClauseIndex) -> Option<Option<Box<ClauseContainer>>> {
		let mut issues = ClauseIssuesBuilder::new();
		match self.db.open(&mut self.lits, id) {
			Some(mut writer) => {
				while let Some(lit) = &self.asr.parse_clause() {
					self.stats.max_variable |= unsafe { lit.variable_unchecked() };
					let test = writer.write(*lit);
					if !test {
						issues.set(|| writer.extract());
					}
					issues.push_literal(*lit);
				}
				let report = writer.close();
				if report.is_contradiction() {
					self.stats.num_contradictions += 1usize;
				}
				Some(issues.extract())
			},
			None => {
				while let Some(_) = &self.asr.parse_clause() {
				}
				None
			},
		}
	}
	fn parse_witness(&mut self) -> Option<Box<(WitnessContainer, bool)>> {
		let mut issues = WitnessIssuesBuilder::new();
		while let Some((var, lit)) = &self.asr.parse_witness() {
			self.stats.max_variable |= *var;
			lit.variable().map(|v| self.stats.max_variable |= v);
			let test = self.subst.set(*var, *lit);
			if test != InsertionTest::Alright {
				issues.set(|| self.subst.extract(), test == InsertionTest::Conflict);
			}
			issues.push_mapping(*var, *lit);
		}
		issues.extract()
	}
	fn skip_chain(&mut self, wsr: bool) {
		while let Some(_) = &self.asr.parse_chain() {
		}
		if wsr {
			while let Some(_) = &self.asr.parse_chain() {
				while let Some(_) = &self.asr.parse_chain() {
				}
			}
		}
	}
	fn parse_chain(&mut self, id: ClauseIndex, wsr: bool) -> Option<Box<Vec<(ClauseIndex, bool)>>> {
		let mut issues = ChainIssuesBuilder::new();
		let mut lat = id;
		let mut first = true;
		loop {
			if self.db.retrieve(lat).is_none() || (lat == id && !first) {
				issues.push_issue(lat, id, false);
				while let Some(_) = &self.asr.parse_chain() {
				}
			} else {
				first = false;
				match self.chain.open(lat) {
					Some(mut writer) => {
						while let Some(cid) = &self.asr.parse_chain() {
							writer.write(*cid);
						}
						writer.close();
					},
					None => {
						issues.push_issue(lat, id, true);
						while let Some(_) = &self.asr.parse_chain() {
						}
					},
				}
			}
			if wsr {
				match &self.asr.parse_chain() {
					Some(cid) => lat = *cid,
					None => break,
				}
			} else {
				break;
			}
		}
		issues.extract()
	}
	fn process_error(&mut self, err: VerificationFailure) {
		let handle = self.logger.handle(self.config.output);
		self.stats.errors.push_error(handle, err);
	}
	fn error_missing_section(&mut self, pos: &StreamPosition) {
		let pos = pos.source(self.asr.name());
		let err = VerificationFailure::missing_proof_section(pos);
		self.process_error(err);
	}	
	fn error_misplaced_section(&mut self, pos: &StreamPosition) {
		let pos = pos.source(self.asr.name());
		let err = VerificationFailure::misplaced_core_section(pos);
		self.process_error(err);
	}
	fn error_duplicated_section(&mut self, pos: &StreamPosition) {
		let pos = pos.source(self.asr.name());
		let err = VerificationFailure::duplicated_proof_section(pos);
		self.process_error(err);
	}
	fn error_conflict_id(&mut self, pos: &StreamPosition, id: ClauseIndex, wsr: bool) {
		let pos = pos.source(self.asr.name());
		let cls = self.db.extract(id).unwrap();
		let num = self.stats.num_proof_instructions();
		let err = if wsr {
			VerificationFailure::conflict_wsr_id(pos, num, id, ClauseContainer(cls))
		} else {
			VerificationFailure::conflict_rup_id(pos, num, id, ClauseContainer(cls))
		};
		self.process_error(err);
	}
	fn error_invalid_clause(&mut self, pos: &StreamPosition, issues: Box<ClauseContainer>, wsr: bool) {
		let num = self.stats.num_proof_instructions();
		let pos = pos.source(self.asr.name());
		let err = if wsr {
			VerificationFailure::invalid_wsr(pos, num, issues)
		} else {
			VerificationFailure::invalid_rup(pos, num, issues)
		};
		self.process_error(err);
	}
	fn error_invalid_witness(&mut self, pos: &StreamPosition, issues: Box<(WitnessContainer, bool)>) {
		let pos = pos.source(self.asr.name());
		let num = self.stats.num_proof_instructions();
		let err = VerificationFailure::invalid_witness(pos, num, issues);
		self.process_error(err);
	}
	fn error_invalid_laterals(&mut self, pos: &StreamPosition, id: ClauseIndex, bx: Box<Vec<(ClauseIndex, bool)>>) {
		let pos = pos.source(self.asr.name());
		let num = self.stats.num_proof_instructions();
		let cls = self.db.extract(id).unwrap();
		let err = VerificationFailure::invalid_laterals(pos, num, ClauseContainer(cls), bx);
		self.process_error(err);
	}
	fn error_incorrect_inference(&mut self, pos: &StreamPosition, id: ClauseIndex, bx: Box<Vec<(ClauseIndex, PropagationIssues, WitnessContainer)>>, wsr: bool) {
		let num = self.stats.num_proof_instructions();
		for (lat, issues, witness) in *bx {
			let pos = pos.source(self.asr.name());
			let cls = self.db.extract(id).unwrap();
			let err = if wsr {
				let latopt = if lat == id {
					None
				} else {
					Some(lat)
				};
				let latclause = self.db.extract(id).unwrap();
				VerificationFailure::incorrect_wsr(pos, num, ClauseContainer(cls), issues, latopt, ClauseContainer(latclause), witness)
			} else {
				VerificationFailure::incorrect_rup(pos, num, ClauseContainer(cls), issues)
			};
			self.process_error(err);
		}
	}
	fn error_missing_deletion(&mut self, pos: &StreamPosition, id: ClauseIndex) {
		let pos = pos.source(self.asr.name());
		let num = self.stats.num_proof_instructions();
		let err = VerificationFailure::missing_deletion(pos, num, id);
		self.process_error(err);
	}
	fn error_unrefuted(&mut self, pos: &StreamPosition) {
		let pos = pos.source(self.asr.name());
		let num = self.stats.num_proof_instructions();
		let err = VerificationFailure::unrefuted(pos, num);
		self.process_error(err);
	}
}