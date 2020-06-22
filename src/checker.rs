use std::{
	fmt::{self, Formatter, Display},
	io::{Stdin},
	path::{PathBuf},
};

use crate::{
	assignment::{Block, InsertionTest, BacktrackBlock, SubstitutionStack},
	chaindb::{ChainDb},
	clausedb::{ClauseDb, ClauseIndex, ClauseSet, ClauseDbMeta, ClauseContainer, RawChainContainer},
	input::{CompressionFormat, InputStream},
	parser::{AsrParser, DimacsParser, VbeParser, AsrInstructionKind, CnfHeaderStats},
	results::{VerificationResult, VerificationFailure},
	unitpropagation::{UnitPropagator, PropagationResult},
	variable::{Variable, Literal, MaybeVariable},
};

pub struct CheckerStats {
	pub max_variable: MaybeVariable,
	pub max_index: Option<ClauseIndex>,
	pub num_premises: usize,
	pub num_cores: usize,
	pub num_rups: usize,
	pub num_srs: usize,
	pub num_atomic_dels: usize,
	pub num_del_instructions: usize
}
impl CheckerStats {
	fn new() -> CheckerStats {
		CheckerStats {
			max_variable: MaybeVariable::None,
			max_index: None,
			num_premises: 0usize,
			num_cores: 0usize,
			num_rups: 0usize,
			num_srs: 0usize,
			num_atomic_dels: 0usize,
			num_del_instructions: 0usize,
		}
	}
	fn num_proof_instructions(&self) -> usize {
		self.num_rups + self.num_srs + self.num_del_instructions
	}
}
impl Display for CheckerStats {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		writeln!(f, "s {:<18} {}", "maximum variable", self.max_variable)?;
		writeln!(f, "s {:<18} {}", "maximum id", self.max_index.map(|x| x.index()).unwrap_or(0usize))?;
		writeln!(f, "s {:<18} {}", "premise clauses", self.num_premises)?;
		writeln!(f, "s {:<18} {}", "core clauses", self.num_cores)?;
		writeln!(f, "s {:<18} {}", "RUP inferences", self.num_rups)?;
		writeln!(f, "s {:<18} {}", "SR inferences", self.num_srs)?;
		writeln!(f, "s {:<18} {}", "deletion instructions", self.num_del_instructions)?;
		writeln!(f, "s {:<18} {}", "clause deletions", self.num_atomic_dels)?;
		writeln!(f, "s {:<18} {}", "total instructions", self.num_proof_instructions())?;
		Ok(())
    }
}

#[derive(Debug)]
pub struct CheckerConfig {
	pub check_core: bool,
	pub check_derivation: bool,
	pub check_refutation: bool,
	pub permissive: bool,
}

pub struct CheckerInputConfig {
	pub path: Option<PathBuf>,
	pub compression: CompressionFormat,
	pub binary: bool,
}

pub struct AsrChecker<'a> {
	cnf: Box<dyn 'a + AsrParser<VerificationFailure>>,
	asr: Box<dyn 'a + AsrParser<VerificationFailure>>,
	db: ClauseDb,
	stats: CheckerStats,
	config: CheckerConfig,
}
impl<'a> AsrChecker<'a> {
	const CnfHeader: [u8; 8] = [b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8];
	const CoreHeader: [u8; 8] = [b'c', b'o', b'r', b'e', 0u8, 0u8, 0u8, 0u8];
	const ProofHeader: [u8; 8] = [b'p', b'r', b'o', b'o', b'f', 0u8, 0u8, 0u8];
	pub fn process<'b: 'a>(cnf: CheckerInputConfig, asr: CheckerInputConfig, stdin: &'b Stdin, config: CheckerConfig) -> CheckingResult {
		match AsrChecker::<'a>::new(cnf, asr, stdin, config) {
			Ok(mut checker) => {
				let res = checker.check();
				CheckingResult {
					result: res,
					stats: checker.stats,
				}
			},
			Err(err) => {
				CheckingResult {
					result: Err(err),
					stats: CheckerStats::new(),
				}
			}
		}
	}
	fn new<'b: 'a>(cnf: CheckerInputConfig, asr: CheckerInputConfig, stdin: &'b Stdin, config: CheckerConfig) -> VerificationResult<AsrChecker<'a>> {
		let cnf_input = match &cnf.path {
			Some(path) => InputStream::<'a, VerificationFailure>::open(&path, cnf.compression)?,
			None => InputStream::<'a, VerificationFailure>::string("p cnf 0 0", "(dummy)"),
		};
		let asr_input = match &asr.path {
			Some(path) => InputStream::<'a, VerificationFailure>::open(&path, asr.compression)?,
			None => InputStream::<'a, VerificationFailure>::stdin(stdin),
		};
		let cnf: Box<dyn 'a + AsrParser<VerificationFailure>> = if cnf.binary {
			Box::new(DimacsParser::<'a, VerificationFailure>::new(cnf_input, "CNF"))
		} else {
			Box::new(VbeParser::<'a, VerificationFailure>::new(cnf_input, "CNF"))
		};
		let asr: Box<dyn 'a + AsrParser<VerificationFailure>> = if asr.binary {
			Box::new(DimacsParser::<'a, VerificationFailure>::new(asr_input, "ASR"))
		} else {
			Box::new(VbeParser::<'a, VerificationFailure>::new(asr_input, "ASR"))
		};
		Ok(AsrChecker {
			cnf: cnf,
			asr: asr,
			db: ClauseDb::new(),
			stats: CheckerStats::new(),
			config: config,
		})
	}
	pub fn check(&mut self) -> VerificationResult<()> {
		let mut block = BacktrackBlock::new();
		{
			let mut set = ClauseSet::new();
			if self.config.check_core {
				self.check_cnf(&mut set, &mut block)?;
			}
			self.check_core(&mut set, &mut block)?;
		}
		self.check_proof(&mut block)?;
		Ok(())
	}
	fn check_cnf(&mut self, set: &mut ClauseSet, block: &mut BacktrackBlock) -> VerificationResult<()> {
		let mut hdfound = false;
		while let Some(hd) = self.cnf.skip_to_header()? {
			if &hd == &Self::CnfHeader {
				if !hdfound {
					hdfound = true;
					self.check_cnf_formula(set, block)?;					
				} else {
					self.error_duplicated_cnf_section()?
				}
			}
		}
		if !hdfound {
			self.error_missing_cnf_section()?
		}
		Ok(())
	}
	fn check_core(&mut self, set: &mut ClauseSet, block: &mut BacktrackBlock) -> VerificationResult<()> {
		let mut hdfound = false;
		while let Some(hd) = self.asr.skip_to_header()? {
			if &hd == &Self::CoreHeader {
				hdfound = true;
				self.check_asr_core(set, block)?;
				break;
			}
		}
		if !hdfound {
			self.error_missing_core_section()?
		}
		Ok(())
	}
	fn check_proof(&mut self, block: &mut BacktrackBlock) -> VerificationResult<()> {
		let mut hdfound = false;
		while let Some(hd) = self.asr.skip_to_header()? {
			if &hd == &Self::ProofHeader {
				if !hdfound {
					hdfound = true;
					if self.config.check_derivation || self.config.check_refutation {
						self.check_asr_proof(block)?;
					}
				} else {
					self.error_duplicated_proof_section()?
				}
			} else if &hd == &Self::CoreHeader {
				self.error_duplicated_core_section()?
			}
		}
		if !hdfound {
			self.error_missing_proof_section()?
		}
		Ok(())
	}
	fn check_cnf_formula(&mut self, set: &mut ClauseSet, block: &mut BacktrackBlock) -> VerificationResult<()> {
		let hdstats = self.cnf.parse_cnf_header()?;
		while let Some(()) = self.cnf.parse_formula()? {
			self.stats.num_premises += 1usize;
			self.parse_set_clause(set, block)?;
		}
		if self.stats.max_variable > hdstats.variables {
			self.error_incorrect_num_variables(&hdstats)?
		} else if self.stats.num_premises != hdstats.clauses {
			self.error_incorrect_num_clauses(&hdstats)?
		} else {
			Ok(())
		}
	}
	fn check_asr_core(&mut self, set: &mut ClauseSet, block: &mut BacktrackBlock) -> VerificationResult<()> {
		while let Some(id) = self.asr.parse_core()? {
			self.stats.num_cores += 1usize;
			self.parse_db_clause(id, unsafe { block.mut_block() }, false)?;
			if self.config.check_core {
				let rf = self.db.retrieve(id).unwrap();
				let del = set.delete(&rf, unsafe { block.mut_block() });
				match del {
					Some(()) => (),
					None => self.error_incorrect_core(id)?,
				}
			}
		}
		Ok(())
	}
	fn check_asr_proof(&mut self, block: &mut BacktrackBlock) -> VerificationResult<()> {
		let mut chain = ChainDb::new();
		let mut subst = SubstitutionStack::new();
		let mut refutation = false;
		loop { match self.asr.parse_proof()? {
			Some(AsrInstructionKind::Rup(id)) => {
				self.stats.num_rups += 1usize;
				let empty = self.parse_db_clause(id, unsafe { block.mut_block() }, true)?;
				self.parse_db_chain(&mut chain, id, None)?;
				if self.config.check_derivation {
					let result = {
						let mut up = UnitPropagator::<'_>::new(block, &mut chain, &self.db, &mut subst);
						let result = up.propagate_rup(id, self.config.permissive).unwrap();
						if result.error() || !result.conflict() {
							up.freeze();
						}
						result
					};
					if result.error() || !result.conflict() {
						self.error_incorrect_rup(&chain, id, result)?
					}
				}
				refutation |= empty;
			},
			Some(AsrInstructionKind::Sr(id)) => {
				self.stats.num_srs += 1usize;
				let empty = self.parse_db_clause(id, unsafe { block.mut_block() } , true)?;
				self.parse_witness(&mut subst)?;
				if self.config.check_derivation {
					if !self.check_witness(&mut subst, block, id) {
						self.error_unsatisfied_sr(&subst, id)?
					}
				}
				while let Some(lat) = self.asr.parse_chain()? {
					self.parse_db_chain(&mut chain, id, Some(lat))?;
				}
				if self.config.check_derivation {
					let (result, optcid) = {
						let mut up = UnitPropagator::<'_>::new(block, &mut chain, &self.db, &mut subst);
						let mut it = self.db.iter();
						loop { match it.next() {
							Some(cid) => {
								let result = up.propagate_sr(id, cid, self.config.permissive).unwrap();
								if result.error() || !result.conflict() {
									up.freeze();
									break (result, Some(cid));
								}
							},
							None => break (PropagationResult::Stable, None)
						} }
					};
					if result.error() || !result.conflict() {
						self.error_incorrect_sr(&chain, &subst, id, optcid.unwrap(), result)?
					}
				}
				refutation |= empty;
			},
			Some(AsrInstructionKind::Del) => {
				self.stats.num_del_instructions += 1usize;
				while let Some(id) = self.asr.parse_chain()? {
					match self.db.delete(id) {
						Some(()) => self.stats.num_atomic_dels += 1usize,
						None => if !self.config.permissive {
							self.error_missing_deletion(id)?
						},
					}
				}
			}
			None => break,
		} }
		if self.config.check_refutation{
			if !refutation {
				self.error_unrefuted()?
			}
		}
		Ok(())
	}
	fn parse_set_clause(&mut self, set: &mut ClauseSet, block: &mut BacktrackBlock) -> VerificationResult<()> {
		let mut writer = set.open(unsafe { block.mut_block() });
		let lit = {
			loop { match self.cnf.parse_clause()? {
				Some(lit) => {
					self.stats.max_variable |= unsafe { lit.variable_unchecked() };
					match writer.write(lit) {
						InsertionTest::Alright => (),
						_ => break Some(lit),
					}
				},
				None => break None,
			} }
		};
		match lit {
			None => writer.close(),
			Some(l) => {
				let cls = writer.extract();
				self.error_invalid_clause(unsafe { block.mut_block() }, cls, l, None)?;
			}
		}
		Ok(())
	}
	fn parse_db_clause(&mut self, id: ClauseIndex, block: &mut Block, proof: bool) -> VerificationResult<bool> {
		let mut empty = true;
		let processed = match self.db.open(block, id) {
			Some(mut writer) => loop { match self.asr.parse_clause()? {
				Some(lit) => {
					empty = false;
					self.stats.max_variable |= unsafe { lit.variable_unchecked() };
					match writer.write(lit) {
						InsertionTest::Alright => (),
						_ => break Some(Some((writer.extract(), lit))),
					}
				},
				None => {
					let meta = ClauseDbMeta {};
					writer.close(meta);
					break Some(None)
				},
			} },
			None => None,
		};
		match processed {
			Some(None) => Ok(empty),
			Some(Some((cls, lit))) => self.error_invalid_clause(&block, cls, lit, Some(proof))?,
			None => self.error_conflict_id(id, proof)?,
		}
	}
	fn parse_witness(&mut self, subst: &mut SubstitutionStack) -> VerificationResult<()> {
		while let Some((var, lit)) = self.asr.parse_witness()? {
			match subst.set(var, lit) {
				InsertionTest::Alright => (),
				_ => self.error_invalid_witness(subst, var, lit)?,
			}
		}
		Ok(())
	}
	fn parse_db_chain(&mut self, chain: &mut ChainDb, id: ClauseIndex, latid: Option<ClauseIndex>) -> VerificationResult<()> {
		match latid {
			Some(lid) => if self.db.retrieve(lid).is_none() || id == lid {
				self.error_missing_lateral_sr(id, lid)?;
			},
			None => (),
		}
		let repeated = {
			match chain.open(latid) {
				Some(mut chw) => {
					while let Some(id) = self.asr.parse_chain()? {
						chw.write(id);
					}
					chw.close();
					false
				},
				None => true,
			}
		};
		if repeated {
			self.error_repeated_lateral_sr(chain, id, latid.unwrap())
		} else {
			Ok(())
		}
	}
	fn check_witness(&mut self, subst: &mut SubstitutionStack, block: &mut BacktrackBlock, id: ClauseIndex) -> bool {
		let rf = self.db.retrieve(id).unwrap();
		let mut it = rf.iter();
		let result = loop { match it.next() {
			Some(lit) => match block.set(lit.complement()) {
				InsertionTest::Conflict => break false,
				_ => (),
			},
			None => break true,
		} };
		block.clear();
		subst.clear();
		result
	}
	fn error_missing_cnf_section<T>(&self) -> VerificationResult<T> {
		Err(VerificationFailure::missing_cnf_section(self.cnf.position().clone()))
	}
	fn error_missing_core_section<T>(&self) -> VerificationResult<T> {
		Err(VerificationFailure::missing_core_section(self.asr.position().clone()))
	}
	fn error_missing_proof_section<T>(&self) -> VerificationResult<T> {
		Err(VerificationFailure::missing_proof_section(self.asr.position().clone()))
	}
	fn error_duplicated_cnf_section<T>(&self) -> VerificationResult<T> {
		Err(VerificationFailure::duplicated_cnf_section(self.cnf.position().clone()))
	}
	fn error_duplicated_core_section<T>(&self) -> VerificationResult<T> {
		Err(VerificationFailure::duplicated_core_section(self.cnf.position().clone()))
	}
	fn error_duplicated_proof_section<T>(&self) -> VerificationResult<T> {
		Err(VerificationFailure::duplicated_proof_section(self.cnf.position().clone()))
	}
	fn error_incorrect_num_variables<T>(&self, hdstats: &CnfHeaderStats) -> VerificationResult<T> {
		let pos = self.cnf.position().clone();
		let vars = self.stats.max_variable;
		let stat = hdstats.variables;
		Err(VerificationFailure::incorrect_num_variables(pos, vars, stat))
	}
	fn error_incorrect_num_clauses<T>(&self, hdstats: &CnfHeaderStats) -> VerificationResult<T> {
		let pos = self.cnf.position().clone();
		let cls = self.stats.num_premises;
		let stat = hdstats.clauses;
		Err(VerificationFailure::incorrect_num_clauses(pos, cls, stat))
	}
	fn error_invalid_clause<T>(&mut self, block: &Block, mut clause: ClauseContainer, lit: Literal, proof: Option<bool>) -> VerificationResult<T> {
		let pos = match proof {
			None => self.cnf.position().clone(),
			Some(_) => self.asr.position().clone(),
		};
		let num = match proof {
			None => self.stats.num_premises,
			Some(false) => self.stats.num_cores,
			Some(true) => self.stats.num_proof_instructions(),
		};
		clause.0.push(lit);
		if proof.is_none() {
			while let Some(l) = self.cnf.parse_clause()? {
				clause.0.push(l);
			}
		} else {
			while let Some(l) = self.asr.parse_clause()? {
				clause.0.push(l);
			}
		}
		match (proof, block.check(lit)) {
			(None, true) => Err(VerificationFailure::premise_repetition(pos, num, clause, lit)),
			(None, false) => Err(VerificationFailure::premise_tautology(pos, num, clause, lit)),
			(Some(false), true) => Err(VerificationFailure::core_repetition(pos, num, clause, lit)),
			(Some(false), false) => Err(VerificationFailure::core_tautology(pos, num, clause, lit)),
			(Some(true), true) => Err(VerificationFailure::inference_repetition(pos, num, clause, lit)),
			(Some(true), false) => Err(VerificationFailure::inference_tautology(pos, num, clause, lit)),
		}
	}
	fn error_invalid_witness<T>(&mut self, subst: &mut SubstitutionStack, var: Variable, lit: Literal) -> VerificationResult<T> {
		let mut witness = subst.extract();
		witness.0.push((var, lit));
		while let Some((v, l)) = self.asr.parse_witness()? {
			witness.0.push((v, l));
		}
		let num = self.stats.num_proof_instructions();
		let pos = self.asr.position().clone();
		match subst.set(var, lit) {
			InsertionTest::Repeated => Err(VerificationFailure::witness_repetition(pos, num, witness)),
			_ => Err(VerificationFailure::witness_inconsistency(pos, num, witness)),
		}
	}
	fn error_conflict_id<T>(&mut self, id: ClauseIndex, proof: bool) -> VerificationResult<T> {
		let pos = self.asr.position().clone();
		let cls = self.db.extract(id).unwrap();
		let num = if proof {
			self.stats.num_proof_instructions()
		} else {
			self.stats.num_cores
		};
		if proof {
			Err(VerificationFailure::conflict_inference_id(pos, num, id, cls))
		} else {
			Err(VerificationFailure::conflict_core_id(pos, num, id, cls))
		}
	}
	fn error_incorrect_core<T>(&mut self, id: ClauseIndex) -> VerificationResult<T> {
		let cls = self.db.extract(id).unwrap();
		let num = self.stats.num_cores;
		let pos = self.asr.position().clone();
		Err(VerificationFailure::incorrect_core(pos, num, id, cls))
	}
	fn error_incorrect_rup<T>(&mut self, chain: &ChainDb, id: ClauseIndex, result: PropagationResult) -> VerificationResult<T> {
		let pos = self.asr.position().clone();
		let num = self.stats.num_proof_instructions();
		let cls = self.db.extract(id).unwrap();
		let chn = chain.retrieve(None).unwrap();
		let idchn = self.db.extract_chain(chn, result.cutoff()).unwrap();
		if result.missing() {
			let issue = chn.get(result.cutoff()).unwrap();
			Err(VerificationFailure::missing_rup(pos, num, cls, *issue, idchn))
		} else if result.null() {
			let issue = chn.get(result.cutoff()).unwrap();
			let issuecls = self.db.extract(*issue).unwrap();
			Err(VerificationFailure::null_rup(pos, num, cls, *issue, issuecls, idchn))
		} else {
			Err(VerificationFailure::unchained_rup(pos, num, cls, idchn))
		}
	}
	fn error_incorrect_sr<T>(&mut self, chain: &ChainDb, subst: &SubstitutionStack, id: ClauseIndex, latid: ClauseIndex, result: PropagationResult) -> VerificationResult<T> {
		let pos = self.asr.position().clone();
		let num = self.stats.num_proof_instructions();
		let wtn = subst.extract();
		let cls = self.db.extract(id).unwrap();
		let latcls = self.db.extract(latid).unwrap();
		let chn = chain.extract(Some(latid));
		let idchn = self.db.extract_chain(&chn, result.cutoff()).unwrap();
		if result.missing() {
			let issue = chn.get(result.cutoff()).unwrap();
			Err(VerificationFailure::missing_sr(pos, num, cls, latid, latcls, wtn, *issue, idchn))
		} else if result.null() {
			let issue = chn.get(result.cutoff()).unwrap();
			let issuecls = self.db.extract(*issue).unwrap();
			Err(VerificationFailure::null_sr(pos, num, cls, latid, latcls, wtn, *issue, issuecls, idchn))
		} else if chain.exists(latid) {
			Err(VerificationFailure::unchained_sr(pos, num, cls, latid, latcls, wtn, idchn))
		} else {
			Err(VerificationFailure::missing_suffix_sr(pos, num, cls, wtn, latid, latcls))
		}
	}
	fn error_unsatisfied_sr<T>(&mut self, subst: &SubstitutionStack, id: ClauseIndex) -> VerificationResult<T> {
		let pos = self.asr.position().clone();
		let num = self.stats.num_proof_instructions();
		let cls = self.db.extract(id).unwrap();
		let wtn = subst.extract();
		Err(VerificationFailure::unsatisfied_sr(pos, num, cls, wtn))
	}
	fn error_missing_lateral_sr<T>(&mut self, id: ClauseIndex, latid: ClauseIndex) -> VerificationResult<T> {
		let pos = self.asr.position().clone();
		let num = self.stats.num_proof_instructions();
		let cls = self.db.extract(id).unwrap();
		let mut chn = Vec::<ClauseIndex>::new();
		while let Some(cid) = self.asr.parse_chain()? {
			chn.push(cid);
		}
		Err(VerificationFailure::missing_lateral_sr(pos, num, cls, latid, RawChainContainer(chn)))
	}
	fn error_repeated_lateral_sr<T>(&mut self, chain: &ChainDb, id: ClauseIndex, latid: ClauseIndex) -> VerificationResult<T> {
		let pos = self.asr.position().clone();
		let num = self.stats.num_proof_instructions();
		let cls = self.db.extract(id).unwrap();
		let latcls = self.db.extract(latid).unwrap();
		let chn1 = chain.extract(Some(latid));
		let mut chn2 = Vec::<ClauseIndex>::new();
		while let Some(cid) = self.asr.parse_chain()? {
			chn2.push(cid);
		}
		Err(VerificationFailure::repeated_lateral_sr(pos, num, cls, latid, latcls, RawChainContainer(chn1), RawChainContainer(chn2)))
	}
	fn error_missing_deletion<T>(&mut self, id: ClauseIndex) -> VerificationResult<T> {
		let pos = self.asr.position().clone();
		let num = self.stats.num_proof_instructions();
		Err(VerificationFailure::missing_deletion(pos, num, id))
	}
	fn error_unrefuted<T>(&mut self) -> VerificationResult<T> {
		let pos = self.asr.position().clone();
		Err(VerificationFailure::unrefuted(pos))
	}
}

pub struct CheckingResult {
	pub result: VerificationResult<()>,
	pub stats: CheckerStats,
}
impl CheckingResult {
	pub fn final_result(&self) -> Option<bool> {
		match &self.result {
			Ok(()) => Some(true),
			Err(err) => if err.failure() {
				Some(false)
			} else {
				None
			},
		}
	}
}