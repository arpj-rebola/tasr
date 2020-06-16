use std::{
	mem::{self},
	ops::{BitOr, BitOrAssign},
	path::{Path},
};

use either::{
	Either::{self, Left, Right},
};

use crate::{
	assignment::{Block, InsertionTest, BacktrackBlock, SubstitutionStack},
	chaindb::{ChainDb},
	clausedb::{ClauseDb, ClauseIndex, ClauseSet, ClauseSetWriter, ClauseDbWriter, ClauseDbMeta, ClauseReference, ClauseContainer, RawChainContainer},
	input::{FilePosition, Positionable},
	parser::{ParsingError, CnfParser, AsrParser, AsrInstructionKind, CnfHeaderStats},
	results::{VerificationResult, VerificationFailure},
	unitpropagation::{UnitPropagator, PropagationResult},
	variable::{Variable, Literal, MaybeVariable},
};

pub struct DynCnfParser<'a> {
	parser: &'a mut dyn CnfParser
}
impl<'a> DynCnfParser<'a> {
	const CnfHeader: [u8; 8] = [b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8];
	pub fn new(mt: &'a mut dyn CnfParser) -> DynCnfParser<'a> {
		DynCnfParser::<'a> { parser: mt }
	}
	pub fn skip_to_header(&mut self) -> VerificationResult<Option<[u8; 8]>> {
		self.parser.skip_to_header().map_err(VerificationFailure::from)
	}
	pub fn parse_cnf_header(&mut self) -> VerificationResult<CnfHeaderStats> {
		self.parser.parse_cnf_header().map_err(VerificationFailure::from)
	}
	pub fn parse_formula(&mut self) -> VerificationResult<Option<()>> {
		self.parser.parse_formula().map_err(VerificationFailure::from)
	}
	pub fn parse_clause(&mut self) -> VerificationResult<Option<Literal>> {
		self.parser.parse_clause().map_err(VerificationFailure::from)
	}
}
impl<'a> Positionable for DynCnfParser<'a> {
	fn position(&self) -> &FilePosition {
		self.parser.position()
	}
}

pub struct DynAsrParser<'a> {
	parser: &'a mut dyn AsrParser
}
impl<'a> DynAsrParser<'a> {
	const CoreHeader: [u8; 8] = [b'c', b'o', b'r', b'e', 0u8, 0u8, 0u8, 0u8];
	const ProofHeader: [u8; 8] = [b'p', b'r', b'o', b'o', b'f', 0u8, 0u8, 0u8];
	pub fn new(mt: &'a mut dyn AsrParser) -> DynAsrParser<'a> {
		DynAsrParser::<'a> { parser: mt }
	}
	pub fn skip_to_header(&mut self) -> VerificationResult<Option<[u8; 8]>> {
		self.parser.skip_to_header().map_err(VerificationFailure::from)
	}
	fn parse_core(&mut self) -> VerificationResult<Option<ClauseIndex>> {
		self.parser.parse_core().map_err(VerificationFailure::from)
	}
	fn parse_proof(&mut self) -> VerificationResult<Option<AsrInstructionKind>> {
		self.parser.parse_proof().map_err(VerificationFailure::from)
	}
	fn parse_clause(&mut self) -> VerificationResult<Option<Literal>> {
		self.parser.parse_clause().map_err(VerificationFailure::from)
	}
	fn parse_chain(&mut self) -> VerificationResult<Option<ClauseIndex>> {
		self.parser.parse_chain().map_err(VerificationFailure::from)
	}
	fn parse_witness(&mut self) -> VerificationResult<Option<(Variable, Literal)>> {
		self.parser.parse_witness().map_err(VerificationFailure::from)
	}
}
impl<'a> Positionable for DynAsrParser<'a> {
	fn position(&self) -> &FilePosition {
		self.parser.position()
	}
}

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

pub struct CheckerConfig {
	pub cnf_file: Option<Box<Path>>,
	pub asr_file: Option<Box<Path>>,
	pub asr_binary: bool,
	pub check_core: bool,
	pub check_proof: bool,
	pub permissive: bool,
}

pub struct AsrChecker<'a> {
	cnf: &'a mut dyn CnfParser,
	asr: &'a mut dyn AsrParser,
	db: ClauseDb,
	stats: CheckerStats,
	config: CheckerConfig,
}

enum InputPart {
	Cnf,
	Core,
	Proof,
}

pub struct CoreChecker<'a> {
	cnf: DynCnfParser<'a>,
	asr: DynAsrParser<'a>,
	db: &'a mut ClauseDb,
	stats: &'a mut CheckerStats,
	config: &'a CheckerConfig,
	set: ClauseSet,
}
impl<'a> CoreChecker<'a> {
	pub fn new<'b: 'a, 'c: 'b>(checker: &'b mut AsrChecker<'c>) -> CoreChecker<'a> {
		CoreChecker::<'a> {
			cnf: DynCnfParser::<'a>::new(&mut *checker.cnf),
			asr: DynAsrParser::<'a>::new(&mut *checker.asr),
			db: &mut checker.db,
			stats: &mut checker.stats,
			config: &checker.config,
			set: ClauseSet::new(),
		}
	}
	pub fn process(mut self) -> VerificationResult<()> {
		if self.config.check_core {
			self.check_cnf()?;
		}
		if self.config.check_core || self.config.check_proof {
			self.check_asr()?;
		}
		Ok(())
	}
	fn check_cnf(&mut self) -> VerificationResult<()> {
		let hdstats = match self.skip_to_cnf_header()? {
			Some(hd) => hd,
			None => self.error_missing_cnf_section()?,
		};
		self.check_cnf_formula()?;
		match self.skip_to_cnf_header()? {
			Some(_) => self.error_duplicated_cnf_section()?,
			None => self.check_cnf_stats(&hdstats),
		}
	}
	fn skip_to_cnf_header(&mut self) -> VerificationResult<Option<CnfHeaderStats>> {
		loop { match self.cnf.skip_to_header()? {
			Some(DynCnfParser::<'_>::CnfHeader) => break Ok(Some(self.cnf.parse_cnf_header()?)),
			None => break Ok(None),
			_ => (),
		} }
	}
	fn check_cnf_formula(&mut self) -> VerificationResult<()> {
		let mut block = Block::new();
		while let Some(()) = self.cnf.parse_formula()? {
			self.stats.num_premises += 1usize;
			self.parse_set_clause(&mut block)?;
		}
		Ok(())
	}
	fn parse_set_clause(&mut self, block: &mut Block) -> VerificationResult<()> {
		let mut writer = self.set.open(block);
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
				self.error_invalid_clause(&block, cls, l, None)?;
			}
		}
		Ok(())
	}
	fn check_cnf_stats(&self, hdstats: &CnfHeaderStats) -> VerificationResult<()> {
		if self.stats.max_variable > hdstats.variables {
			self.error_incorrect_num_variables(&hdstats)?
		} else if self.stats.num_premises != hdstats.clauses {
			self.error_incorrect_num_clauses(&hdstats)?
		} else {
			Ok(())
		}
	}
	fn check_asr(&mut self) -> VerificationResult<()> {
		let mut block = BacktrackBlock::new();
		match self.skip_to_asr_header()? {
			Some(true) => self.check_asr_core(&mut block)?,
			_ => self.error_missing_core_section()?,
		}
		match self.skip_to_asr_header()? {
			Some(false) => self.check_asr_proof(&mut block)?,
			Some(true) => self.error_duplicated_core_section()?,
			None => self.error_missing_proof_section()?,
		}
		match self.skip_to_asr_header()? {
			Some(false) => self.error_duplicated_proof_section()?,
			Some(true) => self.error_duplicated_core_section()?,
			None => Ok(()),
		}
	}
	fn skip_to_asr_header(&mut self) -> VerificationResult<Option<bool>> {
		loop { match self.asr.skip_to_header()? {
			Some(DynAsrParser::<'_>::CoreHeader) => break Ok(Some(true)),
			Some(DynAsrParser::<'_>::ProofHeader) => break Ok(Some(false)),
			None => break Ok(None),
			_ => (),
		} }
	}
	fn check_asr_core(&mut self, block: &mut BacktrackBlock) -> VerificationResult<()> {
		while let Some(id) = self.asr.parse_core()? {
			self.stats.num_cores += 1usize;
			self.parse_db_clause(id, unsafe { block.mut_block() }, false)?;
			if self.config.check_core {
				let rf = self.db.retrieve(id).unwrap();
				let del = self.set.delete(&rf, unsafe { block.mut_block() });
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
				let empty = unsafe { self.parse_db_clause(id, unsafe { block.mut_block() }, true) }?;
				self.parse_db_chain(&mut chain, id, None)?;
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
				refutation |= empty;
			},
			Some(AsrInstructionKind::Sr(id)) => {
				self.stats.num_srs += 1usize;
				let empty = unsafe { self.parse_db_clause(id, unsafe { block.mut_block() } , true) }?;
				self.parse_witness(&mut subst)?;
				if !self.check_witness(&mut subst, block, id) {
					self.error_unsatisfied_sr(&subst, id)?
				}
				while let Some(lat) = self.asr.parse_chain()? {
					self.parse_db_chain(&mut chain, id, Some(lat))?;
				}
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
		if !refutation {
			self.error_unrefuted()?
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
		fn cnf<'b>(cc: &mut CoreChecker<'b>) -> VerificationResult<Option<Literal>> {
			cc.cnf.parse_clause()
		}
		fn asr<'b>(cc: &mut CoreChecker<'b>) -> VerificationResult<Option<Literal>> {
			cc.asr.parse_clause()
		}
		let pos = match proof {
			None => self.cnf.position().clone(),
			Some(_) => self.asr.position().clone(),
		};
		let f = match proof {
			None => cnf,
			Some(_) => asr,
		};
		let num = match proof {
			None => self.stats.num_premises,
			Some(false) => self.stats.num_cores,
			Some(true) => self.stats.num_proof_instructions(),
		};
		clause.0.push(lit);
		while let Some(l) = f(self)? {
			clause.0.push(l);
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
