use std::{
	mem::{self},
	ops::{BitOr, BitOrAssign},
	path::{Path},
};

use either::{
	Either::{self, Left, Right},
};

use crate::{
	assignment::{Block, InsertionTest},
	clausedb::{ClauseDb, ClauseIndex, ClauseSet, ClauseSetWriter, ClauseDbWriter, ClauseDbMeta, ClauseReference},
	input::{FilePosition, Positionable},
	parser::{ParsingError, CnfParser, AsrParser, AsrInstructionKind, CnfHeaderStats},
	results::{VerificationResult, VerificationFailure},
	variable::{Variable, Literal},
};

pub struct DynCnfParser<'a> {
	parser: &'a mut dyn CnfParser
}
impl<'a> DynCnfParser<'a> {
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
	pub declared_variables: Option<Variable>,
	pub declared_clauses: usize,
	pub max_variable: Option<Variable>,
	pub max_index: Option<ClauseIndex>,
	pub num_premises: usize,
	pub num_cores: usize,
	pub num_instructions: usize,
	pub num_rups: usize,
	pub num_srs: usize,
	pub num_xrs: usize,
	pub num_deletions: usize,
}
impl CheckerStats {
	fn new() -> CheckerStats {
		CheckerStats {
			declared_variables: None,
			declared_clauses: 0usize,
			max_variable: None,
			max_index: None,
			num_premises: 0usize,
			num_cores: 0usize,
			num_instructions: 0usize,
			num_rups: 0usize,
			num_srs: 0usize,
			num_xrs: 0usize,
			num_deletions: 0usize,
		}
	}
}

pub struct CheckerConfig {
	pub cnf_file: Box<Path>,
	pub asr_file: Box<Path>,
	pub asr_binary: bool,
	pub check_core: bool,
	pub check_proof: bool,
	pub cnf_literal_repetition_allowed: bool,
	pub cnf_tautology_allowed: bool,
	pub asr_literal_repetition_allowed: bool,
	pub asr_tautology_allowed: bool,
	pub asr_mapping_repetition_allowed: bool,
	pub asr_null_propagation_allowed: bool,
	pub asr_overkill_propagation_allowed: bool,
}

pub struct CoreCheckerData {
	pub set: ClauseSet,
	pub db: ClauseDb,
	pub stats: CheckerStats,
}

pub struct CnfChecker<'a> {
	cnf: DynCnfParser<'a>,
	block: Block,
	config: &'a CheckerConfig,
}
impl<'a> CnfChecker<'a> {
	const CnfHeader: [u8; 8] = [b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8];
	pub fn new<'b: 'a, 'c: 'a, C: CnfParser>(
		cnf: &'b mut C,
		config: &'c CheckerConfig,
	) -> CnfChecker<'a> {
		CnfChecker::<'a> {
			cnf: DynCnfParser::<'a>::new(cnf),
			block: Block::new(),
			config: config,
		}
	}
	pub fn check(&mut self, data: &mut CoreCheckerData) -> VerificationResult<()> {
		match self.skip_to_cnf_header()? {
			Some(()) => (),
			None => Err(VerificationFailure::missing_cnf_section(&self.cnf))?,
		}
		let hdstats = self.cnf.parse_cnf_header()?;
		data.stats.declared_variables = hdstats.variables;
		data.stats.declared_clauses = hdstats.clauses;
		while let Some(()) = self.cnf.parse_formula()? {
			self.check_clause(data)?;
		}
		match self.skip_to_cnf_header()? {
			Some(()) => Ok(()),
			None => Err(VerificationFailure::duplicated_cnf_section(&self.cnf)),
		}
	}
	fn skip_to_cnf_header(&mut self) -> VerificationResult<Option<()>> {
		match self.cnf.skip_to_header()? {
			Some(CnfChecker::<'a>::CnfHeader) => Ok(Some(())),
			None => Ok(None),
			Some(hd) => Err(VerificationFailure::invalid_section(&self.cnf, hd)),
		}
	}
	fn check_clause<'b>(&mut self, data: &mut CoreCheckerData) -> VerificationResult<()> {
		data.stats.num_premises += 1usize;
		let mut writer = data.set.open(&mut self.block);
		while let Some(lit) = self.cnf.parse_clause()? {
			data.stats.max_variable |= unsafe { lit.variable_unchecked() };
			match writer.write(lit) {
				InsertionTest::Alright => (),
				InsertionTest::Repeated => if self.config.cnf_literal_repetition_allowed {
					()
				} else {
					let vec = writer.drain();
					return self.invalid_premise(vec, &data.stats, lit);
				},
				InsertionTest::Conflict => if self.config.cnf_tautology_allowed {
					writer.write_taut(lit);
				} else {
					let vec = writer.drain();
					return self.invalid_premise(vec, &data.stats, lit);
				},
			}
		}
		Ok(())
	}
	fn invalid_premise<T>(&mut self, mut vec: Vec<Literal>, stats: &CheckerStats, rep: Literal) -> VerificationResult<T> {
		vec.push(rep);
		while let Some(lit) = self.cnf.parse_clause()? {
			vec.push(lit)
		}
		if self.block.check(rep) {
			Err(VerificationFailure::premise_repetition(&self.cnf, stats.num_premises, vec, rep))
		} else {
			Err(VerificationFailure::premise_tautology(&self.cnf, stats.num_premises, vec, rep))
		}
	}
}

pub struct AsrCoreChecker<'a> {
	asr: DynAsrParser<'a>,
	block: Block,
	config: &'a CheckerConfig,
}
impl<'a> AsrCoreChecker<'a> {
	const ProofHeader: [u8; 8] = [b'p', b'r', b'o', b'o', b'f', 0u8, 0u8, 0u8];
	const CoreHeader: [u8; 8] = [b'c', b'o', b'r', b'e', 0u8, 0u8, 0u8, 0u8];
	pub fn new<'b: 'a, 'c: 'a, C: AsrParser>(
		asr: &'b mut C,
		config: &'c CheckerConfig,
	) -> AsrCoreChecker<'a> {
		AsrCoreChecker::<'a> {
			asr: DynAsrParser::<'a>::new(asr),
			block: Block::new(),
			config: config,
		}
	}
	pub fn check(&mut self, data: &mut CoreCheckerData) -> VerificationResult<()> {
		match self.skip_to_header(Self::CoreHeader)? {
			Some(()) => (),
			None => Err(VerificationFailure::missing_core_section(&self.asr))?,
		}
		loop { match self.asr.parse_core()? {
			Some(id) => self.check_core(id, data)?,
			None => break,
		} }
		match self.skip_to_header(Self::CoreHeader)? {
			None => Ok(()),
			Some(()) => Err(VerificationFailure::duplicated_core_section(&self.asr))?,
		}
	}
	fn skip_to_header(&mut self, header: [u8; 8]) -> VerificationResult<Option<()>> {
		loop { match self.asr.skip_to_header()? {
			Some(hd) if hd == header => break Ok(Some(())),
			None => break Ok(None),
			Some(Self::ProofHeader) | Some(Self::CoreHeader) => (),
			Some(hd) => Err(VerificationFailure::invalid_section(&self.asr, hd))?,
		} }
	}
	pub fn check_core(&mut self, id: ClauseIndex, data: &mut CoreCheckerData) -> VerificationResult<()> {
		data.stats.max_index |= id;
		data.stats.num_cores += 1usize;
		let mut writer: ClauseDbWriter<'_> = {
			let opt = data.db.open(&mut self.block, id);
			match opt {
				Some(dbw) => Ok(dbw),
				None => {
					mem::drop(opt);
					self.invalid_id(id, &data.stats)
				}
			}
		}?;
		let mut taut: bool = false;
		while let Some(lit) = self.asr.parse_clause()? {
			data.stats.max_variable |= unsafe { lit.variable_unchecked() };
			match writer.write(lit) {
				InsertionTest::Alright => (),
				InsertionTest::Repeated => if self.config.cnf_literal_repetition_allowed {
					()
				} else {
					let mut vec = writer.drain();
					return self.invalid_core(vec, &data.stats, lit);
				},
				InsertionTest::Conflict => if self.config.cnf_tautology_allowed {
					writer.write_taut(lit);
					taut = true;
				} else {
					let mut vec = writer.drain();
					return self.invalid_core(vec, &data.stats, lit);
				},
			}
		}
		let meta = ClauseDbMeta {
			tautology: taut,
		};
		writer.close(meta);
		let rf = data.db.retrieve(id).unwrap();
		let del = data.set.delete(&rf, &mut self.block);
		match del {
			Some(()) => Ok(()),
			None => self.incorrect_core(&rf, id, &data.stats),
		}
	}
	fn invalid_core<T>(&mut self, mut vec: Vec<Literal>, stats: &CheckerStats, rep: Literal) -> VerificationResult<T> {
		vec.push(rep);
		while let Some(lit) = self.asr.parse_clause()? {
			vec.push(lit);
		}
		if self.block.check(rep) {
			Err(VerificationFailure::core_repetition(&self.asr, stats.num_cores, vec, rep))
		} else {
			Err(VerificationFailure::core_tautology(&self.asr, stats.num_cores, vec, rep))
		}
	}
	fn invalid_id<T>(&mut self, id: ClauseIndex, stats: &CheckerStats) -> VerificationResult<T> {
		let mut vec = Vec::<Literal>::new();
		while let Some(lit) = self.asr.parse_clause()? {
			vec.push(lit);
		}
		Err(VerificationFailure::conflict_core_id(&self.asr, stats.num_cores, id, vec))
	}
	fn incorrect_core<T>(&mut self, rf: &ClauseReference, id: ClauseIndex, stats: &CheckerStats) -> VerificationResult<T> {
		let mut it = rf.iter();
		let mut vec = Vec::<Literal>::new();
		while let Some(&lit) = it.next() {
			vec.push(lit);
		}
		Err(VerificationFailure::conflict_core_id(&self.asr, stats.num_cores, id, vec))
	}
}

#[derive(Copy, Clone, Debug)]
pub enum Propagation {
	Conflict,
	Proper(Literal),
	Incorrect,
}
impl Propagation {
	pub const fn new() -> Propagation {
		Propagation::Conflict
	}
}
impl BitOr<Literal> for Propagation {
	type Output = Propagation;
	fn bitor(self, lit: Literal) -> Propagation {
		match self {
			Propagation::Conflict => Propagation::Proper(lit),
			_ => Propagation::Incorrect,
		}
	}
}
impl BitOrAssign<Literal> for Propagation {
	fn bitor_assign(&mut self, lit: Literal) {
		match self {
			Propagation::Conflict => *self = Propagation::Proper(lit),
			_ => *self = Propagation::Incorrect,
		}
	}
}

pub struct UnitPropagator<'a> {
	block: &'a mut Block,
	stack: &'a mut Vec<Literal>,
}
impl<'a> UnitPropagator<'a> {
	pub fn new<'b: 'a, 'c: 'a>(block: &'b mut Block, stack: &'c mut Vec<Literal>) -> UnitPropagator<'a> {
		UnitPropagator::<'a> {
			block: block,
			stack: stack,
		}
	}
	pub fn initialized<'b: 'a, 'c: 'a, 'd: 'a, I: Iterator<Item = &'d Literal>>(block: &'b mut Block, stack: &'c mut Vec<Literal>, it: I) -> UnitPropagator<'a> {
		let mut up = UnitPropagator::new(block, stack);
		for &lit in it {
			up.block.set(lit);
			up.stack.push(lit);
		}
		up
	}
	pub fn propagate(&mut self, rf: &ClauseReference) -> Option<Propagation> {
		let mut prop = Propagation::new();
		for &lit in rf.iter() {
			let test = self.block.set(lit);
			match test {
				InsertionTest::Conflict => (),
				InsertionTest::Alright => {
					self.stack.push(lit);
					prop |= lit;
				},
				InsertionTest::Repeated => None?,
			}
		}
		Some(prop)
	}
}
impl<'a> Drop for UnitPropagator<'a> {
	fn drop(&mut self) {
		self.block.clear_iter(self.stack.iter());
		self.stack.clear();
	}
}

// pub struct AsrProofChecker<'a> {
// 	asr: DynAsrParser<'a>,
// 	block: Block,
// 	config: &'a CheckerConfig,
// }
// impl AsrProofChecker<'a> {
// 	pub fn new<'b: 'a, 'c: 'a, C: AsrParser>(
// 		asr: &'b mut C,
// 		config: &'c CheckerConfig,
// 	) -> AsrCoreChecker<'a> {
// 		AsrCoreChecker::<'a> {
// 			asr: DynAsrParser::<'a>::new(asr),
// 			block: Block::new(),
// 			config: config,
// 		}
// 	}
// }