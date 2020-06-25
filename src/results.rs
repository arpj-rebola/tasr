use std::{
	error::{Error},
	io::{self},
	fmt::{self, Display, Formatter, Binary},
};
use crate::{
	clausedb::{ClauseIndex, ClauseContainer, ChainContainer, WitnessContainer, RawChainContainer},
	input::{FilePosition},
	parser::{ParsingError},
	variable::{Literal, MaybeVariable},
};

#[derive(Debug)]
pub struct IncorrectCnfStats<T: Display> {
	found: T,
	expected: T,
	pos: FilePosition,
}

#[derive(Debug)]
pub struct WrongSection {
	header: String,
	pos: FilePosition,
}

#[derive(Debug)]
pub struct InvalidClause {
	num: usize,
	clause: ClauseContainer,
	pos: FilePosition,
	issue: Literal,
}

#[derive(Debug)]
pub struct InvalidWitness {
	num: usize,
	witness: WitnessContainer,
	pos: FilePosition,
}

#[derive(Debug)]
pub struct ConflictId {
	num: usize,
	id: ClauseIndex,
	clause: ClauseContainer,
	pos: FilePosition,
}

#[derive(Debug)]
pub struct LateralInfo {
	witness: WitnessContainer,
	lateral: Option<(ClauseIndex, ClauseContainer)>,
}

#[derive(Debug)]
pub struct IncorrectChain {
	num: usize,
	clause: ClauseContainer,
	chain: ChainContainer,
	issue: Option<ClauseIndex>,
	issue_clause: Option<ClauseContainer>,
	lateral: Option<LateralInfo>,
	pos: FilePosition,
}

#[derive(Debug)]
pub struct MissingLateral {
	num: usize,
	clause: ClauseContainer,
	lateral: ClauseIndex,
	chain: RawChainContainer,
	pos: FilePosition,
}

#[derive(Debug)]
pub struct RepeatedLateral {
	num: usize,
	clause: ClauseContainer,
	lateral: (ClauseIndex, ClauseContainer),
	chains: (RawChainContainer, RawChainContainer),
	pos: FilePosition,
}

#[derive(Debug)]
pub struct MissingDeletion {
	num: usize,
	id: ClauseIndex,
	pos: FilePosition,
}

#[derive(Debug)]
pub enum VerificationFailure {
	InputError(Box<io::Error>),
	ParsingError(Box<ParsingError>),
	IncorrectNumVariables(Box<IncorrectCnfStats<MaybeVariable>>),
	IncorrectNumClauses(Box<IncorrectCnfStats<usize>>),
	MissingCnfSection(Box<WrongSection>),
	DuplicatedCnfSection(Box<WrongSection>),
	MissingCoreSection(Box<WrongSection>),
	DuplicatedCoreSection(Box<WrongSection>),
	MissingProofSection(Box<WrongSection>),
	DuplicatedProofSection(Box<WrongSection>),
	PremiseTautology(Box<InvalidClause>),
	CoreTautology(Box<InvalidClause>),
	InferenceTautology(Box<InvalidClause>),
	PremiseRepetition(Box<InvalidClause>),
	CoreRepetition(Box<InvalidClause>),
	InferenceRepetition(Box<InvalidClause>),
	WitnessInconsistency(Box<InvalidWitness>),
	WitnessRepetition(Box<InvalidWitness>),
	ConflictCoreId(Box<ConflictId>),
	ConflictInferenceId(Box<ConflictId>),
	IncorrectCore(Box<ConflictId>),
	UnchainedRup(Box<IncorrectChain>),
	NullChainRup(Box<IncorrectChain>),
	MissingChainRup(Box<IncorrectChain>),
	UnchainedWsr(Box<IncorrectChain>),
	NullChainWsr(Box<IncorrectChain>),
	MissingChainWsr(Box<IncorrectChain>),
	RepeatedLateral(Box<RepeatedLateral>),
	MissingLateral(Box<MissingLateral>),
	MissingDeletion(Box<MissingDeletion>),
	Unrefuted(Box<FilePosition>),
}
impl VerificationFailure {
	pub fn incorrect_num_variables(pos: FilePosition, found: MaybeVariable, expected: MaybeVariable) -> VerificationFailure {
		VerificationFailure::IncorrectNumVariables(Box::<IncorrectCnfStats<MaybeVariable>>::new(IncorrectCnfStats::<MaybeVariable> {
			found: found,
			expected: expected,
			pos: pos,
		}))
	}
	pub fn incorrect_num_clauses(pos: FilePosition, found: usize, expected: usize) -> VerificationFailure {
		VerificationFailure::IncorrectNumClauses(Box::<IncorrectCnfStats<usize>>::new(IncorrectCnfStats::<usize> {
			found: found,
			expected: expected,
			pos: pos,
		}))
	}
	pub fn missing_cnf_section(pos: FilePosition) -> VerificationFailure {
		VerificationFailure::MissingCnfSection(Box::<WrongSection>::new(WrongSection {
			pos: pos,
			header: "cnf".to_string(),
		}))
	}
	pub fn duplicated_cnf_section(pos: FilePosition) -> VerificationFailure {
		VerificationFailure::DuplicatedCnfSection(Box::<WrongSection>::new(WrongSection {
			pos: pos,
			header: "cnf".to_string(),
		}))
	}
	pub fn missing_core_section(pos: FilePosition) -> VerificationFailure {
		VerificationFailure::MissingCoreSection(Box::<WrongSection>::new(WrongSection {
			pos: pos,
			header: "asrcore".to_string(),
		}))
	}
	pub fn duplicated_core_section(pos: FilePosition) -> VerificationFailure {
		VerificationFailure::DuplicatedCoreSection(Box::<WrongSection>::new(WrongSection {
			pos: pos,
			header: "asrcore".to_string(),
		}))
	}
	pub fn missing_proof_section(pos: FilePosition) -> VerificationFailure {
		VerificationFailure::MissingProofSection(Box::<WrongSection>::new(WrongSection {
			pos: pos,
			header: "asrproof".to_string(),
		}))
	}
	pub fn duplicated_proof_section(pos: FilePosition) -> VerificationFailure {
		VerificationFailure::DuplicatedProofSection(Box::<WrongSection>::new(WrongSection {
			pos: pos,
			header: "asrproof".to_string(),
		}))
	}
	pub fn premise_repetition(pos: FilePosition, num: usize, clause: ClauseContainer, issue: Literal) -> VerificationFailure {
		VerificationFailure::PremiseRepetition(Box::<InvalidClause>::new(InvalidClause {
			num: num,
			clause: clause,
			pos: pos,
			issue: issue,
		}))
	}
	pub fn premise_tautology(pos: FilePosition, num: usize, clause: ClauseContainer, issue: Literal) -> VerificationFailure {
		VerificationFailure::PremiseTautology(Box::<InvalidClause>::new(InvalidClause {
			num: num,
			clause: clause,
			pos: pos,
			issue: issue,
		}))
	}
	pub fn core_repetition(pos: FilePosition, num: usize, clause: ClauseContainer, issue: Literal) -> VerificationFailure {
		VerificationFailure::CoreRepetition(Box::<InvalidClause>::new(InvalidClause {
			num: num,
			clause: clause,
			pos: pos,
			issue: issue,
		}))
	}
	pub fn core_tautology(pos: FilePosition, num: usize, clause: ClauseContainer, issue: Literal) -> VerificationFailure {
		VerificationFailure::CoreTautology(Box::<InvalidClause>::new(InvalidClause {
			num: num,
			clause: clause,
			pos: pos,
			issue: issue,
		}))
	}
	pub fn inference_repetition(pos: FilePosition, num: usize, clause: ClauseContainer, issue: Literal) -> VerificationFailure {
		VerificationFailure::InferenceRepetition(Box::<InvalidClause>::new(InvalidClause {
			num: num,
			clause: clause,
			pos: pos,
			issue: issue,
		}))
	}
	pub fn inference_tautology(pos: FilePosition, num: usize, clause: ClauseContainer, issue: Literal) -> VerificationFailure {
		VerificationFailure::InferenceTautology(Box::<InvalidClause>::new(InvalidClause {
			num: num,
			clause: clause,
			pos: pos,
			issue: issue,
		}))
	}
	pub fn witness_repetition(pos: FilePosition, num: usize, witness: WitnessContainer) -> VerificationFailure {
		VerificationFailure::WitnessRepetition(Box::<InvalidWitness>::new(InvalidWitness {
			num: num,
			witness: witness,
			pos: pos,
		}))
	}
	pub fn witness_inconsistency(pos: FilePosition, num: usize, witness: WitnessContainer) -> VerificationFailure {
		VerificationFailure::WitnessInconsistency(Box::<InvalidWitness>::new(InvalidWitness {
			num: num,
			witness: witness,
			pos: pos,
		}))
	}
	pub fn conflict_core_id(pos: FilePosition, num: usize, id: ClauseIndex, clause: ClauseContainer) -> VerificationFailure {
		VerificationFailure::ConflictCoreId(Box::<ConflictId>::new(ConflictId {
			num: num,
			id: id,
			clause: clause,
			pos: pos,
		}))
	}
	pub fn conflict_inference_id(pos: FilePosition, num: usize, id: ClauseIndex, clause: ClauseContainer) -> VerificationFailure {
		VerificationFailure::ConflictInferenceId(Box::<ConflictId>::new(ConflictId {
			num: num,
			id: id,
			clause: clause,
			pos: pos,
		}))
	}
	pub fn incorrect_core(pos: FilePosition, num: usize, id: ClauseIndex, clause: ClauseContainer) -> VerificationFailure {
		VerificationFailure::IncorrectCore(Box::<ConflictId>::new(ConflictId {
			num: num,
			id: id,
			clause: clause,
			pos: pos,
		}))
	}
	pub fn unchained_rup(pos: FilePosition, num: usize, clause: ClauseContainer, chain: ChainContainer) -> VerificationFailure {
		VerificationFailure::UnchainedRup(Box::<IncorrectChain>::new(IncorrectChain {
			num: num,
			clause: clause,
			chain: chain,
			issue: None,
			issue_clause: None,
			lateral: None,
			pos: pos,
		}))
	}
	pub fn null_rup(pos: FilePosition, num: usize, clause: ClauseContainer, issueid: ClauseIndex, issueclause: ClauseContainer, chain: ChainContainer) -> VerificationFailure {
		VerificationFailure::NullChainRup(Box::<IncorrectChain>::new(IncorrectChain {
			num: num,
			clause: clause,
			chain: chain,
			issue: Some(issueid),
			issue_clause: Some(issueclause),
			lateral: None,
			pos: pos,
		}))
	}
	pub fn missing_rup(pos: FilePosition, num: usize, clause: ClauseContainer, issueid: ClauseIndex, chain: ChainContainer) -> VerificationFailure {
		VerificationFailure::MissingChainRup(Box::<IncorrectChain>::new(IncorrectChain {
			num: num,
			clause: clause,
			chain: chain,
			issue: Some(issueid),
			issue_clause: None,
			lateral: None,
			pos: pos,
		}))
	}
	pub fn unchained_sr(pos: FilePosition, num: usize, clause: ClauseContainer, lat: Option<(ClauseIndex, ClauseContainer)>, witness: WitnessContainer, chain: ChainContainer) -> VerificationFailure {
		VerificationFailure::UnchainedWsr(Box::<IncorrectChain>::new(IncorrectChain {
			num: num,
			clause: clause,
			chain: chain,
			issue: None,
			issue_clause: None,
			lateral: Some(LateralInfo {
				witness: witness,
				lateral: lat,
			}),
			pos: pos,
		}))
	}
	pub fn null_sr(pos: FilePosition, num: usize, clause: ClauseContainer, lat: Option<(ClauseIndex, ClauseContainer)>, witness: WitnessContainer, issueid: ClauseIndex, issueclause: ClauseContainer, chain: ChainContainer) -> VerificationFailure {
		VerificationFailure::NullChainWsr(Box::<IncorrectChain>::new(IncorrectChain {
			num: num,
			clause: clause,
			chain: chain,
			issue: Some(issueid),
			issue_clause: Some(issueclause),
			lateral: Some(LateralInfo {
				witness: witness,
				lateral: lat,
			}),
			pos: pos,
		}))
	}
	pub fn missing_sr(pos: FilePosition, num: usize, clause: ClauseContainer, lat: Option<(ClauseIndex, ClauseContainer)>, witness: WitnessContainer, issueid: ClauseIndex, chain: ChainContainer) -> VerificationFailure {
		VerificationFailure::MissingChainWsr(Box::<IncorrectChain>::new(IncorrectChain {
			num: num,
			clause: clause,
			chain: chain,
			issue: Some(issueid),
			issue_clause: None,
			lateral: Some(LateralInfo {
				witness: witness,
				lateral: lat,
			}),
			pos: pos,
		}))
	}
	pub fn missing_lateral_sr(pos: FilePosition, num: usize, clause: ClauseContainer, latid: ClauseIndex, chain: RawChainContainer) -> VerificationFailure {
		VerificationFailure::MissingLateral(Box::<MissingLateral>::new(MissingLateral {
			num: num,
			clause: clause,
			lateral: latid,
			chain: chain,
			pos: pos,
		}))
	}
	pub fn repeated_lateral_sr(pos: FilePosition, num: usize, clause: ClauseContainer, latid: ClauseIndex, latclause: ClauseContainer, chain1: RawChainContainer, chain2: RawChainContainer) -> VerificationFailure {
		VerificationFailure::RepeatedLateral(Box::<RepeatedLateral>::new(RepeatedLateral {
			num: num,
			clause: clause,
			lateral: (latid, latclause),
			chains: (chain1, chain2),
			pos: pos,
		}))
	}
	pub fn missing_deletion(pos: FilePosition, num: usize, id: ClauseIndex) -> VerificationFailure {
		VerificationFailure::MissingDeletion(Box::<MissingDeletion>::new(MissingDeletion {
			num: num,
			id: id,
			pos: pos,
		}))
	}
	pub fn unrefuted(pos: FilePosition) -> VerificationFailure {
		VerificationFailure::Unrefuted(Box::<FilePosition>::new(pos))
	}
	pub fn failure(&self) -> bool {
		match self {
			VerificationFailure::IncorrectCore(_) |
			VerificationFailure::UnchainedRup(_) |
			VerificationFailure::NullChainRup(_) |
			VerificationFailure::MissingChainRup(_) |
			VerificationFailure::UnchainedWsr(_) |
			VerificationFailure::NullChainWsr(_) |
			VerificationFailure::MissingChainWsr(_) |
			VerificationFailure::Unrefuted(_) => true,
			_ => false,
		}
	}
	pub fn binary(&self, cnfbin: bool, asrbin: bool) -> bool {
		match self {
			VerificationFailure::InputError(_) => false,
			VerificationFailure::ParsingError(bx) => if bx.file_format() == "CNF" {
				cnfbin
			} else if bx.file_format() == "ASR" {
				asrbin
			} else {
				panic!("Unrecognized format")
			},
			VerificationFailure::IncorrectNumVariables(_) |
			VerificationFailure::IncorrectNumClauses(_) |
			VerificationFailure::MissingCnfSection(_) |
			VerificationFailure::DuplicatedCnfSection(_) |
			VerificationFailure::PremiseTautology(_) |
			VerificationFailure::PremiseRepetition(_) => cnfbin,
			VerificationFailure::MissingCoreSection(_) |
			VerificationFailure::DuplicatedCoreSection(_) |
			VerificationFailure::MissingProofSection(_) |
			VerificationFailure::DuplicatedProofSection(_) |
			VerificationFailure::CoreTautology(_) |
			VerificationFailure::InferenceTautology(_) |
			VerificationFailure::CoreRepetition(_) |
			VerificationFailure::InferenceRepetition(_) |
			VerificationFailure::WitnessInconsistency(_) |
			VerificationFailure::WitnessRepetition(_) |
			VerificationFailure::ConflictCoreId(_) |
			VerificationFailure::ConflictInferenceId(_) |
			VerificationFailure::IncorrectCore(_) |
			VerificationFailure::UnchainedRup(_) |
			VerificationFailure::NullChainRup(_) |
			VerificationFailure::MissingChainRup(_) |
			VerificationFailure::UnchainedWsr(_) |
			VerificationFailure::NullChainWsr(_) |
			VerificationFailure::MissingChainWsr(_) |
			VerificationFailure::MissingLateral(_) |
			VerificationFailure::RepeatedLateral(_) |
			VerificationFailure::MissingDeletion(_) |
			VerificationFailure::Unrefuted(_) => asrbin
		}
	}
}
impl From<io::Error> for VerificationFailure {
	fn from(err: io::Error) -> VerificationFailure {
		VerificationFailure::InputError(Box::<io::Error>::new(err))
	}
}
impl From<ParsingError> for VerificationFailure {
	fn from(err: ParsingError) -> VerificationFailure {
		VerificationFailure::ParsingError(Box::<ParsingError>::new(err))
	}
}
impl Display for VerificationFailure {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			VerificationFailure::InputError(bx) => write!(f, "{}", &**bx),
			VerificationFailure::ParsingError(bx) => write!(f, "{}", &**bx),
			VerificationFailure::IncorrectNumVariables(bx) => write!(f, "Incorrect declared number of variables in CNF file {}:\nDeclared {} variables, found {}.", &bx.pos, &bx.expected, &bx.found),
			VerificationFailure::IncorrectNumClauses(bx) => write!(f, "Incorrect declared number of clauses in CNF file {}:\nDeclared {} clauses, found {}.", &bx.pos, &bx.expected, &bx.found),
			VerificationFailure::MissingCnfSection(bx) => write!(f, "Missing section in CNF file {}:\nCould not find section header '{}'.", &bx.pos, &bx.header),
			VerificationFailure::DuplicatedCnfSection(bx) => write!(f, "Duplicated section in CNF file {}:\nFound duplicated section header '{}'.", &bx.pos, &bx.header),
			VerificationFailure::MissingCoreSection(bx) => write!(f, "Missing section in ASR file {}:\nCould not find section header '{}'.", &bx.pos, &bx.header),
			VerificationFailure::DuplicatedCoreSection(bx) => write!(f, "Duplicated section in ASR file {}:\nFound duplicated section header '{}'.", &bx.pos, &bx.header),
			VerificationFailure::MissingProofSection(bx) => write!(f, "Missing section in ASR file {}:\nCould not find section header '{}'.", &bx.pos, &bx.header),
			VerificationFailure::DuplicatedProofSection(bx) => write!(f, "Duplicated section in ASR file {}:\nFound duplicated section header '{}'.", &bx.pos, &bx.header),
			VerificationFailure::PremiseTautology(bx) => write!(f, "Tautological premise clause found in CNF file {}:\nClause {} on formula entry {} contains complementary literals {} and {}.", &bx.pos, &bx.clause, &bx.num, &bx.issue, bx.issue.complement()),
			VerificationFailure::CoreTautology(bx) => write!(f, "Tautological core clause found in ASR file {}:\nClause {} on core entry {} contains complementary literals {} and {}.", &bx.pos, &bx.clause, &bx.num, &bx.issue, bx.issue.complement()),
			VerificationFailure::InferenceTautology(bx) => write!(f, "Tautological inference clause found in ASR file {}:\nClause {} on proof instruction {} contains complementary literals {} and {}.", &bx.pos, &bx.clause, &bx.num, &bx.issue, bx.issue.complement()),
			VerificationFailure::PremiseRepetition(bx) => write!(f, "Repeated literal found in premise clause in CNF file {}:\nClause {} on formula entry {} contains a repeated literal {}.", &bx.pos, &bx.clause, &bx.num, &bx.issue),
			VerificationFailure::CoreRepetition(bx) => write!(f, "Repeated literal found in core clause in ASR file {}:\nClause {} on core entry {} contains a repeated literal {}.", &bx.pos, &bx.clause, &bx.num, &bx.issue),
			VerificationFailure::InferenceRepetition(bx) => write!(f, "Repeated literal found in inference clause in ASR file {}:\nClause {} on proof instruction {} contains a repeated literal {}.", &bx.pos, &bx.clause, &bx.num, &bx.issue),
			VerificationFailure::WitnessInconsistency(bx) => write!(f, "Inconsistent witness found in ASR file: {}:\nWitness {} in the SR inference on proof instruction {} contains inconsistent mappings.", &bx.pos, &bx.witness, &bx.num),
			VerificationFailure::WitnessRepetition(bx) => write!(f, "Redundant witness mapping found in ASR file: {}:\nWitness {} in the SR inference on proof instruction {} contains redundant mappings.", &bx.pos, &bx.witness, &bx.num),
			VerificationFailure::ConflictCoreId(bx) => write!(f, "Conflicting core clause identifier found in ASR file: {}:\nCore entry {} has identifier {}, but that identifier is already in use for clause {}.", &bx.pos, &bx.num, &bx.id, &bx.clause),
			VerificationFailure::ConflictInferenceId(bx) => write!(f, "Conflicting inferred clause identifier found in ASR file: {}:\nProof instruction {} has identifier {}, but that identifier is already in use for clause {}.", &bx.pos, &bx.num, &bx.id, &bx.clause),
			VerificationFailure::IncorrectCore(bx) => write!(f, "Incorrect core entry in ASR file {}:\nCore entry {} introduces clause {}, but this clause does not occur in the CNF formula.", &bx.pos, &bx.num, &bx.clause),
			VerificationFailure::UnchainedRup(bx) => write!(f, "Incorrect RUP inference in ASR file {}:\nProof instruction {} introduces clause {} as a RUP inference through the unit propagation chain:\n{}but propagation does not produce a contradiction.", &bx.pos, &bx.num, &bx.clause, &bx.chain),
			VerificationFailure::NullChainRup(bx) => write!(f, "Invalid RUP inference in ASR file {}:\nProof instruction {} introduces clause {} as a RUP inference through the unit propagation chain:\n{}The next propagation clause is {}: {}, but this clause does not produce a propagation nor a contradiction.", &bx.pos, &bx.num, &bx.clause, &bx.chain, &bx.issue.as_ref().unwrap(), &bx.issue_clause.as_ref().unwrap()),
			VerificationFailure::MissingChainRup(bx) => write!(f, "Invalid RUP inference in ASR file {}:\nProof instruction {} introduces clause {} as a RUP inference through the unit propagation chain:\n{}The next propagation clause identifier is {}, but no clause is associated to this identifier.", &bx.pos, &bx.num, &bx.clause, &bx.chain, &bx.issue.as_ref().unwrap()),
			VerificationFailure::UnchainedWsr(bx) => {
				write!(f, "Incorrect SR inference in ASR file {}:\nProof instruction {} introduces clause {} as an SR inference upon the witness mapping {}. The unit propagation chain:\n{} is given for", &bx.pos, &bx.num, &bx.clause, &bx.lateral.as_ref().unwrap().witness, &bx.chain)?;
				match &bx.lateral.as_ref().unwrap().lateral {
					Some((latid, latcls)) => write!(f, " clause {}: {}", latid, latcls)?,
					None => write!(f, " the clause itself")?,
				}
				write!(f, ", but propagation does not produce a contradiction.")
			},
			VerificationFailure::NullChainWsr(bx) => {
				write!(f, "Invalid SR inference in ASR file {}:\nProof instruction {} introduces clause {} as an SR inference upon the witness mapping {}. The unit propagation chain:\n{} is given for", &bx.pos, &bx.num, &bx.clause, &bx.lateral.as_ref().unwrap().witness, &bx.chain)?;
				match &bx.lateral.as_ref().unwrap().lateral {
					Some((latid, latcls)) => write!(f, " clause {}: {}", latid, latcls)?,
					None => write!(f, " the clause itself")?,
				}
				write!(f, ". The next propagation clause is {}: {}, but this clause does not produce a propagation nor a contradiction.", &bx.issue.as_ref().unwrap(), &bx.issue_clause.as_ref().unwrap())
			},
			VerificationFailure::MissingChainWsr(bx) => {
				write!(f, "Invalid SR inference in ASR file {}:\nProof instruction {} introduces clause {} as an SR inference upon the witness mapping {}. The unit propagation chain:\n{} is given for", &bx.pos, &bx.num, &bx.clause, &bx.lateral.as_ref().unwrap().witness, &bx.chain)?;
				match &bx.lateral.as_ref().unwrap().lateral {
					Some((latid, latcls)) => write!(f, " clause {}: {}", latid, latcls)?,
					None => write!(f, " the clause itself")?,
				}
				write!(f, ". The next propagation clause identifier is {} but no clause is associated to this identifier.", &bx.issue.as_ref().unwrap())
			},
			VerificationFailure::MissingLateral(bx) => write!(f, "Invalid SR inference in ASR file {}:\nProof instruction {} introduces clause {} as an SR inference. The subchain {} is given for clause identifier {}, but no clause is associated to this identifier.", &bx.pos, &bx.num, &bx.clause, &bx.chain, &bx.lateral),
			VerificationFailure::RepeatedLateral(bx) => write!(f, "Invalid SR inference in ASR file {}:\nProof instruction {} introduces clause {} as an SR inference. Two subchains {} and {} are given for the same clause identifier {}: {}.", &bx.pos, &bx.num, &bx.clause, &bx.chains.0, &bx.chains.1, &bx.lateral.0, &bx.lateral.1),
			VerificationFailure::MissingDeletion(bx) => write!(f, "Invalid deletion instruction in ASR file {}:\nProof instruction {} deletes the clause with identifier {}, but no clause is associated to this identifier.", &bx.pos, &bx.num, &bx.id),
			VerificationFailure::Unrefuted(bx) => write!(f, "Invalid proof in ASR file {}:\nThe proof does not derive the empty clause at any point.", &**bx),
		}
	}
}
impl Binary for VerificationFailure {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			VerificationFailure::InputError(bx) => write!(f, "{}", &**bx),
			VerificationFailure::ParsingError(bx) => write!(f, "{:b}", &**bx),
			VerificationFailure::IncorrectNumVariables(bx) => write!(f, "Incorrect declared number of variables in CNF file {:b}:\nDeclared {} variables, found {}.", &bx.pos, &bx.expected, &bx.found),
			VerificationFailure::IncorrectNumClauses(bx) => write!(f, "Incorrect declared number of clauses in CNF file {:b}:\nDeclared {} clauses, found {}.", &bx.pos, &bx.expected, &bx.found),
			VerificationFailure::MissingCnfSection(bx) => write!(f, "Missing section in CNF file {:b}:\nCould not find section header '{}'.", &bx.pos, &bx.header),
			VerificationFailure::DuplicatedCnfSection(bx) => write!(f, "Duplicated section in CNF file {:b}:\nFound duplicated section header '{}'.", &bx.pos, &bx.header),
			VerificationFailure::MissingCoreSection(bx) => write!(f, "Missing section in ASR file {:b}:\nCould not find section header '{}'.", &bx.pos, &bx.header),
			VerificationFailure::DuplicatedCoreSection(bx) => write!(f, "Duplicated section in ASR file {:b}:\nFound duplicated section header '{}'.", &bx.pos, &bx.header),
			VerificationFailure::MissingProofSection(bx) => write!(f, "Missing section in ASR file {:b}:\nCould not find section header '{}'.", &bx.pos, &bx.header),
			VerificationFailure::DuplicatedProofSection(bx) => write!(f, "Duplicated section in ASR file {:b}:\nFound duplicated section header '{}'.", &bx.pos, &bx.header),
			VerificationFailure::PremiseTautology(bx) => write!(f, "Tautological premise clause found in CNF file {:b}:\nClause {} on formula entry {} contains complementary literals {} and {}.", &bx.pos, &bx.clause, &bx.num, &bx.issue, bx.issue.complement()),
			VerificationFailure::CoreTautology(bx) => write!(f, "Tautological core clause found in ASR file {:b}:\nClause {} on core entry {} contains complementary literals {} and {}.", &bx.pos, &bx.clause, &bx.num, &bx.issue, bx.issue.complement()),
			VerificationFailure::InferenceTautology(bx) => write!(f, "Tautological inference clause found in ASR file {:b}:\nClause {} on proof instruction {} contains complementary literals {} and {}.", &bx.pos, &bx.clause, &bx.num, &bx.issue, bx.issue.complement()),
			VerificationFailure::PremiseRepetition(bx) => write!(f, "Repeated literal found in premise clause in CNF file {:b}:\nClause {} on formula entry {} contains a repeated literal {}.", &bx.pos, &bx.clause, &bx.num, &bx.issue),
			VerificationFailure::CoreRepetition(bx) => write!(f, "Repeated literal found in core clause in ASR file {:b}:\nClause {} on core entry {} contains a repeated literal {}.", &bx.pos, &bx.clause, &bx.num, &bx.issue),
			VerificationFailure::InferenceRepetition(bx) => write!(f, "Repeated literal found in inference clause in ASR file {:b}:\nClause {} on proof instruction {} contains a repeated literal {}.", &bx.pos, &bx.clause, &bx.num, &bx.issue),
			VerificationFailure::WitnessInconsistency(bx) => write!(f, "Inconsistent witness found in ASR file: {}:\nWitness {} in the SR inference on proof instruction {} contains inconsistent mappings.", &bx.pos, &bx.witness, &bx.num),
			VerificationFailure::WitnessRepetition(bx) => write!(f, "Redundant witness mapping found in ASR file: {}:\nWitness {} in the SR inference on proof instruction {} contains redundant mappings.", &bx.pos, &bx.witness, &bx.num),
			VerificationFailure::ConflictCoreId(bx) => write!(f, "Conflicting core clause identifier found in ASR file: {}:\nCore entry {} has identifier {}, but that identifier is already in use for clause {}.", &bx.pos, &bx.num, &bx.id, &bx.clause),
			VerificationFailure::ConflictInferenceId(bx) => write!(f, "Conflicting inferred clause identifier found in ASR file: {}:\nProof instruction {} has identifier {}, but that identifier is already in use for clause {}.", &bx.pos, &bx.num, &bx.id, &bx.clause),
			VerificationFailure::IncorrectCore(bx) => write!(f, "Incorrect core entry in ASR file {:b}:\nCore entry {} introduces clause {}, but this clause does not occur in the CNF formula.", &bx.pos, &bx.num, &bx.clause),
			VerificationFailure::UnchainedRup(bx) => write!(f, "Incorrect RUP inference in ASR file {:b}:\nProof instruction {} introduces clause {} as a RUP inference through the unit propagation chain:\n{}but propagation does not produce a contradiction.", &bx.pos, &bx.num, &bx.clause, &bx.chain),
			VerificationFailure::NullChainRup(bx) => write!(f, "Invalid RUP inference in ASR file {:b}:\nProof instruction {} introduces clause {} as a RUP inference through the unit propagation chain:\n{}The next propagation clause is {}: {}, but this clause does not produce a propagation nor a contradiction.", &bx.pos, &bx.num, &bx.clause, &bx.chain, &bx.issue.as_ref().unwrap(), &bx.issue_clause.as_ref().unwrap()),
			VerificationFailure::MissingChainRup(bx) => write!(f, "Invalid RUP inference in ASR file {:b}:\nProof instruction {} introduces clause {} as a RUP inference through the unit propagation chain:\n{}The next propagation clause identifier is {}, but no clause is associated to this identifier.", &bx.pos, &bx.num, &bx.clause, &bx.chain, &bx.issue.as_ref().unwrap()),
			VerificationFailure::UnchainedWsr(bx) => {
				write!(f, "Incorrect SR inference in ASR file {:b}:\nProof instruction {} introduces clause {} as an SR inference upon the witness mapping {}. The unit propagation chain:\n{} is given for", &bx.pos, &bx.num, &bx.clause, &bx.lateral.as_ref().unwrap().witness, &bx.chain)?;
				match &bx.lateral.as_ref().unwrap().lateral {
					Some((latid, latcls)) => write!(f, " clause {}: {}", latid, latcls)?,
					None => write!(f, " the clause itself")?,
				}
				write!(f, ", but propagation does not produce a contradiction.")
			},
			VerificationFailure::NullChainWsr(bx) => {
				write!(f, "Invalid SR inference in ASR file {:b}:\nProof instruction {} introduces clause {} as an SR inference upon the witness mapping {}. The unit propagation chain:\n{} is given for", &bx.pos, &bx.num, &bx.clause, &bx.lateral.as_ref().unwrap().witness, &bx.chain)?;
				match &bx.lateral.as_ref().unwrap().lateral {
					Some((latid, latcls)) => write!(f, " clause {}: {}", latid, latcls)?,
					None => write!(f, " the clause itself")?,
				}
				write!(f, ". The next propagation clause is {}: {}, but this clause does not produce a propagation nor a contradiction.", &bx.issue.as_ref().unwrap(), &bx.issue_clause.as_ref().unwrap())
			},
			VerificationFailure::MissingChainWsr(bx) => {
				write!(f, "Invalid SR inference in ASR file {:b}:\nProof instruction {} introduces clause {} as an SR inference upon the witness mapping {}. The unit propagation chain:\n{} is given for", &bx.pos, &bx.num, &bx.clause, &bx.lateral.as_ref().unwrap().witness, &bx.chain)?;
				match &bx.lateral.as_ref().unwrap().lateral {
					Some((latid, latcls)) => write!(f, " clause {}: {}", latid, latcls)?,
					None => write!(f, " the clause itself")?,
				}
				write!(f, ". The next propagation clause identifier is {} but no clause is associated to this identifier.", &bx.issue.as_ref().unwrap())
			},
			VerificationFailure::MissingLateral(bx) => write!(f, "Invalid SR inference in ASR file {:b}:\nProof instruction {} introduces clause {} as an SR inference. The subchain {} is given for clause identifier {}, but no clause is associated to this identifier.", &bx.pos, &bx.num, &bx.clause, &bx.chain, &bx.lateral),
			VerificationFailure::RepeatedLateral(bx) => write!(f, "Invalid SR inference in ASR file {:b}:\nProof instruction {} introduces clause {} as an SR inference. Two subchains {} and {} are given for the same clause identifier {}: {}.", &bx.pos, &bx.num, &bx.clause, &bx.chains.0, &bx.chains.1, &bx.lateral.0, &bx.lateral.1),
			VerificationFailure::MissingDeletion(bx) => write!(f, "Invalid deletion instruction in ASR file {:b}:\nProof instruction {} deletes the clause with identifier {}, but no clause is associated to this identifier.", &bx.pos, &bx.num, &bx.id),
			VerificationFailure::Unrefuted(bx) => write!(f, "Invalid proof in ASR file {:b}:\nThe proof does not derive the empty clause at any point.", &**bx),
		}
	}
}
impl Error for VerificationFailure {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
		match self {
			VerificationFailure::InputError(bx) => Some(bx),
			VerificationFailure::ParsingError(bx) => Some(bx),
			_ => None,
		}
    }
}

pub type VerificationResult<T> = Result<T, VerificationFailure>;