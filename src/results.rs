use crate::{
	clausedb::{ClauseDb, ClauseIndex},
	input::{FilePosition, Positionable},
	parser::{ParsingError, CnfParser, AsrParser},
	variable::{Variable, Literal},
};

pub struct Verified {
	checked_core: bool,
	checked_proof: bool,
}

pub struct UnchainedRup {
	num_inference: usize,
	clause: Vec<Literal>,
	chain: Vec<ClauseIndex>,
}

pub struct FalsifiableRed {
	num_inference: usize,
	clause: Vec<Literal>,
	witness: Vec<(Variable, Literal)>,
	reverse: bool,
}

pub struct MissingRed {
	num_inference: usize,
	clause: Vec<Literal>,
	witness: Vec<(Variable, Literal)>,
	resolvent_id: ClauseIndex,
	resolvent_clause: Vec<Literal>,
}

pub struct UnchainedRed {
	num_inference: usize,
	clause: Vec<Literal>,
	witness: Vec<(Variable, Literal)>,
	resolvent_id: ClauseIndex,
	resolvent_clause: Vec<Literal>,
	resolvent_chain: Vec<ClauseIndex>,
}

pub struct InvalidClause {
	num: usize,
	clause: Vec<Literal>,
	pos: FilePosition,
	issue: Literal,
}

pub struct InvalidWitness {
	num: usize,
	witness: Vec<(Variable, Literal)>,
	pos: FilePosition,
}

pub struct EmptyId {
	num: usize,
	id: ClauseIndex,
	pos: FilePosition,
}

pub struct ConflictId {
	num: usize,
	id: ClauseIndex,
	clause: Vec<Literal>,
	pos: FilePosition,
}

pub struct WrongSection {
	header: String,
	pos: FilePosition,
}

pub enum VerificationFailure {
	ParsingError(Box<ParsingError>),
	MissingCnfSection(Box<WrongSection>),
	DuplicatedCnfSection(Box<WrongSection>),
	MissingCoreSection(Box<WrongSection>),
	DuplicatedCoreSection(Box<WrongSection>),
	MissingProofSection(Box<WrongSection>),
	DuplicatedProofSection(Box<WrongSection>),
	InvalidSection(Box<WrongSection>),
	PremiseTautology(Box<InvalidClause>),
	CoreTautology(Box<InvalidClause>),
	InferenceTautology(Box<InvalidClause>),
	PremiseRepetition(Box<InvalidClause>),
	CoreRepetition(Box<InvalidClause>),
	InferenceRepetition(Box<InvalidClause>),
	WitnessInconsistency(Box<InvalidWitness>),
	WitnessRepetition(Box<InvalidWitness>),
	EmptyId(Box<EmptyId>),
	ConflictCoreId(Box<ConflictId>),
	ConflictInferenceId(Box<ConflictId>),
	IncorrectCore(Box<ConflictId>),
	UnchainedRup(Box<UnchainedRup>),
	FalsifiableSr(Box<FalsifiableRed>),
	FalsifiableXr(Box<FalsifiableRed>),
	MissingSr(Box<MissingRed>),
	MissingXr(Box<MissingRed>),
	UnchainedSr(Box<UnchainedRed>),
	UnchainedXr(Box<UnchainedRed>),
	Unrefuted,
}
impl VerificationFailure {
	pub fn missing_cnf_section<P: Positionable + ?Sized>(input: &P) -> VerificationFailure {
		VerificationFailure::MissingCnfSection(Box::<WrongSection>::new(WrongSection {
			pos: input.position().clone(),
			header: "cnf".to_string(),
		}))
	}
	pub fn duplicated_cnf_section<P: Positionable + ?Sized>(input: &P) -> VerificationFailure {
		VerificationFailure::DuplicatedCnfSection(Box::<WrongSection>::new(WrongSection {
			pos: input.position().clone(),
			header: "cnf".to_string(),
		}))
	}
	pub fn missing_core_section<P: Positionable + ?Sized>(input: &P) -> VerificationFailure {
		VerificationFailure::MissingCoreSection(Box::<WrongSection>::new(WrongSection {
			pos: input.position().clone(),
			header: "asrcore".to_string(),
		}))
	}
	pub fn duplicated_core_section<P: Positionable + ?Sized>(input: &P) -> VerificationFailure {
		VerificationFailure::DuplicatedCoreSection(Box::<WrongSection>::new(WrongSection {
			pos: input.position().clone(),
			header: "asrcore".to_string(),
		}))
	}
	pub fn missing_proof_section<P: Positionable + ?Sized>(input: &P) -> VerificationFailure {
		VerificationFailure::MissingProofSection(Box::<WrongSection>::new(WrongSection {
			pos: input.position().clone(),
			header: "asrproof".to_string(),
		}))
	}
	pub fn duplicated_proof_section<P: Positionable + ?Sized>(input: &P) -> VerificationFailure {
		VerificationFailure::DuplicatedProofSection(Box::<WrongSection>::new(WrongSection {
			pos: input.position().clone(),
			header: "asrproof".to_string(),
		}))
	}
	pub fn invalid_section<P: Positionable + ?Sized>(input: &P, hd: [u8; 8]) -> VerificationFailure {
		VerificationFailure::InvalidSection(Box::<WrongSection>::new(WrongSection {
			pos: input.position().clone(),
			header: VerificationFailure::header_to_string(&hd),
		}))
	}
	pub fn premise_repetition<P: Positionable + ?Sized>(input: &P, num: usize, clause: Vec<Literal>, issue: Literal) -> VerificationFailure {
		VerificationFailure::PremiseRepetition(Box::<InvalidClause>::new(InvalidClause {
			num: num,
			clause: clause,
			pos: input.position().clone(),
			issue: issue,
		}))
	}
	pub fn premise_tautology<P: Positionable + ?Sized>(input: &P, num: usize, clause: Vec<Literal>, issue: Literal) -> VerificationFailure {
		VerificationFailure::PremiseTautology(Box::<InvalidClause>::new(InvalidClause {
			num: num,
			clause: clause,
			pos: input.position().clone(),
			issue: issue,
		}))
	}
	pub fn core_repetition<P: Positionable + ?Sized>(input: &P, num: usize, clause: Vec<Literal>, issue: Literal) -> VerificationFailure {
		VerificationFailure::CoreRepetition(Box::<InvalidClause>::new(InvalidClause {
			num: num,
			clause: clause,
			pos: input.position().clone(),
			issue: issue,
		}))
	}
	pub fn core_tautology<P: Positionable + ?Sized>(input: &P, num: usize, clause: Vec<Literal>, issue: Literal) -> VerificationFailure {
		VerificationFailure::CoreTautology(Box::<InvalidClause>::new(InvalidClause {
			num: num,
			clause: clause,
			pos: input.position().clone(),
			issue: issue,
		}))
	}
	pub fn conflict_core_id<P: Positionable + ?Sized>(input: &P, num: usize, id: ClauseIndex, clause: Vec<Literal>) -> VerificationFailure {
		VerificationFailure::ConflictCoreId(Box::<ConflictId>::new(ConflictId {
			num: num,
			id: id,
			clause: clause,
			pos: input.position().clone(),
		}))
	}
	pub fn incorrect_core<P: Positionable + ?Sized>(input: &P, num: usize, id: ClauseIndex, clause: Vec<Literal>) -> VerificationFailure {
		VerificationFailure::IncorrectCore(Box::<ConflictId>::new(ConflictId {
			num: num,
			id: id,
			clause: clause,
			pos: input.position().clone(),
		}))
	}
	fn header_to_string(hd: &[u8]) -> String {
		let mut s = String::new();
		for c in hd {
			if c == &0u8 {
				break;
			} else {
				s.push(*c as char);
			}
		}
		s
	}
}
impl From<ParsingError> for VerificationFailure {
	fn from(err: ParsingError) -> VerificationFailure {
		VerificationFailure::ParsingError(Box::<ParsingError>::new(err))
	}
}

pub type VerificationResult<T> = Result<T, VerificationFailure>;