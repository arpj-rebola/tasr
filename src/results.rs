use std::{
	error::{Error},
	fmt::{self, Display, Formatter},
};

use colored::{
	Colorize,
};

use crate::{
	basic::{ClauseIndex, ClauseContainer, WitnessContainer},
	clausedb::{ClauseDb},
	chaindb::{ChainDb},
	assignment::{Substitution},
	input::{SourcePosition},
	variable::{Literal, MaybeVariable, Variable},
	logger::{LoggerHandle, LoggerLevel},
	unitpropagation::{PropagationResult},
};

#[derive(Debug)]
pub struct IncorrectCnfStats<T: Display> {
	found: T,
	expected: T,
	pos: SourcePosition,
}

#[derive(Debug)]
pub enum SectionError {
	Missing,
	Misplaced,
	Duplicated,
}
impl Display for SectionError {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match self {
			SectionError::Missing => write!(f, "missing"),
			SectionError::Misplaced => write!(f, "misplaced"),
			SectionError::Duplicated => write!(f, "duplicated"),
		}
	}
}

/// Error structure for `VerificationFailure::MissingCnfSection`, `VerificationFailure::MissingCoreSection`, `VerificationFailure::MissingProofSection`, `VerificationFailure::DuplicatedCnfSection`, `VerificationFailure::DuplicatedCoreSection` and `VerificationFailure::DuplicatedProofSection`.
#[derive(Debug)]
pub struct WrongSection {
	/// Missing or duplicated header string.
	header: String,
	/// Position at which a duplicated header is found, or a header is concluded to be missing.
	format: String,
	pos: SourcePosition,
	issue: SectionError,
}

#[derive(Debug)]
pub struct ClauseIssues {
	clause: ClauseContainer,
	issues: Vec<(Literal, bool)>,
}
impl ClauseIssues {
	pub fn serious(&self) -> bool {
		self.issues.iter().all(|(_, rep)| *rep)
	}
	pub fn is_ok(&self, permissive: bool) -> bool {
		permissive && !self.serious()
	}
}

#[derive(Debug)]
pub struct ClauseIssuesBuilder(Option<Box<ClauseIssues>>);
impl ClauseIssuesBuilder {
	pub fn new() -> ClauseIssuesBuilder {
		ClauseIssuesBuilder(None)
	}
	pub fn set<F>(&mut self, init: F) where
		F: FnOnce() -> ClauseContainer
	{
		if self.0.is_none() {
			self.0 = Some(Box::new(ClauseIssues {
				clause: init(),
				issues: Vec::new(),
			}));
		}
	}
	pub fn push_literal(&mut self, lit: Literal) {
		match &mut self.0 {
			None => (),
			Some(bx) => bx.clause.0.push(lit),
		}
	}
	pub fn push_issue(&mut self, lit: Literal, rep: bool) {
		match &mut self.0 {
			None => (),
			Some(bx) => bx.issues.push((lit, rep)),
		}
	}
	pub fn is_ok(&self, permissive: bool) -> bool {
		match &self.0 {
			None => true,
			Some(bx) => bx.is_ok(permissive),
		}
	}
	pub fn extract(self) -> Option<Box<ClauseIssues>> {
		self.0
	}
}

#[derive(Debug)]
pub struct InvalidClause {
	num: (usize, String),
	pos: SourcePosition,
	kind: String,
	format: String,
	clause: ClauseContainer,
	issues: Vec<(Literal, bool)>,
}

#[derive(Debug)]
pub struct WitnessIssues {
	witness: WitnessContainer,
	issues: Vec<(Variable, Literal, bool)>,
}
impl WitnessIssues {
	pub fn serious(&self) -> bool {
		self.issues.iter().all(|(_, _, rep)| *rep)
	}
	pub fn is_ok(&self, permissive: bool) -> bool {
		permissive && !self.serious()
	}
}

#[derive(Debug)]
pub struct WitnessIssuesBuilder(Option<Box<WitnessIssues>>);
impl WitnessIssuesBuilder {
	pub fn new() -> WitnessIssuesBuilder {
		WitnessIssuesBuilder(None)
	}
	pub fn set<F>(&mut self, init: F) where
		F: FnOnce() -> WitnessContainer
	{
		if self.0.is_none() {
			self.0 = Some(Box::new(WitnessIssues {
				witness: init(),
				issues: Vec::new(),
			}));
		}
	}
	pub fn push_mapping(&mut self, var: Variable, lit: Literal) {
		match &mut self.0 {
			None => (),
			Some(bx) => bx.witness.0.push((var, lit)),
		}
	}
	pub fn push_issue(&mut self, var: Variable, lit: Literal, rep: bool) {
		match &mut self.0 {
			None => (),
			Some(bx) => bx.issues.push((var, lit, rep)),
		}
	}
	pub fn extract(self) -> Option<Box<WitnessIssues>> {
		self.0
	}
}

#[derive(Debug)]
pub struct InvalidWitness {
	num: usize,
	pos: SourcePosition,
	witness: WitnessContainer,
	issues: Vec<(Variable, Literal, bool)>,
	format: String,
}

pub struct ChainIssuesBuilder(Option<Box<Vec<(ClauseIndex, bool)>>>);
impl ChainIssuesBuilder {
	pub fn new() -> ChainIssuesBuilder {
		ChainIssuesBuilder(None)
	}
	pub fn push_issue(&mut self, lat: ClauseIndex, id: ClauseIndex, rep: bool) {
		let repp = rep && (lat != id);
		match &mut self.0 {
			None => self.0 = Some(Box::new(vec![(lat, repp)])),
			Some(bx) => bx.push((lat, repp)),
		}
	}
	pub fn extract(self) -> Option<Box<Vec<(ClauseIndex, bool)>>> {
		self.0
	}
}

#[derive(Debug)]
pub struct PropagationIssues {
	chain: Vec<(ClauseContainer, ClauseIndex, Option<PropagationResult>)>,
	conflict: bool,
}
impl PropagationIssues {
	fn new(id: ClauseIndex, prop: Vec<PropagationResult>, db: &ClauseDb, chain: &ChainDb) -> Option<PropagationIssues> {
		let chn = chain.retrieve(id)?;
		let mut outchain = Vec::new();
		for cid in chn {
			outchain.push((db.extract(*cid)?, *cid, None));
		}
		let mut conflict = true;
		for issue in prop {
			match issue.index() {
				None => conflict = false,
				Some(n) => outchain.get_mut(n).as_mut().unwrap().2 = Some(issue),
			}
		}
		Some(PropagationIssues {
			chain: outchain,
			conflict: conflict,
		})
	}
	fn serious(&self) -> bool {
		!self.conflict || self.chain.iter().any(|(_, _, resopt)| match resopt {
			Some(PropagationResult::Stable) | Some(PropagationResult::Missing(_)) => true,
			_ => false,
		})
	}
}

#[derive(Debug)]
pub struct PropagationIssuesBuilder(Option<Box<Vec<PropagationResult>>>, Option<Box<Vec<(ClauseIndex, Vec<PropagationResult>)>>>);
impl PropagationIssuesBuilder {
	pub fn new() -> PropagationIssuesBuilder {
		PropagationIssuesBuilder(None, None)
	}
	pub fn push_issue(&mut self, res: PropagationResult) {
		if self.0.is_none() {
			self.0 = Some(Box::new(Vec::new()));
		}
		self.0.as_mut().unwrap().push(res);
	}
	pub fn push_chain(&mut self, lat: ClauseIndex) {
		let tk = self.0.take();
		match tk {
			None => (),
			Some(bx) => {
				if self.1.is_none() {
					self.1 = Some(Box::new(Vec::new()));
				}
				self.1.as_mut().unwrap().push((lat, *bx));
			}
		}
	}
	pub fn extract(&mut self, db: &ClauseDb, chain: &ChainDb, subst: &Substitution) -> Option<Box<Vec<(ClauseIndex, PropagationIssues, WitnessContainer)>>> {
		let tk = self.1.take();
		match tk {
			None => None,
			Some(mut bx) => {
				let mut out = Box::new(Vec::new());
				for (lat, prop) in bx.drain(..) {
					out.push((lat, PropagationIssues::new(lat, prop, db, chain).unwrap(), subst.extract()))
				}
				Some(out)
			}
		}
	}
}

#[derive(Debug)]
pub struct IncorrectChain {
	num: usize,
	clause: ClauseContainer,
	chain: PropagationIssues,
	latid: Option<ClauseIndex>,
	latclause: ClauseContainer,
	witness: WitnessContainer,
	pos: SourcePosition,
	format: String,
}

#[derive(Debug)]
pub struct ConflictId {
	num: (usize, String),
	id: ClauseIndex,
	clause: ClauseContainer,
	pos: SourcePosition,
	format: String,
	kind: String,
}

#[derive(Debug)]
pub struct InvalidLaterals {
	num: usize,
	format: String,
	clause: ClauseContainer,
	laterals: Vec<(ClauseIndex, bool)>,
	pos: SourcePosition,
}

#[derive(Debug)]
pub struct MissingDeletion {
	num: usize,
	format: String,
	id: ClauseIndex,
	pos: SourcePosition,
}

#[derive(Debug)]
pub struct RefutationError {
	num: usize,
	format: String,
	pos: SourcePosition,
}

#[derive(Debug)]
pub enum VerificationFailure {
	IncorrectNumVariables(Box<IncorrectCnfStats<MaybeVariable>>),
	IncorrectNumClauses(Box<IncorrectCnfStats<usize>>),
	WrongSection(Box<WrongSection>),
	InvalidClause(Box<InvalidClause>),
	UncompliantClause(Box<InvalidClause>),
	InvalidWitness(Box<InvalidWitness>),
	UncompliantWitness(Box<InvalidWitness>),
	ConflictId(Box<ConflictId>),
	IncorrectCore(Box<ConflictId>),
	IncorrectRup(Box<IncorrectChain>),
	IncorrectWsr(Box<IncorrectChain>),
	UncompliantRup(Box<IncorrectChain>),
	UncompliantWsr(Box<IncorrectChain>),
	InvalidLaterals(Box<InvalidLaterals>),
	MissingDeletion(Box<MissingDeletion>),
	Unrefuted(Box<RefutationError>),
}
impl VerificationFailure {
	pub fn incorrect_num_variables(pos: SourcePosition, found: MaybeVariable, expected: MaybeVariable) -> VerificationFailure {
		VerificationFailure::IncorrectNumVariables(Box::<IncorrectCnfStats<MaybeVariable>>::new(IncorrectCnfStats::<MaybeVariable> {
			found: found,
			expected: expected,
			pos: pos,
		}))
	}
	pub fn incorrect_num_clauses(pos: SourcePosition, found: usize, expected: usize) -> VerificationFailure {
		VerificationFailure::IncorrectNumClauses(Box::<IncorrectCnfStats<usize>>::new(IncorrectCnfStats::<usize> {
			found: found,
			expected: expected,
			pos: pos,
		}))
	}
	pub fn missing_cnf_section(pos: SourcePosition) -> VerificationFailure {
		VerificationFailure::WrongSection(Box::<WrongSection>::new(WrongSection {
			pos: pos,
			header: "cnf".to_string(),
			format: "CNF".to_string(),
			issue: SectionError::Missing,
		}))
	}
	pub fn duplicated_cnf_section(pos: SourcePosition) -> VerificationFailure {
		VerificationFailure::WrongSection(Box::<WrongSection>::new(WrongSection {
			pos: pos,
			header: "cnf".to_string(),
			format: "CNF".to_string(),
			issue: SectionError::Duplicated,
		}))
	}
	pub fn missing_core_section(pos: SourcePosition) -> VerificationFailure {
		VerificationFailure::WrongSection(Box::<WrongSection>::new(WrongSection {
			pos: pos,
			header: "core".to_string(),
			format: "ASR".to_string(),
			issue: SectionError::Missing,
		}))
	}
	pub fn duplicated_core_section(pos: SourcePosition) -> VerificationFailure {
		VerificationFailure::WrongSection(Box::<WrongSection>::new(WrongSection {
			pos: pos,
			header: "core".to_string(),
			format: "ASR".to_string(),
			issue: SectionError::Duplicated,
		}))
	}
	pub fn misplaced_core_section(pos: SourcePosition) -> VerificationFailure {
		VerificationFailure::WrongSection(Box::<WrongSection>::new(WrongSection {
			pos: pos,
			header: "core".to_string(),
			format: "ASR".to_string(),
			issue: SectionError::Misplaced,
		}))
	}
	pub fn missing_proof_section(pos: SourcePosition) -> VerificationFailure {
		VerificationFailure::WrongSection(Box::<WrongSection>::new(WrongSection {
			pos: pos,
			header: "proof".to_string(),
			format: "ASR".to_string(),
			issue: SectionError::Missing,
		}))
	}
	pub fn duplicated_proof_section(pos: SourcePosition) -> VerificationFailure {
		VerificationFailure::WrongSection(Box::<WrongSection>::new(WrongSection {
			pos: pos,
			header: "proof".to_string(),
			format: "ASR".to_string(),
			issue: SectionError::Duplicated,
		}))
	}
	pub fn misplaced_proof_section(pos: SourcePosition) -> VerificationFailure {
		VerificationFailure::WrongSection(Box::<WrongSection>::new(WrongSection {
			pos: pos,
			header: "proof".to_string(),
			format: "ASR".to_string(),
			issue: SectionError::Misplaced,
		}))
	}
	fn invalid_clause(pos: SourcePosition, num: usize, issues: Box<ClauseIssues>, format: &str, kind: &str, numbering: &str) -> VerificationFailure {
		let serious = issues.serious();
		let bx = Box::<InvalidClause>::new(InvalidClause {
			num: (num, numbering.to_string()),
			pos: pos,
			kind: kind.to_string(),
			format: format.to_string(),
			clause: issues.clause,
			issues: issues.issues,
		});
		if serious {
			VerificationFailure::InvalidClause(bx)
		} else {
			VerificationFailure::UncompliantClause(bx)
		}
	}
	pub fn invalid_premise(pos: SourcePosition, num: usize, issues: Box<ClauseIssues>) -> VerificationFailure {
		VerificationFailure::invalid_clause(pos, num, issues, "CNF", "premise", "premise")
	}
	pub fn invalid_core(pos: SourcePosition, num: usize, issues: Box<ClauseIssues>) -> VerificationFailure {
		VerificationFailure::invalid_clause(pos, num, issues, "ASR", "core", "core")
	}
	pub fn invalid_rup(pos: SourcePosition, num: usize, issues: Box<ClauseIssues>) -> VerificationFailure {
		VerificationFailure::invalid_clause(pos, num, issues, "ASR", "RUP inference", "inference")
	}
	pub fn invalid_wsr(pos: SourcePosition, num: usize, issues: Box<ClauseIssues>) -> VerificationFailure {
		VerificationFailure::invalid_clause(pos, num, issues, "ASR", "WSR inference", "inference")
	}
	pub fn invalid_witness(pos: SourcePosition, num: usize, issues: Box<WitnessIssues>) -> VerificationFailure {
		let serious = issues.serious();
		let bx = Box::<InvalidWitness>::new(InvalidWitness {
			num: num,
			pos: pos,
			witness: issues.witness,
			issues: issues.issues,
			format: "ASR".to_string(),
		});
		if serious {
			VerificationFailure::InvalidWitness(bx)
		} else {
			VerificationFailure::UncompliantWitness(bx)
		}
	}
	pub fn conflict_id(pos: SourcePosition, num: usize, id: ClauseIndex, clause: ClauseContainer, format: &str, kind: &str, numbering: &str) -> VerificationFailure {
		VerificationFailure::ConflictId(Box::<ConflictId>::new(ConflictId {
			num: (num, numbering.to_string()),
			id: id,
			clause: clause,
			pos: pos,
			format: format.to_string(),
			kind: kind.to_string(),
		}))
	}
	pub fn conflict_core_id(pos: SourcePosition, num: usize, id: ClauseIndex, clause: ClauseContainer) -> VerificationFailure {
		VerificationFailure::conflict_id(pos, num, id, clause, "ASR", "core", "core")
	}
	pub fn conflict_rup_id(pos: SourcePosition, num: usize, id: ClauseIndex, clause: ClauseContainer) -> VerificationFailure {
		VerificationFailure::conflict_id(pos, num, id, clause, "ASR", "RUP inference", "inference")
	}
	pub fn conflict_wsr_id(pos: SourcePosition, num: usize, id: ClauseIndex, clause: ClauseContainer) -> VerificationFailure {
		VerificationFailure::conflict_id(pos, num, id, clause, "ASR", "WSR inference", "inference")
	}
	pub fn incorrect_core(pos: SourcePosition, num: usize, id: ClauseIndex, clause: ClauseContainer) -> VerificationFailure {
		VerificationFailure::IncorrectCore(Box::<ConflictId>::new(ConflictId {
			num: (num, "".to_string()),
			id: id,
			clause: clause,
			pos: pos,
			format: "ASR".to_string(),
			kind: "core".to_string(),
		}))
	}
	pub fn incorrect_rup(pos: SourcePosition, num: usize, clause: ClauseContainer, chain: PropagationIssues) -> VerificationFailure {
		let serious = chain.serious();
		let bx = Box::<IncorrectChain>::new(IncorrectChain {
			num: num,
			clause: clause,
			chain: chain,
			latid: None,
			latclause: ClauseContainer(Vec::new()),
			witness: WitnessContainer(Vec::new()),
			pos: pos,
			format: "ASR".to_string(),
		});
		if serious {
			VerificationFailure::IncorrectRup(bx)
		} else {
			VerificationFailure::UncompliantRup(bx)
		}
	}
	pub fn incorrect_wsr(pos: SourcePosition, num: usize, clause: ClauseContainer, chain: PropagationIssues, latid: Option<ClauseIndex>, latclause: ClauseContainer, witness: WitnessContainer) -> VerificationFailure {
		let serious = chain.serious();
		let bx = Box::<IncorrectChain>::new(IncorrectChain {
			num: num,
			clause: clause,
			chain: chain,
			latid: latid,
			latclause: latclause,
			witness: witness,
			pos: pos,
			format: "ASR".to_string(),
		});
		if serious {
			VerificationFailure::IncorrectWsr(bx)
		} else {
			VerificationFailure::UncompliantWsr(bx)
		}
	}
	pub fn invalid_laterals(pos: SourcePosition, num: usize, clause: ClauseContainer, lats: Box<Vec<(ClauseIndex, bool)>>) -> VerificationFailure {
		VerificationFailure::InvalidLaterals(Box::<InvalidLaterals>::new(InvalidLaterals {
			num: num,
			format: "ASR".to_string(),
			clause: clause,
			laterals: *lats,
			pos: pos,
		}))
	}
	pub fn missing_deletion(pos: SourcePosition, num: usize, id: ClauseIndex) -> VerificationFailure {
		VerificationFailure::MissingDeletion(Box::<MissingDeletion>::new(MissingDeletion {
			num: num,
			id: id,
			pos: pos,
			format: "ASR".to_string(),
		}))
	}
	pub fn unrefuted(pos: SourcePosition, num: usize) -> VerificationFailure {
		VerificationFailure::Unrefuted(Box::<RefutationError>::new(RefutationError {
			num: num,
			format: "ASR".to_string(),
			pos: pos,
		}))
	}
	pub fn serious(&self) -> bool {
		match self {
			VerificationFailure::IncorrectNumVariables(_) |
			VerificationFailure::IncorrectNumClauses(_) |
			VerificationFailure::WrongSection(_) |
			VerificationFailure::UncompliantClause(_) |
			VerificationFailure::UncompliantWitness(_) |
			VerificationFailure::MissingDeletion(_) |
			VerificationFailure::UncompliantRup(_) |
			VerificationFailure::InvalidLaterals(_) |
			VerificationFailure::UncompliantWsr(_) => false,
			VerificationFailure::InvalidClause(_) |
			VerificationFailure::InvalidWitness(_) |
			VerificationFailure::ConflictId(_) |
			VerificationFailure::IncorrectCore(_) |
			VerificationFailure::Unrefuted(_) |
			VerificationFailure::IncorrectRup(_) |
			VerificationFailure::IncorrectWsr(_) => true,
		}
	}
}
impl Display for VerificationFailure {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			VerificationFailure::IncorrectNumVariables(bx) => {
				write!(f, "{}", "Incorrect declared number of variables\n".white().bold())?;
				write!(f, "{}{} (CNF format)\n", "  -> ".blue(), &bx.pos)?;
				write!(f, "\tDeclared {} variables in CNF header, found {}.\n", &bx.expected, &bx.found)
			},
			VerificationFailure::IncorrectNumClauses(bx) => {
				write!(f, "{}", "Incorrect declared number of clauses\n".white().bold())?;
				write!(f, "{}{} (CNF format)\n", "  -> ".blue(), &bx.pos)?;
				write!(f, "\tDeclared {} clauses in CNF header, found {}.\n", &bx.expected, &bx.found)
			},
			VerificationFailure::WrongSection(bx) => {
				write!(f, "{}", "Invalid section\n".white().bold())?;
				write!(f, "{}{} ({} format)\n", "  -> ".blue(), &bx.pos, &bx.format)?;
				write!(f, "\tSection header '{}' is {}.\n", &bx.header, &bx.issue)
			},
			VerificationFailure::InvalidClause(bx) | VerificationFailure::UncompliantClause(bx) => {
				write!(f, "{}{}{}\n", "Invalid ".white().bold(), format!("{}", &bx.kind).white().bold(), " clause".white().bold())?;
				write!(f, "{}{} ({} format)\n", "  -> ".blue(), &bx.pos, &bx.format)?;
				write!(f, "\tClause {} at {} position #{} is invalid because:\n", format!("{}", &bx.clause).bold().yellow(), &bx.num.1, &bx.num.0)?;
				for (lit, rep) in &bx.issues {
					if *rep {
						write!(f, "\t  Literal {} is repeated.\n", lit)?;
					} else {
						write!(f, "\t  Literal {} is inconsistent with literal {}.\n", lit, lit.complement())?;
					}
				}
				Ok(())
			},
			VerificationFailure::InvalidWitness(bx) | VerificationFailure::UncompliantWitness(bx) => {
				write!(f, "{}\n", "Invalid WSR witness".white().bold())?;
				write!(f, "{}{} ({} format)\n", "  -> ".blue(), &bx.pos, &bx.format)?;
				write!(f, "\tWitness {} for WSR inference at inference position #{} is invalid because:\n", format!("{}", &bx.witness).bold().magenta(), &bx.num)?;
				for (var, lit, rep) in &bx.issues {
					if *rep {
						write!(f, "\t  Mapping {} -> {} is repeated.\n", var, lit)?;
					} else {
						write!(f, "\t  Mapping {} -> {} is inconsistent with a previous mapping.\n", var, lit)?;
					}
				}
				Ok(())
			},
			VerificationFailure::ConflictId(bx) => {
				write!(f, "{}\n", "Conflicting clause identifier".white().bold())?;
				write!(f, "{}{} ({} format)\n", "  -> ".blue(), &bx.pos, &bx.format)?;
				write!(f, "\tA clause is introduced as a {} with clause identifier {} at {} position #{}", &bx.kind, &bx.id, &bx.num.1, &bx.num.0)?;
				write!(f, ", but clause {} was already assigned that identifier.\n", format!("{}", &bx.clause).bold().yellow())
			},
			VerificationFailure::IncorrectCore(bx) => {
				write!(f, "{}\n", "Incorrect core introduction".white().bold())?;
				write!(f, "{}{} ({} format)\n", "  -> ".blue(), &bx.pos, &bx.format)?;
				write!(f, "\tThe clause {} is introduced as a core clause ", format!("{}", &bx.clause).bold().yellow())?;
				write!(f, "with clause identifier {} at core position #{}, but was not found among the premises.\n", &bx.id, &bx.num.0)
			},
			VerificationFailure::IncorrectRup(bx) | VerificationFailure::UncompliantRup(bx) => {
				write!(f, "{}\n", "Incorrect RUP introduction".white().bold())?;
				write!(f, "{}{} ({} format)\n", "  -> ".blue(), &bx.pos, &bx.format)?;
				write!(f, "\tThe clause {} is introduced as a RUP inference clause ", format!("{}", &bx.clause).bold().yellow())?;
				write!(f, "at inference position #{}, but the specified unit propagation chain is invalid because:", &bx.num)?;
				if bx.chain.chain.is_empty() {
					write!(f, "(empty)\n")?;
				} else {
					for (clause, id, resopt) in &bx.chain.chain {
						write!(f, "\t  {}: {}  ", id, format!("{}", clause).yellow())?;
						match resopt {
							Some(PropagationResult::Missing(_)) => write!(f, "clause is missing from currently derived formula\n")?,
							Some(PropagationResult::Null(_)) => write!(f, "clause does not propagate a literal or produce a conflict\n")?,
							Some(PropagationResult::Done(_)) => write!(f, "conflict was already reached\n")?,
							_ => write!(f, "\n")?,
						}
					}
				}
				if !bx.chain.conflict {
					write!(f, "a conflict is not reached\n")?;
				}
				Ok(())
			},
			VerificationFailure::IncorrectWsr(bx) | VerificationFailure::UncompliantWsr(bx) => {
				write!(f, "{}\n", "Incorrect WSR introduction".white().bold())?;
				write!(f, "{}{} ({} format)\n", "  -> ".blue(), &bx.pos, &bx.format)?;
				write!(f, "\tThe clause {} is introduced as a WSR inference clause ", format!("{}", &bx.clause).bold().yellow())?;
				write!(f, "upon the witness {} ", format!("{}", &bx.witness).bold().magenta())?;
				write!(f, "at inference position #{}. The unit propagation chain for the ", &bx.num)?;
				match &bx.latid {
					Some(id) => write!(f, "{}: {} ", id, format!("{}", &bx.latclause).bold().yellow())?,
					None => write!(f, "clause itself ")?,
				}
				write!(f, "is invalid because:")?;
				if bx.chain.chain.is_empty() {
					write!(f, "(empty)\n")?;
				} else {
					for (clause, id, resopt) in &bx.chain.chain {
						write!(f, "\t  {}: {}  ", id, format!("{}", clause).yellow())?;
						match resopt {
							Some(PropagationResult::Missing(_)) => write!(f, "clause is missing from currently derived formula\n")?,
							Some(PropagationResult::Null(_)) => write!(f, "clause does not propagate a literal or produce a conflict\n")?,
							Some(PropagationResult::Done(_)) => write!(f, "conflict was already reached\n")?,
							_ => write!(f, "\n")?,
						}
					}
				}
				if !bx.chain.conflict {
					write!(f, "a conflict is not reached\n")?;
				}
				Ok(())
			},
			VerificationFailure::InvalidLaterals(bx) => {
				write!(f, "{}\n", "Invalid lateral chains in WSR inference".white().bold())?;
				write!(f, "{}{} ({} format)\n", "  -> ".blue(), &bx.pos, &bx.format)?;
				write!(f, "\tThe clause {} is introduced as a WSR inference clause ", format!("{}", &bx.clause).bold().yellow())?;
				write!(f, "at inference position #{}, but the unit propagation chains for the following laterals are invalid:\n", &bx.num)?;
				for (id, rep) in &bx.laterals {
					if *rep {
						write!(f, "\t  {}: another unit propagation chain exists for this lateral clause\n", id)?;
					} else {
						write!(f, "\t  {}: no clause occurs in the currently derived formula with this clause identifier\n", id)?;
					}
				}
				Ok(())
			},
			VerificationFailure::MissingDeletion(bx) => {
				write!(f, "{}\n", "Missing clause deletion".white().bold())?;
				write!(f, "{}{} ({} format)\n", "  -> ".blue(), &bx.pos, &bx.format)?;
				write!(f, "\tThe deletion instruction at inference position #{} ", &bx.num)?;
				write!(f, "deletes a clause with clause identifier {}, which does not occur in the currently derived formula.\n", &bx.id)
			},
			VerificationFailure::Unrefuted(bx) => {
				write!(f, "{}\n", "Unrefuted derivation".white().bold())?;
				write!(f, "{}{} ({} format)\n", "  -> ".blue(), &bx.pos, &bx.format)?;
				write!(f, "\tNo empty clause occurs in the derived formula at the end of the derivation, after inference position #{}.\n", &bx.num)
			},
		}
	}
}
impl Error for VerificationFailure {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
		None
    }
}


pub type VerificationResult<T> = Result<T, VerificationFailure>;

pub struct ErrorStorage {
	vec: Vec<VerificationFailure>,
	permissive: bool,
}
impl ErrorStorage {
	pub fn new(permissive: bool) -> ErrorStorage {
		ErrorStorage {
			vec: Vec::new(),
			permissive: permissive,
		}
	}
	pub fn push_error(&mut self, handle: LoggerHandle<'_, '_>, error: VerificationFailure) {
		if self.permissive && !error.serious() {
			log_warning!(handle, target: "Warning"; "{}", &error);
		} else {
			log_error!(handle, target: "Error"; "{}", &error);
		}
		self.vec.push(error);
	}
	pub fn count(&self) -> (usize, usize) {
		let mut warnings = 0usize;
		let mut errors = 0usize;
		for err in &self.vec {
			if self.permissive && !err.serious() {
				warnings += 1usize;
			} else {
				errors += 1usize;
			}
		}
		(warnings, errors)
	}
}
