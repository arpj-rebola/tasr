use std::{
	convert::{TryFrom},
	error::{Error},
	fmt::{self, Debug, Display, Binary, Formatter},
	io::{self},
};

use either::{
	Either::{self, Left, Right},
};

use crate::{
	input::{FilePosition, Positionable, InputStream},
	lexer::{LexingError, DimacsLexer, VbeLexer, DimacsLexeme, VbeLexeme, DimacsDiscriminant},
	variable::{Variable, Literal, MaybeVariable},
	clausedb::{ClauseIndex},
};

pub struct OutOfRangeField {
	pos: FilePosition,
	number: String,
	range: String,
	field: String,
}

pub struct ExpectedField {
	pos: FilePosition,
	field: String,
}

pub enum ParsingError {
	LexingError(Box<LexingError>),
	OutOfRangeField(Box<OutOfRangeField>),
	ExpectedField(Box<ExpectedField>),
}
impl ParsingError {
	fn out_of_range_field<P: Positionable>(input: &P, num: i64, range: String, field: &str) -> ParsingError {
		ParsingError::OutOfRangeField(Box::<OutOfRangeField>::new(OutOfRangeField {
			pos: input.position().clone(),
			number: format!("{}", num),
			range: range,
			field: field.to_string(),
		}))
	}
	fn out_of_range_field_u64<P: Positionable>(input: &P, num: u64, range: String, field: &str) -> ParsingError {
		ParsingError::OutOfRangeField(Box::<OutOfRangeField>::new(OutOfRangeField {
			pos: input.position().clone(),
			number: format!("{}", num),
			range: range,
			field: field.to_string(),
		}))
	}
	fn expected_field<P: Positionable>(input: &P, field: &str) -> ParsingError {
		ParsingError::ExpectedField(Box::<ExpectedField>::new(ExpectedField {
			pos: input.position().clone(),
			field: field.to_string(),
		}))
	}
}
impl From<LexingError> for ParsingError {
	fn from(err: LexingError) -> ParsingError {
		ParsingError::LexingError(Box::<LexingError>::new(err))
	}
}
impl Debug for ParsingError {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "{}", self)
	}
}
impl Display for ParsingError {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			ParsingError::LexingError(bx) => write!(f, "{}", &**bx),
			ParsingError::OutOfRangeField(bx) => write!(f, "Field {} out of range in file {}:\nFound {}, valid range {}.", &bx.field, &bx.pos, &bx.number, &bx.range),
			ParsingError::ExpectedField(bx) => write!(f, "Expected {} in file {}.", &bx.field, &bx.pos),
		}
	}
}
impl Binary for ParsingError {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			ParsingError::LexingError(bx) => write!(f, "{:b}", &**bx),
			ParsingError::OutOfRangeField(bx) => write!(f, "Field {} out of range in file {:b}:\nFound {}, valid range {}.", &bx.field, &bx.pos, &bx.number, &bx.range),
			ParsingError::ExpectedField(bx) => write!(f, "Expected {} in file {:b}.", &bx.field, &bx.pos),
		}
	}
}
impl Error for ParsingError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
		match self {
			ParsingError::LexingError(bx) => Some(bx),
			_ => None,
		}
    }
}

type ParsingResult<T> = Result<T, ParsingError>;

pub struct CnfHeaderStats {
	pub variables: MaybeVariable,
	pub clauses: usize
}

pub enum AsrInstructionKind {
	Rup(ClauseIndex),
	Sr(ClauseIndex),
	Del,
}

pub trait CnfParser: 'static + Positionable {
	fn skip_to_header(&mut self) -> ParsingResult<Option<[u8; 8]>>;
	fn parse_cnf_header(&mut self) -> ParsingResult<CnfHeaderStats>;
	fn parse_formula(&mut self) -> ParsingResult<Option<()>>;
	fn parse_clause(&mut self) -> ParsingResult<Option<Literal>>;
}

pub trait AsrParser: 'static + Positionable {
	fn skip_to_header(&mut self) -> ParsingResult<Option<[u8; 8]>>;
	fn parse_core(&mut self) -> ParsingResult<Option<ClauseIndex>>;
	fn parse_proof(&mut self) -> ParsingResult<Option<AsrInstructionKind>>;
	fn parse_clause(&mut self) -> ParsingResult<Option<Literal>>;
	fn parse_chain(&mut self) -> ParsingResult<Option<ClauseIndex>>;
	fn parse_witness(&mut self) -> ParsingResult<Option<(Variable, Literal)>>;
}

pub struct DimacsCnfParser {
	lexer: DimacsLexer,
}
impl DimacsCnfParser {
	pub fn new(is: InputStream) -> DimacsCnfParser {
		DimacsCnfParser { lexer: DimacsLexer::new(is) }
	}
	#[inline]
	fn next(&mut self) -> ParsingResult<Option<DimacsLexeme>> {
		self.lexer.next_err::<ParsingError>()
	}
	fn error_expected_cnf_header<T>(&self) -> ParsingResult<T> {
		Err(ParsingError::expected_field(self, "header 'p cnf'"))
	}
	fn error_expected_num_variables<T>(&self) -> ParsingResult<T> {
		Err(ParsingError::expected_field(self, "number of variables"))
	}
	fn error_expected_num_clauses<T>(&self) -> ParsingResult<T> {
		Err(ParsingError::expected_field(self, "number of clauses"))
	}
	fn error_expected_clause<T>(&self) -> ParsingResult<T> {
		Err(ParsingError::expected_field(self, "clause or EOF"))
	}
	fn error_expected_clause_literal<T>(&self) -> ParsingResult<T> {
		Err(ParsingError::expected_field(self, "literal or end-of-clause zero"))
	}
	fn error_oor_num_variables<T>(&self, num: i64) -> ParsingResult<T> {
		let range = format!("[{}, {}]", 0i64, Variable::MaxValue);
		Err(ParsingError::out_of_range_field(self, num, range, "number of variables"))
	}
	fn error_oor_num_clauses<T>(&self, num: i64) -> ParsingResult<T> {
		let max = i64::try_from(usize::max_value()).unwrap_or(i64::max_value());
		let range = format!("[{}, {}]", 0i64, max);
		Err(ParsingError::out_of_range_field(self, num, range, "number of clauses"))
	}
	fn error_oor_literal<T>(&self, num: i64) -> ParsingResult<T> {
		let range = format!("[{}, {}] U [{}, {}]", -Literal::MaxValue, -1i64, 1i64, Literal::MaxValue);
		Err(ParsingError::out_of_range_field(self, num, range, "literal"))
	}
}
impl CnfParser for DimacsCnfParser {
	fn skip_to_header(&mut self) -> ParsingResult<Option<[u8; 8]>> {
		loop { match self.next()? {
			Some(DimacsLexeme::Header(hd)) => break Ok(Some(hd)),
			None => break Ok(None),
			_ => (),
		} }
	}
	fn parse_cnf_header(&mut self) -> ParsingResult<CnfHeaderStats> {
		let var = match self.next()? {
			Some(DimacsLexeme::Number(num)) => match MaybeVariable::try_from(num) {
				Ok(var) => var,
				Err(x) => self.error_oor_num_variables(x)?,
			},
			_ => self.error_expected_num_variables()?,
		};
		let cls = match self.next()? {
			Some(DimacsLexeme::Number(num)) => match usize::try_from(num) {
				Ok(cls) => cls,
				Err(_) => self.error_oor_num_clauses(num)?,
			},
			_ => self.error_expected_num_clauses()?,
		};
		Ok(CnfHeaderStats {
			variables: var,
			clauses: cls,
		})
	}
	fn parse_formula(&mut self) -> ParsingResult<Option<()>> {
		match self.lexer.peek_err::<ParsingError>()? {
			Some(DimacsDiscriminant::Number) => Ok(Some(())),
			Some(DimacsDiscriminant::Header) | None => Ok(None),
			_ => self.error_expected_clause(),
		}
	}
	fn parse_clause(&mut self) -> ParsingResult<Option<Literal>> {
		match self.next()? {
			Some(DimacsLexeme::Number(num)) => match Literal::try_from(num) {
				Ok(lit) => Ok(Some(lit)),
				Err(0i64) => Ok(None),
				Err(x) => self.error_oor_literal(x)?,
			},
			_ => self.error_expected_clause_literal()?,
		}
	}
}
impl Positionable for DimacsCnfParser {
	fn position(&self) -> &FilePosition {
		self.lexer.position()
	}
}

struct DimacsAsrParser {
	lexer: DimacsLexer,
}
impl DimacsAsrParser {
	pub fn new(is: InputStream) -> DimacsAsrParser {
		DimacsAsrParser { lexer: DimacsLexer::new(is) }
	}
	#[inline]
	fn next(&mut self) -> ParsingResult<Option<DimacsLexeme>> {
		self.lexer.next_err::<ParsingError>()
	}
	fn parse_proper_id(&mut self) -> ParsingResult<ClauseIndex> {
		match self.next()? {
			Some(DimacsLexeme::Number(num)) => match ClauseIndex::try_from(num) {
				Ok(id) => Ok(id),
				Err(x) => self.error_oor_id(x),
			},
			_ => self.error_expected_proper_id(),
		}
	}
	fn error_expected_asr_header<T>(&self) -> ParsingResult<T> {
		Err(ParsingError::expected_field(self, "header 'p asr'"))
	}
	fn error_expected_core_header<T>(&self) -> ParsingResult<T> {
		Err(ParsingError::expected_field(self, "header 'p core'"))
	}
	fn error_expected_proof_header<T>(&self) -> ParsingResult<T> {
		Err(ParsingError::expected_field(self, "header 'p proof'"))
	}
	fn error_expected_core<T>(&self) -> ParsingResult<T> {
		Err(ParsingError::expected_field(self, "core marker, proof header, or EOF"))
	}
	fn error_expected_proper_id<T>(&self) -> ParsingResult<T> {
		Err(ParsingError::expected_field(self, "clause id"))
	}
	fn error_expected_chain_id<T>(&self) -> ParsingResult<T> {
		Err(ParsingError::expected_field(self, "clause id or end-of-chain zero"))
	}
	fn error_expected_variable<T>(&self) -> ParsingResult<T> {
		Err(ParsingError::expected_field(self, "variable or end-of-witness zero"))
	}
	fn error_expected_clause_literal<T>(&self) -> ParsingResult<T> {
		Err(ParsingError::expected_field(self, "literal or end-of-clause zero"))
	}
	fn error_expected_witness_literal<T>(&self) -> ParsingResult<T> {
		Err(ParsingError::expected_field(self, "literal, top 't', or bottom 'f'"))
	}
	fn error_expected_instruction<T>(&self) -> ParsingResult<T> {
		Err(ParsingError::expected_field(self, "proof instruction marker or EOF"))
	}
	fn error_oor_id<T>(&self, num: i64) -> ParsingResult<T> {
		let range = format!("[{}, {}]", 1i64, ClauseIndex::MaxValue);
		Err(ParsingError::out_of_range_field(self, num, range, "clause id"))
	}
	fn error_oor_literal<T>(&self, num: i64) -> ParsingResult<T> {
		let range = format!("[{}, {}] U [{}, {}]", -Literal::MaxValue, -1i64, 1i64, Literal::MaxValue);
		Err(ParsingError::out_of_range_field(self, num, range, "literal"))
	}
	fn error_oor_variable<T>(&self, num: i64) -> ParsingResult<T> {
		let range = format!("[{}, {}] U [{}, {}]", -Variable::MaxValue, -1i64, 1i64, Variable::MaxValue);
		Err(ParsingError::out_of_range_field(self, num, range, "variable"))
	}
}
impl AsrParser for DimacsAsrParser {
	fn skip_to_header(&mut self) -> ParsingResult<Option<[u8; 8]>> {
		loop { match self.next()? {
			Some(DimacsLexeme::Header(hd)) => break Ok(Some(hd)),
			None => break Ok(None),
			_ => (),
		} }
	}
	fn parse_core(&mut self) -> ParsingResult<Option<ClauseIndex>> {
		match self.lexer.peek_err::<ParsingError>()? {
			Some(DimacsDiscriminant::Letter) => match self.next()? {
				Some(DimacsLexeme::Letter(b'k')) => Ok(Some(self.parse_proper_id()?)),
				_ => self.error_expected_core(),
			},
			Some(DimacsDiscriminant::Header) | None => Ok(None),
			_ => self.error_expected_core(),
		}
	}
	fn parse_proof(&mut self) -> ParsingResult<Option<AsrInstructionKind>> {
		match self.lexer.peek_err::<ParsingError>()? {
			Some(DimacsDiscriminant::Letter) => match self.next()? {
				Some(DimacsLexeme::Letter(b'r')) => Ok(Some(AsrInstructionKind::Rup(self.parse_proper_id()?))),
				Some(DimacsLexeme::Letter(b's')) => Ok(Some(AsrInstructionKind::Sr(self.parse_proper_id()?))),
				Some(DimacsLexeme::Letter(b'd')) => Ok(Some(AsrInstructionKind::Del)),
				_ => self.error_expected_instruction(),
			},
			Some(DimacsDiscriminant::Header) | None => Ok(None),
			_ => self.error_expected_instruction(),
		}
	}
	fn parse_clause(&mut self) -> ParsingResult<Option<Literal>> {
		match self.next()? {
			Some(DimacsLexeme::Number(num)) => match Literal::try_from(num) {
				Ok(lit) => Ok(Some(lit)),
				Err(0i64) => Ok(None),
				Err(x) => self.error_oor_literal(x),
			},
			_ => self.error_expected_clause_literal(),
		}
	}
	fn parse_chain(&mut self) -> ParsingResult<Option<ClauseIndex>> {
		match self.next()? {
			Some(DimacsLexeme::Number(num)) => match ClauseIndex::try_from(num) {
				Ok(lit) => Ok(Some(lit)),
				Err(0i64) => Ok(None),
				Err(x) => self.error_oor_id(x),
			},
			_ => self.error_expected_chain_id(),
		}
	}
	fn parse_witness(&mut self) -> ParsingResult<Option<(Variable, Literal)>> {
		let (var, comp) = match self.next()? {
			Some(DimacsLexeme::Number(num)) => match Variable::try_from(num) {
				Ok(var) => (var, num < 0i64),
				Err(0i64) => return Ok(None),
				Err(x) => self.error_oor_variable(x)?,
			},
			_ => self.error_expected_variable()?,
		};
		let pre_lit = match self.next()? {
			Some(DimacsLexeme::Number(num)) => match Literal::try_from(num) {
				Ok(lit) => lit,
				Err(x) => self.error_oor_literal(x)?,
			},
			Some(DimacsLexeme::Letter(b't')) => Literal::Top,
			Some(DimacsLexeme::Letter(b'f')) => Literal::Bottom,
			_ => self.error_expected_witness_literal()?,
		};
		let lit = if comp {
			pre_lit.complement()
		} else {
			pre_lit
		};
		Ok(Some((var, lit)))
	}
}
impl Positionable for DimacsAsrParser {
	fn position(&self) -> &FilePosition {
		self.lexer.position()
	}
}

// struct VbeAsrParser {
// 	lexer: VbeLexer,
// }
// impl VbeAsrParser {
// 	pub fn new(is: InputStream) -> VbeAsrParser {
// 		VbeAsrParser { lexer: VbeLexer::new(is) }
// 	}
// 	#[inline]
// 	fn next(&mut self) -> ParsingResult<Option<VbeLexeme>> {
// 		self.lexer.next_err::<ParsingError>()
// 	}
// 	fn parse_proper_id(&mut self) -> ParsingResult<ClauseIndex> {
// 		match self.next()? {
// 			Some(vbe) => match ClauseIndex::try_from(vbe) {
// 				Ok(id) => Ok(id),
// 				Err(x) => self.error_oor_id(x),
// 			},
// 			_ => self.error_expected_proper_id(),
// 		}
// 	}
// 	fn error_expected_asr_header<T>(&self) -> ParsingResult<T> {
// 		Err(ParsingError::expected_field(self, "header '0x727361'"))
// 	}
// 	fn error_expected_core_header<T>(&self) -> ParsingResult<T> {
// 		Err(ParsingError::expected_field(self, "header '0x65726F63'"))
// 	}
// 	fn error_expected_core<T>(&self) -> ParsingResult<T> {
// 		Err(ParsingError::expected_field(self, "core marker, proof header '0x666F6F7270', or EOF"))
// 	}
// 	fn error_expected_proper_id<T>(&self) -> ParsingResult<T> {
// 		Err(ParsingError::expected_field(self, "clause id"))
// 	}
// 	fn error_expected_chain_id<T>(&self) -> ParsingResult<T> {
// 		Err(ParsingError::expected_field(self, "clause id or end-of-chain 0x00"))
// 	}
// 	fn error_expected_variable<T>(&self) -> ParsingResult<T> {
// 		Err(ParsingError::expected_field(self, "variable or end-of-witness 0x00"))
// 	}
// 	fn error_expected_clause_literal<T>(&self) -> ParsingResult<T> {
// 		Err(ParsingError::expected_field(self, "literal or end-of-clause 0x00"))
// 	}
// 	fn error_expected_witness_literal<T>(&self) -> ParsingResult<T> {
// 		Err(ParsingError::expected_field(self, "literal, top 0x00, or bottom 0x01"))
// 	}
// 	fn error_expected_instruction<T>(&self) -> ParsingResult<T> {
// 		Err(ParsingError::expected_field(self, "proof instruction marker or EOF"))
// 	}
// 	fn error_oor_id<T>(&self, num: VbeLexeme) -> ParsingResult<T> {
// 		let range = format!("[{}, {}]", 1i64, ClauseIndex::MaxValue);
// 		Err(ParsingError::out_of_range_field_u64(self, num.into(), range, "clause id"))
// 	}
// 	fn error_oor_literal<T>(&self, num: VbeLexeme) -> ParsingResult<T> {
// 		let range = format!("[{}, {}]", 2u64, 2i64 * Literal::MaxValue);
// 		Err(ParsingError::out_of_range_field_u64(self, num.into(), range, "literal"))
// 	}
// 	fn error_oor_variable<T>(&self, num: VbeLexeme) -> ParsingResult<T> {
// 		let range = format!("[{}, {}]", 2u64, 2i64 * Literal::MaxValue);
// 		Err(ParsingError::out_of_range_field_u64(self, num.into(), range, "variable"))
// 	}
// }
// impl AsrParser for VbeAsrParser {
// 	fn parse_header(&mut self) -> ParsingResult<()> {
// 		let fst = self.next()?;
// 		let snd = self.next()?;
// 		match (fst, snd) {
// 			(Some(VbeLexeme::PlusZero), Some(vbe)) if vbe.header() == [b'a', b's', b'r', 0u8, 0u8, 0u8, 0u8, 0u8] => (),
// 			_ => self.error_expected_asr_header()?,
// 		}
// 		let fst = self.next()?;
// 		let snd = self.next()?;
// 		match (fst, snd) {
// 			(Some(VbeLexeme::PlusZero), Some(vbe)) if vbe.header() == [b'c', b'o', b'r', b'e', 0u8, 0u8, 0u8, 0u8] => Ok(()),
// 			_ => self.error_expected_core_header()?,
// 		}
// 	}
// 	fn parse_core(&mut self) -> ParsingResult<Either<ClauseIndex, Option<()>>> {
// 		match self.next()? {
// 			Some(VbeLexeme::CharacterK) => Ok(Left(self.parse_proper_id()?)),
// 			Some(vbe) if vbe.header() == [b'p', b'r', b'o', b'o', b'f', 0u8, 0u8, 0u8] => Ok(Right(Some(()))),
// 			None => Ok(Right(None)),
// 			_ => self.error_expected_core(),
// 		}
// 	}
// 	fn parse_proof(&mut self) -> ParsingResult<Option<AsrInstructionKind>> {
// 		match self.next()? {
// 			Some(VbeLexeme::CharacterR) => Ok(Some(AsrInstructionKind::Rup(self.parse_proper_id()?))),
// 			Some(VbeLexeme::CharacterS) => Ok(Some(AsrInstructionKind::Sr(self.parse_proper_id()?))),
// 			Some(VbeLexeme::CharacterX) => Ok(Some(AsrInstructionKind::Xr(self.parse_proper_id()?))),
// 			Some(VbeLexeme::CharacterD) => Ok(Some(AsrInstructionKind::Del)),
// 			None => Ok(None),
// 			_ => self.error_expected_instruction(),
// 		}
// 	}
// 	fn parse_clause(&mut self) -> ParsingResult<Option<Literal>> {
// 		match self.next()? {
// 			Some(vbe) => match Literal::try_from(vbe) {
// 				Ok(lit) => Ok(Some(lit)),
// 				Err(VbeLexeme::PlusZero) => Ok(None),
// 				Err(x) => self.error_oor_literal(x),
// 			},
// 			_ => self.error_expected_clause_literal(),
// 		}
// 	}
// 	fn parse_chain(&mut self) -> ParsingResult<Option<ClauseIndex>> {
// 		match self.next()? {
// 			Some(vbe) => match ClauseIndex::try_from(vbe) {
// 				Ok(lit) => Ok(Some(lit)),
// 				Err(VbeLexeme::PlusZero) => Ok(None),
// 				Err(x) => self.error_oor_id(x),
// 			},
// 			_ => self.error_expected_chain_id(),
// 		}
// 	}
// 	fn parse_witness(&mut self) -> ParsingResult<Option<(Variable, Literal)>> {
// 		let (var, comp) = match self.next()? {
// 			Some(vbe) => match Variable::try_from(vbe) {
// 				Ok(var) => (var, vbe.number() & 1u64 == 1u64),
// 				Err(VbeLexeme::PlusZero) => return Ok(None),
// 				Err(x) => self.error_oor_variable(x)?,
// 			},
// 			_ => self.error_expected_variable()?,
// 		};
// 		let pre_lit = match self.next()? {
// 			Some(vbe) => match Literal::try_from(vbe) {
// 				Ok(lit) => lit,
// 				Err(VbeLexeme::PlusZero) => Literal::Top,
// 				Err(VbeLexeme::MinusZero) => Literal::Bottom,
// 				Err(x) => self.error_oor_literal(x)?,
// 			},
// 			_ => self.error_expected_witness_literal()?,
// 		};
// 		let lit = if comp {
// 			pre_lit.complement()
// 		} else {
// 			pre_lit
// 		};
// 		Ok(Some((var, lit)))
// 	}
// }
// impl Positionable for VbeAsrParser {
// 	fn position(&self) -> &FilePosition {
// 		self.lexer.position()
// 	}
// }
