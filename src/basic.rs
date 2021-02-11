use std::{
	fmt::{Result as FmtResult, Debug, Display, Formatter},
	convert::{TryFrom},
	cmp::{Ordering},
	ops::{BitOr, BitOrAssign},
	num::{NonZeroU32, NonZeroU64},
	mem::{self},
};

/// A literal structure. A literal is either a variable, or its complement, or top or bottom.
#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct Literal {
    val: u32,
}
impl Literal {
	/// Maximum positive value as an `i64` that can be converted into a `Literal`. The minimum negative value as an `i64` that can be converted into a `Literal` is the opposite of this number.
	pub const MaxValue: i64 = (u32::max_value() as i64) >> 1;
	/// Top literal.
	pub const Top: Literal = Literal::new(0u32);
	/// Bottom literal.
	pub const Bottom: Literal = Literal::new(1u32);
	#[inline(always)]
	const fn new(val: u32) -> Literal {
		Literal { val: val }
	}
	/// Returns `true` if the literal is positive-signed.
	#[inline(always)]
	pub const fn positive(self) -> bool {
		self.val & 1u32 == 0u32
	}
	/// Returns `true` if the literal is negative-signed.
	#[inline(always)]
	pub const fn negative(self) -> bool {
		!self.positive()
	}
	/// Returns the complement of a literal.
	#[inline(always)]
	pub const fn complement(self) -> Literal {
		Literal::new(self.val ^ 1u32)
	}
	/// Returns a `MaybeVariable` containing the variable underlying this literal, or `None` if the literal is top or bottom.
	#[inline(always)]
	pub fn variable(self) -> MaybeVariable {
		unsafe { mem::transmute::<u32, MaybeVariable>(self.val & !1u32) }
	}
	/// Returns the ordering index of a literal.
	#[inline(always)]
	pub fn index(self) -> usize {
		self.val as usize
	}
	#[inline(always)]
	pub fn text(&self) -> TextLiteral<'_> {
		TextLiteral(self)
	}
}
impl TryFrom<i64> for Literal {
	type Error = i64;
	/// An `i64` is converted into a Literal according to the following rules: `0i64` is never converted into a literal, any other in-range number `n` is converted into a literal with the same underlying variable obtained by converting the absolute value of `n`; positive numbers are converted into positive literals and similarly for negative numbers.
	fn try_from(num: i64) -> Result<Literal, i64> {
		if num > 0i64 {
			if num > Self::MaxValue {
				Err(num)
			} else {
				Ok(Literal::new((num as u32) << 1usize))
			}
		} else {
			if num == 0i64 || num < -Self::MaxValue {
				Err(num)
			} else {
				Ok(Literal::new(((-num as u32) << 1usize) | 1u32))
			}
		}
	}
}
impl Debug for Literal {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		<Literal as Display>::fmt(self, f)
	}
}
impl Display for Literal {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		if self.val > 1u32 {
			if self.val & 1u32 == 0u32 {
				write!(f, "+{}", self.val >> 1)
			} else {
				write!(f, "-{}", self.val >> 1)
			}
		} else {
			if self.val == 0u32 {
				write!(f, "T")
			} else {
				write!(f, "F")
			}
		}
	}
}

pub struct TextLiteral<'a>(&'a Literal);
impl<'a> Display for TextLiteral<'a> {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		if self.0.val > 1u32 {
			if self.0.val & 1u32 == 0u32 {
				write!(f, "{}", self.0.val >> 1)
			} else {
				write!(f, "-{}", self.0.val >> 1)
			}
		} else {
			if self.0.val == 0u32 {
				write!(f, "t")
			} else {
				write!(f, "f")
			}
		}
	}
}

/// A propositional variable structure.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct Variable {
	val: NonZeroU32,
}
impl Variable {
	/// Maximum value as an `i64` that can be converted into a Variable.
	pub const MaxValue: i64 = (u32::max_value() as i64) >> 1;
	/// Returns the positive literal with this underlying variable.
	#[inline(always)]
	pub fn positive(self) -> Literal {
		Literal::new(self.val.get())
	}
	/// Returns the negative literal with this underlying variable.
	#[inline(always)]
	pub fn negative(self) -> Literal {
		Literal::new(self.val.get() | 1u32)
	}
	/// Returns the ordering index of a variable.
	#[inline(always)]
	pub fn index(self) -> usize {
		((self.val.get() >> 1) - 1u32) as usize
	}
	#[inline(always)]
	pub fn text(&self) -> TextVariable<'_> {
		TextVariable(self)
	}
}
impl TryFrom<i64> for Variable {
	type Error = i64;
	fn try_from(num: i64) -> Result<Variable, i64> {
		if num > 0i64 && num <= Self::MaxValue {
			unsafe { Ok(Variable { val: NonZeroU32::new_unchecked((num as u32) << 1) }) }
		} else {
			Err(num)
		}
	}
}
impl BitOr for Variable {
	type Output = Variable;
	#[inline(always)]
	fn bitor(self, var: Variable) -> Variable {
		Variable { val: self.val.max(var.val) }
	}
}
impl BitOrAssign for Variable {
	#[inline(always)]
	fn bitor_assign(&mut self, var: Variable) {
		self.val = self.val.max(var.val)
	}
}
impl Debug for Variable {
	fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
		write!(f, "{}", self.val.get() >> 1)
	}
}
impl Display for Variable {
	fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
		write!(f, "{}", self.val.get() >> 1)
	}
}

pub struct TextVariable<'a>(&'a Variable);
impl<'a> Display for TextVariable<'a> {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		write!(f, "{}", self.0.val.get() >> 1)
	}
}

#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct MaybeVariable {
    val: Option<Variable>,
}
impl MaybeVariable {
	#[inline(always)]
	pub const fn new(val: Option<Variable>) -> MaybeVariable {
		MaybeVariable { val: val }
	}
	#[inline(always)]
	pub const fn get(self) -> Option<Variable> {
		self.val
	}
	#[inline(always)]
	pub fn text(&self) -> TextMaybeVariable<'_> {
		TextMaybeVariable(self)
	}
}
impl TryFrom<i64> for MaybeVariable {
	type Error = i64;
	fn try_from(num: i64) -> Result<MaybeVariable, i64> {
		if num >= 0i64 && num <= Variable::MaxValue {
			unsafe { Ok(mem::transmute::<u32, MaybeVariable>((num as u32) << 1)) }
		} else {
			Err(num)
		}
	}
}
impl PartialOrd for MaybeVariable {
	#[inline(always)]
    fn partial_cmp(&self, oth: &MaybeVariable) -> Option<Ordering> {
        unsafe { mem::transmute::<&MaybeVariable, &u32>(self).partial_cmp(mem::transmute::<&MaybeVariable, &u32>(oth)) }
    }
}
impl Ord for MaybeVariable {
	#[inline(always)]
    fn cmp(&self, oth: &MaybeVariable) -> Ordering {
		unsafe { mem::transmute::<&MaybeVariable, &u32>(self).cmp(mem::transmute::<&MaybeVariable, &u32>(oth)) }
    }
}
impl BitOr<MaybeVariable> for MaybeVariable {
	type Output = MaybeVariable;
	#[inline(always)]
	fn bitor(self, var: MaybeVariable) -> MaybeVariable {
		unsafe { mem::transmute::<u32, MaybeVariable>(mem::transmute::<MaybeVariable, u32>(self).max(mem::transmute::<MaybeVariable, u32>(var))) }
	}
}
impl BitOr<Variable> for MaybeVariable {
	type Output = MaybeVariable;
	#[inline(always)]
	fn bitor(self, var: Variable) -> MaybeVariable {
		unsafe { mem::transmute::<u32, MaybeVariable>(mem::transmute::<MaybeVariable, u32>(self).max(mem::transmute::<Variable, u32>(var))) }
	}
}
impl BitOr<Literal> for MaybeVariable {
	type Output = MaybeVariable;
	#[inline(always)]
	fn bitor(self, lit: Literal) -> MaybeVariable {
		unsafe { mem::transmute::<u32, MaybeVariable>(mem::transmute::<MaybeVariable, u32>(self).max(mem::transmute::<Literal, u32>(lit) & !1u32)) }
	}
}
impl BitOrAssign<MaybeVariable> for MaybeVariable {
	#[inline(always)]
	fn bitor_assign(&mut self, var: MaybeVariable) {
		*self = unsafe { mem::transmute::<u32, MaybeVariable>(mem::transmute::<MaybeVariable, u32>(*self).max(mem::transmute::<MaybeVariable, u32>(var))) }
	}
}
impl BitOrAssign<Variable> for MaybeVariable {
	#[inline(always)]
	fn bitor_assign(&mut self, var: Variable) {
		*self = unsafe { mem::transmute::<u32, MaybeVariable>(mem::transmute::<MaybeVariable, u32>(*self).max(mem::transmute::<Variable, u32>(var))) }
	}
}
impl BitOrAssign<Literal> for MaybeVariable {
	#[inline(always)]
	fn bitor_assign(&mut self, lit: Literal) {
		*self = unsafe { mem::transmute::<u32, MaybeVariable>(mem::transmute::<MaybeVariable, u32>(*self).max(mem::transmute::<Literal, u32>(lit) & !1u32)) }
	}
}
impl Display for MaybeVariable {
	fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
		unsafe { write!(f, "{}", mem::transmute::<&MaybeVariable, &u32>(self) >> 1) }
	}
}

pub struct TextMaybeVariable<'a>(&'a MaybeVariable);
impl<'a> Display for TextMaybeVariable<'a> {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		match self.0.val {
			Some(var) => write!(f, "{}", var),
			None => write!(f, "0"),
		}
	}
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct ClauseIndex {
	val: NonZeroU64,
}
impl ClauseIndex {
	pub const MaxValue: i64 = i64::max_value() as i64;
	#[inline(always)]
	pub fn index(&self) -> usize {
		(self.val.get() as usize) - 1usize
	}
	pub fn next(self) -> ClauseIndex {
		ClauseIndex { val: unsafe { NonZeroU64::new_unchecked(self.val.get() + 1u64) } }
	}
	#[inline(always)]
	pub fn text(&self) -> TextClauseIndex<'_> {
		TextClauseIndex(self)
	}
}
impl TryFrom<i64> for ClauseIndex {
	type Error = i64;
	fn try_from(num: i64) -> Result<ClauseIndex, i64> {
		if num > 0i64 && num <= ClauseIndex::MaxValue {
			unsafe { Ok(ClauseIndex { val: NonZeroU64::new_unchecked(num as u64) }) }
		} else {
			Err(num)
		}
	}
}
impl Debug for ClauseIndex {
	fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
		write!(f, "#{}", self.val.get())
	}
}
impl Display for ClauseIndex {
	fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
		write!(f, "#{}", self.val.get())
	}
}

pub struct TextClauseIndex<'a>(&'a ClauseIndex);
impl<'a> Display for TextClauseIndex<'a> {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		write!(f, "{}", self.0.val)
	}
}

// pub struct ClauseIndexEnumerator {
// 	val: u64,
// }
// impl ClauseIndexEnumerator {
// 	fn new() -> ClauseIndexEnumerator {
// 		ClauseIndexEnumerator { val: 0u64 }
// 	}
// }
// impl Iterator for ClauseIndexEnumerator {
// 	type Item = ClauseIndexEnumerator;
// 	fn next(&mut self) -> Option<ClauseIndex> {
// 		if self.val <= ClauseIndex::MaxValue as u64 {
// 			self.val += 1u64;
// 			unsafe { ClauseIndex { val: NonZeroU64::new_unchecked(self.val) } }
// 		} else {
// 			None
// 		}
// 	}
// }
// impl From<ClauseIndex> for ClauseIndexEnumerator {
// 	fn from(id: ClauseIndex) -> ClauseIndexEnumerator {
// 		ClauseIndexEnumerator { val: id.val.get() }
// 	}
// }

// #[derive(Copy, Clone, Debug)]
// #[repr(transparent)]
// pub struct MaybeClauseIndex {
//     val: u64,
// }
// impl MaybeClauseIndex {
// 	#[inline(always)]
// 	pub const fn new(val: Option<ClauseIndex>) -> MaybeClauseIndex {
// 		MaybeClauseIndex {
// 			val: unsafe { mem::transmute::<Option<ClauseIndex>, u64>(val) }
// 		}
// 	}
// 	#[inline(always)]
// 	pub const fn get(self) -> Option<ClauseIndex> {
// 		self.val
// 	}
// 	// #[inline(always)]
// 	// pub unsafe fn succ(self) -> MaybeClauseIndex {
// 	// 	mem::transmute::<u32, MaybeClauseIndex>(mem::transmute::<MaybeClauseIndex, u32>(self) + 1u32)
// 	// }
// 	#[inline(always)]
// 	pub fn text(&self) -> TextMaybeClauseIndex<'_> {
// 		TextMaybeClauseIndex(self)
// 	}
// }
// impl TryFrom<i64> for MaybeClauseIndex {
// 	type Error = i64;
// 	fn try_from(num: i64) -> Result<MaybeClauseIndex, i64> {
// 		if num >= 0i64 && num <= ClauseIndex::MaxValue {
// 			unsafe { Ok(mem::transmute::<u64, MaybeClauseIndex>(num as u64)) }
// 		} else {
// 			Err(num)
// 		}
// 	}
// }
// impl BitOr<MaybeClauseIndex> for MaybeClauseIndex {
// 	type Output = MaybeClauseIndex;
// 	fn bitor(self, mid: MaybeClauseIndex) -> MaybeClauseIndex {
// 		unsafe { mem::transmute::<u64, MaybeClauseIndex>(mem::transmute::<MaybeClauseIndex, u64>(self) | mem::transmute::<MaybeClauseIndex, u64>(mid)) }
// 	}
// }
// impl BitOr<ClauseIndex> for MaybeClauseIndex {
// 	type Output = MaybeClauseIndex;
// 	fn bitor(self, id: ClauseIndex) -> MaybeClauseIndex {
// 		unsafe { mem::transmute::<u64, MaybeClauseIndex>(mem::transmute::<MaybeClauseIndex, u64>(self) | mem::transmute::<ClauseIndex, u64>(id)) }
// 	}
// }
// impl Display for MaybeClauseIndex {
// 	fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
// 		write!(f, "{}", unsafe { mem::transmute::<&MaybeClauseIndex, &u64>(self) })
// 	}
// }

// pub struct TextMaybeClauseIndex<'a>(&'a MaybeClauseIndex);
// impl<'a> Display for TextMaybeClauseIndex<'a> {
// 	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
// 		match self.0.val {
// 			Some(id) => write!(f, "{}", id.text()),
// 			None => write!(f, "0"),
// 		}
// 	}
// }

// #[derive(Copy, Clone, PartialEq, Eq)]
// pub enum InstructionNumberKind {
// 	Premise,
// 	Core,
// 	Proof,
// }
// impl InstructionNumberKind {
// 	fn section(&self) -> &str {
// 		match self {
// 			InstructionNumberKind::Premise => "premise",
// 			InstructionNumberKind::Core => "core",
// 			InstructionNumberKind::Proof => "proof",
// 		}
// 	}
// }
// impl Display for InstructionNumberKind {
// 	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
// 		match self {
// 			InstructionNumberKind::Premise => write!(f, "premise clause"),
// 			InstructionNumberKind::Core => write!(f, "core clause"),
// 			InstructionNumberKind::Proof => write!(f, "proof instruction"),
// 		}
// 	}
// }

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct InstructionNumber {
	val: u64
}
impl InstructionNumber {
	pub const MaxValue: i64 = (u64::max_value() >> 2) as i64;
	pub const PremiseKind: u64 = 0b00u64 << 62;
	pub const CoreKind: u64 = 0b01u64 << 62;
	pub const BufferKind: u64 = 0b10u64 << 62;
	pub const ProofKind: u64 = 0b11u64 << 62;
	pub const NumberMask: u64 = !InstructionNumber::ProofKind;
	pub fn new(kind: u64, n: i64) -> Result<InstructionNumber, i64> {
		if n > 0i64 && n < InstructionNumber::MaxValue {
			Ok(InstructionNumber{ val: (kind & !InstructionNumber::NumberMask) | (n as u64) })
		} else {
			Err(n)
		}
	}
	pub fn new_premise() -> InstructionNumber {
		InstructionNumber { val: InstructionNumber::PremiseKind }
	}
	pub fn new_core() -> InstructionNumber {
		InstructionNumber { val: InstructionNumber::CoreKind }
	}
	pub fn new_buffer() -> InstructionNumber {
		InstructionNumber { val: InstructionNumber::BufferKind }
	}
	pub fn new_proof() -> InstructionNumber {
		InstructionNumber { val: InstructionNumber::ProofKind }
	}
	pub fn next(&mut self) {
		self.val += 0b1u64;
	}
	pub fn increase(&mut self, inc: u64) {
		self.val += inc & InstructionNumber::NumberMask
	}
	// pub fn text(&self) -> TextInstructionNumber<'_> {
	// 	TextInstructionNumber::<'_>(self)
	// }
}
impl Display for InstructionNumber {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		let kind = match self.val & !InstructionNumber::NumberMask {
			InstructionNumber::ProofKind => "proof instruction",
			InstructionNumber::CoreKind => "core clause",
			InstructionNumber::PremiseKind => "premise clause",
			InstructionNumber::BufferKind => "buffered proof instruction",
			_ => "???",
		};
		write!(f, "{} {}", kind, InstructionNumber::NumberMask & self.val)
	}
}

// pub struct TextInstructionNumber<'a>(&'a InstructionNumber);
// impl<'a> Display for TextInstructionNumber<'a> {
// 	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
// 		let kind = match self.0.val & 0b11u64 {
// 			InstructionNumber::ProofKind => 'i',
// 			InstructionNumber::CoreKind => 'k',
// 			InstructionNumber::PremiseKind => 'o',
// 			_ => return Ok(()),
// 		};
// 		write!(f, "{} {}", kind, self.0.val >> 2)
// 	}
// }



// #[derive(Copy, Clone, PartialEq, Eq, Debug)]
// #[repr(transparent)]
// pub struct InstructionNumber {
//     val: u64,
// }
// impl InstructionNumber {
// 	pub const MaxValue: i64 = !InstructionNumber::Flags as i64;
// 	const ProofFlag: u64 = 0b10u64 << 62;
// 	const CoreFlag: u64 = 0b01u64 << 62;
// 	const Flags: u64 = InstructionNumber::ProofFlag | InstructionNumber::CoreFlag;
// 	pub fn new(kind: InstructionNumberKind) -> InstructionNumber {
// 		InstructionNumber { val: match kind {
// 			InstructionNumberKind::Premise => 0b00u64,
// 			InstructionNumberKind::Core => 0b01u64,
// 			InstructionNumberKind::Proof => 0b11u64,
// 		} << 62 }
// 	}
// 	pub fn kind(&self) -> InstructionNumberKind {
// 		if self.val >= InstructionNumber::Flags {
// 			InstructionNumberKind::Proof
// 		} else if self.val >= InstructionNumber::CoreFlag {
// 			InstructionNumberKind::Core
// 		}  else {
// 			InstructionNumberKind::Premise
// 		}
// 	}
// 	pub fn succ(self) -> Option<InstructionNumber> {
// 		if self.val & !InstructionNumber::Flags != !InstructionNumber::Flags {
// 			Some(InstructionNumber { val: self.val + 1u64 })
// 		} else {
// 			None
// 		}
// 	}
// 	pub fn number(&self) -> u64 {
// 		self.val & !InstructionNumber::Flags
// 	}
// 	#[inline(always)]
// 	pub fn text(&self) -> TextInstructionNumber<'_> {
// 		TextInstructionNumber(self)
// 	}
// 	pub fn is_break(&self, div: NonZeroU64) -> bool {
// 		self.val & InstructionNumber::Flags == InstructionNumber::Flags && (self.val & !InstructionNumber::Flags) % div.get() == 0u64
// 	}
// }
// impl Display for InstructionNumber {
// 	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
// 		if self.val >= InstructionNumber::ProofFlag {
// 			write!(f, "proof instruction #{}", self.val & !InstructionNumber::Flags)
// 		} else if self.val >= InstructionNumber::CoreFlag {
// 			write!(f, "core clause #{}", self.val & !InstructionNumber::Flags)
// 		} else {
// 			write!(f, "premise clause #{}", self.val & !InstructionNumber::Flags)
// 		}
// 	}
// }

// impl TryFrom<(InstructionNumberKind, i64)> for InstructionNumber {
// 	type Error = i64;
// 	fn try_from(pair: (InstructionNumberKind, i64)) -> Result<InstructionNumber, i64> {
// 		if pair.1 > 0i64 && pair.1 <= InstructionNumber::MaxValue {
// 			Ok(InstructionNumber { val: pair.1 as u64 | match pair.0 {
// 				InstructionNumberKind::Premise => 0u64,
// 				InstructionNumberKind::Core => InstructionNumber::CoreFlag,
// 				InstructionNumberKind::Proof => InstructionNumber::Flags,
// 			} })
// 		} else {
// 			Err(pair.1)
// 		}
// 	}
// }
// impl TryFrom<(InstructionNumberKind, u64)> for InstructionNumber {
// 	type Error = u64;
// 	fn try_from(pair: (InstructionNumberKind, u64)) -> Result<InstructionNumber, u64> {
// 		if pair.1 > 0u64 && pair.1 <= InstructionNumber::MaxValue as u64 {
// 			Ok(InstructionNumber { val: pair.1 | match pair.0 {
// 				InstructionNumberKind::Premise => 0u64,
// 				InstructionNumberKind::Core => InstructionNumber::CoreFlag,
// 				InstructionNumberKind::Proof => InstructionNumber::Flags,
// 			} })
// 		} else {
// 			Err(pair.1)
// 		}
// 	}
// }
// impl PartialOrd for InstructionNumber {
// 	fn partial_cmp(&self, other: &InstructionNumber) -> Option<Ordering> {
// 		self.val.partial_cmp(&other.val)
// 	}
// }
// impl Ord for InstructionNumber {
// 	fn cmp(&self, other: &InstructionNumber) -> Ordering {
// 		self.val.cmp(&other.val)
// 	}
// }

// pub struct TextInstructionNumber<'a>(&'a InstructionNumber);
// impl<'a> Display for TextInstructionNumber<'a> {
// 	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
// 		if self.0.val >= InstructionNumber::ProofFlag {
// 			write!(f, "i {}", self.0.val & !InstructionNumber::Flags)
// 		} else if self.0.val >= InstructionNumber::CoreFlag {
// 			write!(f, "k {}", self.0.val & !InstructionNumber::Flags)
// 		} else {
// 			write!(f, "o {}", self.0.val & !InstructionNumber::Flags)
// 		}
// 	}
// }


#[cfg(test)]
pub mod test {
	use std::{
		convert::{TryFrom},
		num::{NonZeroU32, NonZeroU64},
	};
	use rand::{self, Rng};
	use crate::{
		basic::{Variable, Literal, ClauseIndex},
	};

	pub fn generate_literal<R: Rng>(rng: &mut R, limit: Option<u32>) -> Literal {
		Literal { val: match limit {
			None => rng.gen(),
			Some(x) => rng.gen_range(0u32, x),
		} }
	}
	pub fn generate_variable<R: Rng>(rng: &mut R) -> Variable {
		loop {
			let num: u32 = rng.gen();
			if num >= 2u32 {
				break Variable { val: unsafe { NonZeroU32::new_unchecked(num & !1u32) } }
			}
		}
	}
	pub fn generate_external_literal<R: Rng>(rng: &mut R, exclude: &[Literal], limit: Option<u32>) -> Literal {
		loop {
			let lit = generate_literal(rng, limit);
			if !exclude.contains(&lit) {
				break lit;
			}
		}
	}
	pub fn generate_external_variable<R: Rng>(rng: &mut R, exclude: &[Variable]) -> Variable {
		loop {
			let var = generate_variable(rng);
			if !exclude.contains(&var) {
				break var;
			}
		}
	}
	pub fn generate_index<R: Rng>(rng: &mut R, limit: Option<u64>) -> ClauseIndex {
		ClauseIndex { val: match limit {
			None => loop {
				let n: u64 = rng.gen_range(1u64, ClauseIndex::MaxValue as u64 + 1u64);
				if n != 0u64 {
					break unsafe { NonZeroU64::new_unchecked(n) };
				}
			},
			Some(x) => unsafe { NonZeroU64::new_unchecked(rng.gen_range(1u64, x)) },
		} }
	}
	pub fn generate_external_index<R: Rng>(rng: &mut R, exclude: &[ClauseIndex], limit: Option<u64>) -> ClauseIndex {
		loop {
			let id = generate_index(rng, limit);
			if !exclude.contains(&id) {
				break id;
			}
		}
	}


	#[test]
	fn test_literals() {
		let mut rng = rand::thread_rng();
		let number = rng.gen_range(1i64, Literal::MaxValue + 1i64);
		let lit1 = Literal::Top;
		let lit2 = Literal::Bottom;
		let lit3 = Literal::try_from(number).unwrap();
		let lit4 = Literal::try_from(-number).unwrap();
		let lit5 = Literal::try_from(Literal::MaxValue).unwrap();
		let lit6 = Literal::try_from(-Literal::MaxValue).unwrap();
		assert!(Literal::try_from(0i64).is_err());
		assert!(Literal::try_from(Literal::MaxValue + 1i64).is_err());
		assert!(lit1.index() == 0usize);
		assert!(lit2.index() == 1usize);
		assert!(lit3.index() == (number as usize) * 2usize);
		assert!(lit4.index() == (number as usize) * 2usize + 1usize);
		assert!(lit5.index() == u32::max_value() as usize - 1usize);
		assert!(lit6.index() == u32::max_value() as usize);
		assert!(lit1.complement() == lit2);
		assert!(lit2.complement() == lit1);
		assert!(lit3.complement() == lit4);
		assert!(lit4.complement() == lit3);
		assert!(lit5.complement() == lit6);
		assert!(lit6.complement() == lit5);
		let mvar1 = lit1.variable();
		let mvar2 = lit2.variable();
		let mvar3 = lit3.variable();
		let mvar4 = lit4.variable();
		let mvar5 = lit5.variable();
		let mvar6 = lit6.variable();
		assert!(mvar1.get().is_none());
		assert!(mvar2.get().is_none());
		assert!(mvar3.get().is_some());
		assert!(mvar4.get().is_some());
		assert!(mvar5.get().is_some());
		assert!(mvar6.get().is_some());
		assert!(mvar1 == mvar2);
		assert!(mvar3 == mvar4);
		assert!(mvar5 == mvar6);
		let var3 = mvar3.get().unwrap();
		let var4 = mvar4.get().unwrap();
		let var5 = mvar5.get().unwrap();
		let var6 = mvar6.get().unwrap();
		assert!(mvar1 | var3 == mvar3);
		assert!(mvar1 | var5 == mvar5);
		assert!(mvar3 | var5 == mvar5);
		assert!(mvar1 | lit1 == mvar1);
		assert!(mvar1 | lit3 == mvar3);
		assert!(mvar1 | lit5 == mvar5);
		assert!(mvar3 | mvar3 == mvar3);
		assert!(mvar3 | mvar5 == mvar5);
		assert!(mvar5 | mvar5 == mvar5);
		assert!(var3 < var5);
		assert!(var3.index() == (number - 1i64) as usize);
		assert!(var4.index() == (number - 1i64) as usize);
		assert!(var5.index() == ((u32::max_value() as usize) >> 1usize) - 1usize);
		assert!(var6.index() == ((u32::max_value() as usize) >> 1usize) - 1usize);
	}

	#[test]
	fn test_clause_indices() {
		let mut rng = rand::thread_rng();
		let number = rng.gen_range(1i64, ClauseIndex::MaxValue);
		let id2 = ClauseIndex::try_from(number).unwrap();
		let id3 = ClauseIndex::try_from(ClauseIndex::MaxValue).unwrap();
		assert!(id2.index() == (number as usize) - 1usize);
		assert!(id3.index() == (ClauseIndex::MaxValue as usize) - 1usize);
	}
}