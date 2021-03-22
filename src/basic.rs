use std::{
	fmt::{Result as FmtResult, Debug, Display, Formatter},
	convert::{TryFrom},
	ops::{BitOr, BitOrAssign},
	num::{NonZeroU32, NonZeroU64},
	io::{Write as IoWrite, Result as IoResult},
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
	// /// Returns a `MaybeVariable` containing the variable underlying this literal, or `None` if the literal is top or bottom.
	#[inline(always)]
	pub fn variable(self) -> Option<Variable> {
		unsafe { mem::transmute::<u32, Option<Variable>>(self.val & !1u32) }
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
	#[inline]
	pub fn binary<W: IoWrite>(&self, w: &mut W) -> IoResult<()> {
		let mut num: u32 = self.val;
		if num >= 1u32 {
			loop {
				let c = (num & 0b0111_1111u32) as u8;
				num >>= 7;
				let cont = num != 0u32;
				w.write_all(&[c | ((cont as u8) * 0b1000_0000u8)])?;
				if !cont {
					return Ok(())
				}
			}
		} else {
			let letter = if num == 0u32 {
				b'f'
			} else {
				b't'
			};
			w.write_all(&[0x01, letter])
		}
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
	#[inline]
	pub fn binary<W: IoWrite>(&self, w: &mut W) -> IoResult<()> {
		self.positive().binary(w)
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
	#[inline]
	pub fn binary<W: IoWrite>(&self, w: &mut W) -> IoResult<()> {
		let mut num: u64 = self.val.get() << 1;
		loop {
			let c = (num & 0b0111_1111u64) as u8;
			num >>= 7;
			let cont = num != 0u64;
			w.write_all(&[c | ((cont as u8) * 0b1000_0000u8)])?;
			if !cont {
				return Ok(())
			}
		}
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
	#[inline]
	pub fn new_premise() -> InstructionNumber {
		InstructionNumber { val: InstructionNumber::PremiseKind }
	}
	#[inline]
	pub fn new_core() -> InstructionNumber {
		InstructionNumber { val: InstructionNumber::CoreKind }
	}
	#[inline]
	pub fn new_buffer() -> InstructionNumber {
		InstructionNumber { val: InstructionNumber::BufferKind }
	}
	#[inline]
	pub fn new_proof() -> InstructionNumber {
		InstructionNumber { val: InstructionNumber::ProofKind }
	}
	#[inline]
	pub fn next(&mut self) {
		self.val += 0b1u64;
	}
	#[inline]
	pub fn increase(&mut self, inc: u64) {
		self.val += inc & InstructionNumber::NumberMask
	}
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
		assert!(mvar1 == mvar2);
		assert!(mvar3 == mvar4);
		assert!(mvar5 == mvar6);
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