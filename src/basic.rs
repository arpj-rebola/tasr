use std::{
	fmt::{Result as FmtResult, Debug, Display, Formatter},
	convert::{TryFrom},
	cmp::{Ordering},
	ops::{BitOr},
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
		if self.val > 1u32 {
			if self.val & 1u32 == 0u32 {
				write!(f, "+{}", self.val >> 1)
			} else {
				write!(f, "-{}", self.val >> 1)
			}
		} else {
			if self.val == 0u32 {
				write!(f, "t")
			} else {
				write!(f, "f")
			}
		}
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
				write!(f, "t")
			} else {
				write!(f, "f")
			}
		}
	}
}

// /// Display structure for literals in the textual ASR format.
// pub struct TextAsrLiteral<'a>(pub &'a Literal);
// impl<'a> Display for TextAsrLiteral<'a> {
//     fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
// 		if self.0.val > 1u32 {
// 			if self.0.val & 1u32 == 0u32 {
// 				write!(f, "{}", self.0.val >> 1)
// 			} else {
// 				write!(f, "-{}", self.0.val >> 1)
// 			}
// 		} else {
// 			if self.0.val == 0u32 {
// 				write!(f, "t")
// 			} else {
// 				write!(f, "f")
// 			}
// 		}
//     }
// }

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
// impl PartialOrd for Variable {
// 	#[inline(always)]
//     fn partial_cmp(&self, oth: &Variable) -> Option<Ordering> {
//         Some(self.cmp(oth))
//     }
// }
// impl Ord for Variable {
// 	#[inline(always)]
//     fn cmp(&self, oth: &Variable) -> Ordering {
// 		Some(self.val.cmp(oth.val))
//     }
// }
impl BitOr for Variable {
	type Output = Variable;
	#[inline(always)]
	fn bitor(self, var: Variable) -> Variable {
		Variable { val: self.val.max(var.val) }
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

// pub struct TextAsrVariable<'a>(pub &'a Variable);
// impl<'a> Display for TextAsrVariable<'a> {
//     fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
// 		write!(f, "{}", self.0.val.get() >> 1)
//     }
// }

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
// 	pub fn unwrap(self) -> Variable {
// 		if self.val > 1u32 {
// 			unsafe { Variable::new(self.val) }
// 		} else {
// 			panic!("tried to unwrap None value for MaybeVariable type");
// 		}
// 	}
// 	#[inline(always)]
// 	pub fn is_variable(self) -> bool {
// 		self.val > 1u32
// 	}
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
// impl PartialEq for MaybeVariable {
// 	#[inline(always)]
// 	fn eq(&self, oth: &MaybeVariable) -> bool {
// 		self.val | 1u32 == oth.val | 1u32
// 	}
// }
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
		unsafe { mem::transmute::<u32, MaybeVariable>(mem::transmute::<MaybeVariable, u32>(self) | mem::transmute::<MaybeVariable, u32>(var)) }
	}
}
impl BitOr<Variable> for MaybeVariable {
	type Output = MaybeVariable;
	#[inline(always)]
	fn bitor(self, var: Variable) -> MaybeVariable {
		unsafe { mem::transmute::<u32, MaybeVariable>(mem::transmute::<MaybeVariable, u32>(self) | mem::transmute::<Variable, u32>(var)) }
	}
}
impl BitOr<Literal> for MaybeVariable {
	type Output = MaybeVariable;
	#[inline(always)]
	fn bitor(self, lit: Literal) -> MaybeVariable {
		unsafe { mem::transmute::<u32, MaybeVariable>(mem::transmute::<MaybeVariable, u32>(self) | (mem::transmute::<Literal, u32>(lit) & !1u32)) }
	}
}
impl Display for MaybeVariable {
	fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
		unsafe { write!(f, "{}", mem::transmute::<&MaybeVariable, &u32>(self) >> 1) }
	}
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[repr(transparent)]
pub struct ClauseIndex {
	val: NonZeroU32,
}
impl ClauseIndex {
	pub const MaxValue: i64 = u32::max_value() as i64;
	// pub const MaxIndex: usize = (u32::max_value() - 1u32) as usize;
	// pub const MinElem: ClauseIndex = ClauseIndex { val: 1u32 };
	// #[inline(always)]
	// const unsafe fn new(val: u32) -> ClauseIndex {
	// 	ClauseIndex { val: val }
	// }
	#[inline(always)]
	pub fn index(&self) -> usize {
		(self.val.get() as usize) - 1usize
	}
}
impl TryFrom<i64> for ClauseIndex {
	type Error = i64;
	fn try_from(num: i64) -> Result<ClauseIndex, i64> {
		if num > 0i64 && num <= ClauseIndex::MaxValue {
			unsafe { Ok(ClauseIndex { val: NonZeroU32::new_unchecked(num as u32) }) }
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

// pub struct TextAsrClauseIndex<'a>(pub &'a ClauseIndex);
// impl<'a> Display for TextAsrClauseIndex<'a> {
//     fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
// 		write!(f, "{}", self.0.val.get())
//     }
// }

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[repr(transparent)]
pub struct MaybeClauseIndex {
    val: Option<ClauseIndex>,
}
impl MaybeClauseIndex {
	#[inline(always)]
	pub const fn new(val: Option<ClauseIndex>) -> MaybeClauseIndex {
		MaybeClauseIndex { val: val }
	}
	#[inline(always)]
	pub const fn get(self) -> Option<ClauseIndex> {
		self.val
	}
	#[inline(always)]
	pub unsafe fn succ(self) -> MaybeClauseIndex {
		mem::transmute::<u32, MaybeClauseIndex>(mem::transmute::<MaybeClauseIndex, u32>(self) + 1u32)
	}
}
impl TryFrom<i64> for MaybeClauseIndex {
	type Error = i64;
	fn try_from(num: i64) -> Result<MaybeClauseIndex, i64> {
		if num >= 0i64 && num <= ClauseIndex::MaxValue {
			unsafe { Ok(mem::transmute::<u32, MaybeClauseIndex>(num as u32)) }
		} else {
			Err(num)
		}
	}
}
impl BitOr<MaybeClauseIndex> for MaybeClauseIndex {
	type Output = MaybeClauseIndex;
	fn bitor(self, mid: MaybeClauseIndex) -> MaybeClauseIndex {
		unsafe { mem::transmute::<u32, MaybeClauseIndex>(mem::transmute::<MaybeClauseIndex, u32>(self) | mem::transmute::<MaybeClauseIndex, u32>(mid)) }
	}
}
impl BitOr<ClauseIndex> for MaybeClauseIndex {
	type Output = MaybeClauseIndex;
	fn bitor(self, id: ClauseIndex) -> MaybeClauseIndex {
		unsafe { mem::transmute::<u32, MaybeClauseIndex>(mem::transmute::<MaybeClauseIndex, u32>(self) | mem::transmute::<ClauseIndex, u32>(id)) }
	}
}
impl Display for MaybeClauseIndex {
	fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
		write!(f, "{}", unsafe { mem::transmute::<&MaybeClauseIndex, &u32>(self) })
	}
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum InstructionNumberKind {
	Core,
	Proof,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[repr(transparent)]
pub struct InstructionNumber {
    val: u64,
}
impl InstructionNumber {
	pub const MaxValue: i64 = (InstructionNumber::Flag - 1u64) as i64;
	const Flag: u64 = 1u64 << 63;
	pub fn new(kind: InstructionNumberKind) -> InstructionNumber {
		InstructionNumber { val: match kind {
			InstructionNumberKind::Core => 0u64,
			InstructionNumberKind::Proof => InstructionNumber::Flag,
		} }
	}
	pub fn kind(&self) -> Option<InstructionNumberKind> {
		if self.val & !InstructionNumber::Flag == 0u64 {
			None
		} else if self.val & InstructionNumber::Flag == 0u64 {
			Some(InstructionNumberKind::Core)
		} else {
			Some(InstructionNumberKind::Proof)
		}
	}
	pub fn offset(&self) -> Option<NonZeroU64> {
		NonZeroU64::new(self.val & !InstructionNumber::Flag)
	}
	pub fn insert(&mut self, num: u64) {
		if self.val & !InstructionNumber::Flag == 0u64 {
			self.val |= num & !InstructionNumber::Flag
		}
	}
}
impl TryFrom<(i64, InstructionNumberKind)> for InstructionNumber {
	type Error = i64;
	fn try_from(pair: (i64, InstructionNumberKind)) -> Result<InstructionNumber, i64> {
		if pair.0 > 0i64 {
			Ok(InstructionNumber { val: pair.0 as u64 | match pair.1 {
				InstructionNumberKind::Core => 0u64,
				InstructionNumberKind::Proof => InstructionNumber::Flag,
			} })
		} else {
			Err(pair.0)
		}
	}
}


#[cfg(test)]
pub mod test {
	use std::{
		convert::{TryFrom},
		num::{NonZeroU32},
	};
	use rand::{self, Rng};
	use crate::{
		basic::{Variable, Literal, ClauseIndex, MaybeClauseIndex},
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
	pub fn generate_index<R: Rng>(rng: &mut R, limit: Option<u32>) -> ClauseIndex {
		ClauseIndex { val: match limit {
			None => loop {
				let n = rng.gen();
				if n != 0u32 {
					break unsafe { NonZeroU32::new_unchecked(n) };
				}
			},
			Some(x) => unsafe { NonZeroU32::new_unchecked(rng.gen_range(1u32, x)) },
		} }
	}
	pub fn generate_external_index<R: Rng>(rng: &mut R, exclude: &[ClauseIndex], limit: Option<u32>) -> ClauseIndex {
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
		let number = rng.gen_range(1i64, ClauseIndex::MaxValue + 1i64);
		let id1 = unsafe { MaybeClauseIndex::new(None).succ().get().unwrap() };
		let id2 = ClauseIndex::try_from(number).unwrap();
		let id3 = ClauseIndex::try_from(ClauseIndex::MaxValue).unwrap();
		assert!(id1.index() == 0usize);
		assert!(id2.index() == (number as usize) - 1usize);
		assert!(id3.index() == (ClauseIndex::MaxValue as usize) - 1usize);
		assert!(unsafe { MaybeClauseIndex::new(Some(id1)).succ() } == MaybeClauseIndex::try_from(2i64).unwrap());
	}
}