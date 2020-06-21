use std::{
	cmp::{Ordering},
	convert::{TryFrom},
	fmt::{self, Formatter, Debug, Display},
	ops::{BitOr, BitOrAssign},
};
use rand::{Rng,
	distributions::{Uniform},
};
use crate::{
	hasher::{Hashable32, Hasher32},
};

#[derive(Copy, Clone, Eq)]
#[repr(transparent)]
pub struct Variable {
    val: u32,
}
impl Variable {
	pub const MaxValue: i64 = (u32::max_value() as i64) >> 1;
	pub const MaxIndex: usize = ((u32::max_value() >> 1) - 1u32) as usize;
	pub const MaxVariable: Variable = Variable { val: u32::max_value() };
	pub const MinVariable: Variable = Variable { val: 2u32 };
    pub(crate) const fn new(val: u32) -> Variable {
        Variable { val: val }
	}
	pub fn random<R: Rng + ?Sized>(rng: &mut R, lo: Variable, hi: Variable) -> Variable {
		Variable { val: rng.sample(Uniform::new_inclusive(lo.val, hi.val)) }
	}
	pub fn positive(&self) -> Literal {
		Literal::new(self.val & !1u32)
	}
	pub fn negative(&self) -> Literal {
		Literal::new(self.val | 1u32)
	}
	pub fn index(&self) -> usize {
		((self.val >> 1) - 1u32) as usize
	}
	pub const fn next(&self) -> Variable {
		Variable::new(self.val + 2u32)
	}
}
impl TryFrom<i64> for Variable {
	type Error = i64;
	fn try_from(num: i64) -> Result<Variable, i64> {
		if num > 0i64 && num <= Self::MaxValue {
			Ok(Variable::new((num as u32) << 1))
		} else if num < 0i64 && num >= -Self::MaxValue {
			Ok(Variable::new((-num as u32) << 1))
		} else {
			Err(num)
		}
	}
}
impl TryFrom<u64> for Variable {
	type Error = u64;
	fn try_from(num: u64) -> Result<Variable, u64> {
		if num <= u32::max_value() as u64 && num > 1u64 {
			Ok(Variable::new(num as u32))
		} else {
			Err(num)
		}
	}
}
impl PartialEq for Variable {
	fn eq(&self, oth: &Variable) -> bool {
		self.val | 1u32 == oth.val | 1u32
	}
}
impl PartialOrd for Variable {
    fn partial_cmp(&self, oth: &Variable) -> Option<Ordering> {
        Some(self.cmp(oth))
    }
}
impl Ord for Variable {
    fn cmp(&self, oth: &Variable) -> Ordering {
        (self.val | 1u32).cmp(&(oth.val | 1u32))
    }
}
impl Hashable32 for Variable {
	fn hash<H: Hasher32>(&self, hs: &mut H) {
		(self.val | 1u32).hash::<H>(hs)
	}
}
impl BitOr for Variable {
	type Output = Variable;
	fn bitor(self, var: Variable) -> Variable {
		Variable::new(self.val.max(var.val))
	}
}
impl BitOrAssign for Variable {
	fn bitor_assign(&mut self, var: Variable) {
		self.val = self.val.max(var.val)
	}
}
impl Debug for Variable {
	fn fmt(&self , f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.val >> 1usize)
	}
}
impl Display for Variable {
	fn fmt(&self , f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.val >> 1usize)
	}
}

#[derive(Copy, Clone)]
pub struct MaybeVariable {
	val: u32
}
impl MaybeVariable {
	pub const None: MaybeVariable = MaybeVariable { val: 0u32 };
}
impl From<Variable> for MaybeVariable {
	fn from(var: Variable) -> MaybeVariable {
		MaybeVariable { val: var.val }
	}
}
impl TryFrom<i64> for MaybeVariable {
	type Error = i64;
	fn try_from(num: i64) -> Result<MaybeVariable, i64> {
		if num > 0i64 && num <= Variable::MaxValue {
			Ok(MaybeVariable { val: (num as u32) << 1 })
		} else if num == 0i64 {
			Ok(MaybeVariable::None)
		} else {
			Err(num)
		}
	}
}
impl TryFrom<u64> for MaybeVariable {
	type Error = u64;
	fn try_from(num: u64) -> Result<MaybeVariable, u64> {
		if num > 0u64 && num <= Variable::MaxValue as u64 {
			Ok(MaybeVariable { val: (num as u32) << 1 })
		} else if num == 0u64 {
			Ok(MaybeVariable::None)
		} else {
			Err(num)
		}
	}
}
impl BitOr<Variable> for MaybeVariable {
	type Output = MaybeVariable;
	fn bitor(self, var: Variable) -> MaybeVariable {
		MaybeVariable { val: u32::max(self.val, var.val) }
	}
}
impl BitOrAssign<Variable> for MaybeVariable {
	fn bitor_assign(&mut self, var: Variable) {
		self.val = u32::max(self.val, var.val)
	}
}
impl PartialEq for MaybeVariable {
	fn eq(&self, oth: &MaybeVariable) -> bool {
		self.val | 1u32 == oth.val | 1u32
	}
}
impl Eq for MaybeVariable {}
impl PartialOrd for MaybeVariable {
	fn partial_cmp(&self, oth: &MaybeVariable) -> Option<Ordering> {
		Some(self.cmp(oth))
	}
}
impl Ord for MaybeVariable {
	fn cmp(&self, oth: &MaybeVariable) -> Ordering {
		(self.val | 1u32).cmp(&(oth.val | 1u32))
	}
}
impl Debug for MaybeVariable {
	fn fmt(&self , f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.val >> 1usize)
	}
}
impl Display for MaybeVariable {
	fn fmt(&self , f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.val >> 1)
	}
}

#[derive(Copy, Clone, PartialEq, Eq)]
#[repr(transparent)]
pub struct Literal {
    val: u32,
}
impl Literal {
	pub const MaxValue: i64 = (u32::max_value() as i64) >> 1;
	pub const Top: Literal = Literal::new(0u32);
	pub const Bottom: Literal = Literal::new(1u32);
	pub(crate) const fn new(val: u32) -> Literal {
		Literal { val: val }
	}
	pub fn random<R: Rng + ?Sized>(rng: &mut R, lo: Option<Variable>, hi: Variable) -> Literal {
		let low = match lo {
			Some(var) => var.val & !1u32,
			None => 0u32,
		};
		let high = hi.val | 1u32;
		Literal { val: rng.sample(Uniform::new_inclusive(low, high)) }
	}
	#[inline]
	pub const fn positive(&self) -> bool {
		self.val & 1u32 == 0u32
	}
	#[inline]
	pub const fn negative(&self) -> bool {
		!self.positive()
	}
    pub const fn complement(&self) -> Literal {
        Literal::new(self.val ^ 1u32)
	}
	pub const fn proper(&self) -> bool {
		self.val >> 1usize != 0u32
	}
	pub fn mapping(&self) -> Option<(Variable, Literal)> {
		if self.proper() {
			unsafe { Some(self.mapping_unchecked()) }
		} else {
			None
		}
	}
	pub unsafe fn mapping_unchecked(&self) -> (Variable, Literal) {
		let atom = if self.positive() {
			Literal::Top
		} else {
			Literal::Bottom
		};
		(self.variable_unchecked(), atom)
	}
	pub fn split(&self) -> Option<(Variable, bool)> {
		if self.proper() {
			unsafe { Some((self.variable_unchecked(), self.positive())) }
		} else {
			None
		}
	}
	pub fn variable(&self) -> Option<Variable> {
		if self.proper() {
			unsafe { Some(self.variable_unchecked()) }
		} else {
			None
		}
	}
	pub const unsafe fn variable_unchecked(&self) -> Variable {
		Variable::new(self.val)
	}
	pub const fn index(&self) -> usize {
		self.val as usize
	}
}
impl TryFrom<i64> for Literal {
	type Error = i64;
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
impl TryFrom<u64> for Literal {
	type Error = u64;
	fn try_from(num: u64) -> Result<Literal, u64> {
		if num <= u32::max_value() as u64 && num > 1u64 {
			Ok(Literal::new(num as u32))
		} else {
			Err(num)
		}
	}
}
impl Hashable32 for Literal {
	fn hash<H: Hasher32>(&self, hs: &mut H) {
		self.val.hash::<H>(hs)
	}
}
impl Debug for Literal {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		let pref : &str = if self.val & 1u32 == 1u32 { "-" } else { "+" };
		let num : u32 = self.val >> 1usize ;
		write!(f, "{}{}", pref , num)
	}
}
impl Display for Literal {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		if self.val > 1u32 {
			if self.val & 1u32 == 0u32 {
				write!(f, "{}", self.val >> 1)
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

#[cfg(test)]
mod test {
	use std::{
		convert::{TryFrom},
	};
	use crate::{
		variable::{Literal, Variable},
	};

	#[test]
	fn test_literal() {
		let lit1 = Literal::Top;
		let lit2 = Literal::Bottom;
		let lit3 = Literal { val: 36u32 };
		let lit4 = Literal { val: 37u32 };
		let lit5 = Literal { val: u32::max_value() - 1u32 };
		let lit6 = Literal { val: u32::max_value() };
		assert!(lit1.positive());
		assert!(!lit2.positive());
		assert!(lit3.positive());
		assert!(!lit4.positive());
		assert!(lit5.positive());
		assert!(!lit6.positive());
		assert!(!lit1.negative());
		assert!(lit2.negative());
		assert!(!lit3.negative());
		assert!(lit4.negative());
		assert!(!lit5.negative());
		assert!(lit6.negative());
		assert!(lit1.complement() == lit2);
		assert!(lit2.complement() == lit1);
		assert!(lit3.complement() == lit4);
		assert!(lit4.complement() == lit3);
		assert!(lit5.complement() == lit6);
		assert!(lit6.complement() == lit5);
		assert!(!lit1.proper());
		assert!(!lit2.proper());
		assert!(lit3.proper());
		assert!(lit4.proper());
		assert!(lit5.proper());
		assert!(lit6.proper());
		assert!(lit1.mapping() == None);
		assert!(lit2.mapping() == None);
		assert!(lit3.mapping() == Some((Variable { val: lit3.val }, Literal::Top)));
		assert!(lit4.mapping() == Some((Variable { val: lit3.val }, Literal::Bottom)));
		assert!(lit5.mapping() == Some((Variable { val: lit5.val }, Literal::Top)));
		assert!(lit6.mapping() == Some((Variable { val: lit5.val }, Literal::Bottom)));
		assert!(lit1.variable() == None);
		assert!(lit2.variable() == None);
		assert!(lit3.variable() == Some(Variable { val: lit3.val }));
		assert!(lit4.variable() == Some(Variable { val: lit3.val }));
		assert!(lit5.variable() == Some(Variable { val: lit5.val }));
		assert!(lit6.variable() == Some(Variable { val: lit5.val }));
		assert!(lit1.index() == 0usize);
		assert!(lit2.index() == 1usize);
		assert!(lit3.index() == 36usize);
		assert!(lit4.index() == 37usize);
		assert!(lit5.index() == 0xFFFFFFFEusize);
		assert!(lit6.index() == 0xFFFFFFFFusize);
	}

	#[test]
	fn test_variable() {
		let var1 = Variable { val: 36u32 };
		let var2 = Variable { val: 37u32 };
		let var3 = Variable { val: u32::max_value() - 1u32 };
		let var4 = Variable { val: u32::max_value() };
		assert!(var1.positive() == Literal { val: 36u32 });
		assert!(var2.positive() == Literal { val: 36u32 });
		assert!(var3.positive() == Literal { val: u32::max_value() - 1u32 });
		assert!(var4.positive() == Literal { val: u32::max_value() - 1u32 });
		assert!(var1.negative() == Literal { val: 37u32 });
		assert!(var2.negative() == Literal { val: 37u32 });
		assert!(var3.negative() == Literal { val: u32::max_value()});
		assert!(var4.negative() == Literal { val: u32::max_value()});
		assert!(var1.index() == 17usize);
		assert!(var2.index() == 17usize);
		assert!(var3.index() == 0x7FFFFFFEusize);
		assert!(var4.index() == 0x7FFFFFFEusize);
		assert!(var1 | var2 == var1);
		assert!(var1 | var3 == var4);
	}

	#[test]
	fn test_variable_literal_making() {
		assert!(Variable::try_from(0i64) == Err(0i64));
		assert!(Variable::try_from(1i64) == Ok(Variable { val: 2u32 }));
		assert!(Variable::try_from(-1i64) == Ok(Variable { val: 2u32 }));
		assert!(Variable::try_from(Variable::MaxValue) == Ok(Variable { val: u32::max_value() }));
		assert!(Variable::try_from(-Variable::MaxValue) == Ok(Variable { val: u32::max_value() }));
		assert!(Variable::try_from(Variable::MaxValue + 1i64) == Err(Variable::MaxValue + 1i64));
		assert!(Variable::try_from(-Variable::MaxValue - 1i64) == Err(-Variable::MaxValue - 1i64));
		assert!(Literal::try_from(0i64) == Err(0i64));
		assert!(Literal::try_from(1i64) == Ok(Literal { val: 2u32 }));
		assert!(Literal::try_from(-1i64) == Ok(Literal { val: 3u32 }));
		assert!(Literal::try_from(Literal::MaxValue) == Ok(Literal { val: u32::max_value() - 1u32}));
		assert!(Literal::try_from(-Literal::MaxValue) == Ok(Literal { val: u32::max_value() }));
		assert!(Literal::try_from(Literal::MaxValue + 1i64) == Err(Literal::MaxValue + 1i64));
		assert!(Literal::try_from(-Literal::MaxValue - 1i64) == Err(-Literal::MaxValue - 1i64));
	}
}