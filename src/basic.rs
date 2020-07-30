use std::{
	convert::{TryFrom},
	fmt::{self, Formatter, Display, Binary, Debug},
	ops::{BitOr, BitOrAssign},
	cmp::{Ordering}
};

use crate::{
	variable::{MaybeVariable, Literal, Variable},
};

#[derive(Copy, Clone)]
pub struct OutputInteger32(pub u32);
impl Display for OutputInteger32 {
	fn fmt(&self , f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{}", self.0)
	}
}
impl Binary for OutputInteger32 {
    fn fmt(&self , f: &mut Formatter<'_>) -> fmt::Result {
		let mut val = self.0;
		loop {
			let mut c = (val & 0b0111_1111u32) as u8;
			if val >= 0b1000_0000u32 {
				c |= 0b1000_0000u8;
			}
			val >>= 7;
			write!(f, "{}", c as char)?;
			if val == 0u32 {
				break Ok(());
			}
		}
	}
}

pub struct CnfHeaderStats {
	pub variables: MaybeVariable,
	pub clauses: usize,
}
impl Display for CnfHeaderStats {
	fn fmt(&self , f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "p cnf {} {}", self.variables, self.clauses)
	}
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd)]
#[repr(transparent)]
pub struct ClauseIndex {
	val: u32,
}
impl ClauseIndex {
	pub const MaxValue: i64 = (u32::max_value() as i64) + 1i64;
	pub fn index(&self) -> usize {
		self.val as usize
	}
	pub fn new(val: u32) -> ClauseIndex {
		ClauseIndex { val: val }
	}
}
impl TryFrom<u64> for ClauseIndex {
	type Error = u64;
	fn try_from(num: u64) -> Result<ClauseIndex, u64> {
		if num > 0u64 && num <= ClauseIndex::MaxValue as u64 {
			Ok(ClauseIndex { val: (num - 1u64) as u32 })
		} else {
			Err(num)
		}
	}
}
impl TryFrom<i64> for ClauseIndex {
	type Error = i64;
	fn try_from(num: i64) -> Result<ClauseIndex, i64> {
		if num > 0i64 && num <= ClauseIndex::MaxValue {
			Ok(ClauseIndex { val: (num - 1i64) as u32 })
		} else {
			Err(num)
		}
	}
}
impl BitOr for ClauseIndex {
	type Output = ClauseIndex;
	fn bitor(self, id: ClauseIndex) -> ClauseIndex {
		ClauseIndex { val: self.val.max(id.val) }
	}
}
impl BitOrAssign for ClauseIndex {
	fn bitor_assign(&mut self, id: ClauseIndex) {
		self.val = self.val.max(id.val)
	}
}
impl BitOr<ClauseIndex> for Option<ClauseIndex> {
	type Output = Option<ClauseIndex>;
	fn bitor(self, id: ClauseIndex) -> Option<ClauseIndex> {
		self.map_or_else(|| Some(id), |x| Some(x | id))
	}
}
impl BitOrAssign<ClauseIndex> for Option<ClauseIndex> {
	fn bitor_assign(&mut self, id: ClauseIndex) {
		self.as_mut().map(|x| x.bitor_assign(id));
	}
}
impl Debug for ClauseIndex {
	fn fmt(&self , f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "#{}", self.index() + 1usize)
	}
}
impl Display for ClauseIndex {
	fn fmt(&self , f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "#{}", self.index() + 1usize)
	}
}
impl Ord for ClauseIndex {
	fn cmp(&self, other: &Self) -> Ordering {
		self.val.cmp(&other.val)
	}
}

#[derive(Debug, Clone)]
pub struct ClauseContainer(pub Vec<Literal>);
impl Display for ClauseContainer {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		let mut first = false;
		write!(f, "[")?;
		for lit in &self.0 {
			if first {
				write!(f, ", ")?;
			} else {
				first = true;
			}
			write!(f, "{}", lit)?;
		}
		write!(f, "]")
	}
}

#[derive(Debug)]
pub struct ChainContainer(pub Vec<(ClauseIndex, ClauseContainer)>);
impl Display for ChainContainer {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		if self.0.is_empty() {
			write!(f, "(empty)\n")
		} else {
			for (index, clause) in &self.0 {
				write!(f, "{}: {}\n", index, clause)?;
			}
			Ok(())
		}
	}
}

#[derive(Debug)]
pub struct RawChainContainer(pub Vec<ClauseIndex>);
impl Display for RawChainContainer {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		let mut first = false;
		write!(f, "(")?;
		for lit in &self.0 {
			if first {
				write!(f, ", ")?;
			} else {
				first = true;
			}
			write!(f, "{}", lit)?;
		}
		write!(f, ")")
	}
}

#[derive(Debug)]
pub struct WitnessContainer(pub Vec<(Variable, Literal)>);
impl Display for WitnessContainer {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		let mut first = false;
		write!(f, "{{")?;
		for (var, lit) in &self.0 {
			if first {
				write!(f, ", ")?;
			} else {
				first = true;
			}
			write!(f, "{} -> {}", var, lit)?;
		}
		write!(f, "}}")
	}
}