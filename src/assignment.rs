use std::{
	fmt::{self, Debug, Formatter},
};

use crate::{
	variable::{Literal}
};

#[derive(PartialEq, Eq)]
pub enum InsertionTest {
	Alright,
	Repeated,
	Conflict,
}

pub struct Block {
	vec: Vec<u8>
}
impl Block {
	pub fn new() -> Block {
		Block { vec: Vec::<u8>::new() }
	}
	pub fn with_capacity(cap: usize) -> Block {
		Block { vec: Vec::<u8>::with_capacity((cap >> 3) + 1usize) }
	}
	pub fn check(&self, lit: Literal) -> bool {
		match self.vec.get(lit.index() >> 3) {
			Some(chunk) => {
				let mask = 1u8 << (lit.index() & 0b111usize);
				mask & chunk != 0u8
			},
			None => false,
		}
	}
	pub fn set(&mut self, lit: Literal) -> InsertionTest {
		let ix = lit.index() >> 3;
		if self.vec.len() <= ix {
			self.vec.resize((ix << 1) + 1usize, 0u8);
		}
		let mt = unsafe { self.vec.get_unchecked_mut(ix) };
		let mask = 0b11u8 << (lit.index() & 0b110usize);
		let submask = 1u8 << (lit.index() & 0b111usize);
		if mask & *mt == 0u8 {
			*mt |= submask;
			InsertionTest::Alright
		} else if submask & *mt != 0u8 {
			InsertionTest::Repeated
		} else {
			InsertionTest::Conflict
		}
	}
	pub fn set_conflict(&mut self, lit: Literal) -> InsertionTest {
		let ix = lit.index() >> 3;
		if self.vec.len() <= ix {
			self.vec.resize((ix << 1) + 1usize, 0u8);
		}
		let mt = unsafe { self.vec.get_unchecked_mut(ix) };
		let submask = 1u8 << (lit.index() & 0b111usize);
		let test = if submask & *mt == 0u8 {
			*mt |= submask;
			InsertionTest::Alright
		} else {
			InsertionTest::Repeated
		};
		test
	}
	pub fn clear(&mut self, lit: Literal) {
		match self.vec.get_mut(lit.index() >> 3) {
			Some(mt) => *mt &= !(1u8 << (lit.index() & 0b111usize)),
			None => (),
		}
	}
	pub fn clear_iter<'a, I: Iterator<Item = &'a Literal>>(&mut self, it: I) {
		for &lit in it {
			self.clear(lit)
		}
	}
}
impl Debug for Block {
	fn fmt(&self , f: &mut Formatter<'_>) -> fmt::Result {
		let mut vec = Vec::<Literal>::new();
		let size = self.vec.len();
		for pos in 0..size {
			let chunk = self.vec.get(pos).unwrap();
			for bit in 0usize..8usize {
				if chunk & (1u8 << bit) != 0u8 {
					let lit = Literal::new(((pos << 3) | bit) as u32);
					vec.push(lit);
				}
			}
		}
		write!(f, "{:?}", vec)
	}
}

#[cfg(test)]
mod test {
	use std::{
		convert::{TryFrom},
	};
	use rand::{self, Rng};
	use crate::{
		assignment::{Block, InsertionTest},
		variable::{Variable, Literal},
	};

	#[derive(Debug)]
	enum TestOp {
		In(Literal),
		Out(Literal),
	}

	fn check_block(vec: &Vec<TestOp>) {
		let mut block = Block::new();
		let mut lits = Vec::<Literal>::new();
		for op in vec {
			match op {
				TestOp::In(lit) => {
					let test = block.set(*lit);
					if lits.contains(lit) {
						assert!(test == InsertionTest::Repeated);
					} else if lits.contains(&lit.complement()) {
						assert!(test == InsertionTest::Conflict);
					} else {
						assert!(test == InsertionTest::Alright);
						lits.push(*lit);
					}
				},
				TestOp::Out(lit) => {
					match lits.iter().enumerate().find(|(_, l)| *l == lit) {
						Some((n, _)) => {
							lits.swap_remove(n);
						},
						None => (),
					}
					block.clear(*lit);
				},
			}
		}
		for n in 0u32..1100u32 {
			let lit = Literal::new(n);
			let inblock = block.check(lit);
			let inlits = lits.iter().find(|&&l| l == lit).is_some();
			assert!(inblock == inlits)
		}
	}

	#[test]
	fn test_block() {
		let mut rng = rand::thread_rng();
		let maxvar = Variable::try_from(500i64).unwrap();
		let mut vec = Vec::<TestOp>::new();
		for _ in 0..100000 {
			let dir: bool = rng.gen();
			let lit = Literal::random(&mut rng, None, maxvar);
			let op = if dir { TestOp::In(lit) } else { TestOp::Out(lit) };
			vec.push(op);
		}
		check_block(&vec);
	}
}