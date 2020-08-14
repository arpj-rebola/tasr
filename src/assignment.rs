use crate::{
	bst::{BinarySearchTree},
	basic::{WitnessContainer},
	variable::{Literal, Variable},
};

#[derive(PartialEq, Eq)]
pub enum InsertionTest {
	Alright,
	Repeated,
	Conflict,
}

pub struct LiteralSet {
	vec: Vec<u8>
}
impl LiteralSet {
	pub fn new() -> LiteralSet {
		let mut vec = Vec::<u8>::new();
		vec.push(0b0000_0000u8);
		LiteralSet { vec: vec }
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
	pub fn set(&mut self, lit: Literal) {
		let ix = lit.index() >> 3;
		if self.vec.len() <= ix {
			self.vec.resize((ix << 1) + 1usize, 0u8);
		}
		let mask = 1u8 << (lit.index() & 0b111usize);
		*unsafe { self.vec.get_unchecked_mut(ix) } |= mask;
	}
	pub fn clear(&mut self, lit: Literal) {
		let ix = lit.index() >> 3;
		if let Some(chunk) = self.vec.get_mut(ix) {
			let mask = !(1u8 << (lit.index() & 0b111usize));
			*chunk &= mask;
		}
	}
	pub fn check_set(&mut self, lit: Literal) -> bool {
		let ix = lit.index() >> 3;
		if self.vec.len() <= ix {
			self.vec.resize((ix << 1) + 1usize, 0u8);
		}
		let chunk = unsafe { self.vec.get_unchecked_mut(ix) };
		let mask = 1u8 << (lit.index() & 0b111usize);
		let result = mask & *chunk != 0u8;
		*chunk |= mask;
		result
	}
	pub fn check_clear(&mut self, lit: Literal) -> bool {
		let ix = lit.index() >> 3;
		if let Some(chunk) = self.vec.get_mut(ix) {
			let mask = 1u8 << (lit.index() & 0b111usize);
			let result = mask & *chunk != 0u8;
			*chunk &= !mask;
			result
		} else {
			false
		}
	}
}

pub struct Block {
	vec: Vec<u8>,
	stack: Vec<Literal>,
}
impl Block {
	pub const Restart: usize = 0usize;
	pub fn new() -> Block {
		Block {
			vec: vec![0b0000_0001u8],
			stack: vec![Literal::Top],
		}
	}
	pub fn check(&self, lit: Literal) -> InsertionTest {
		let ix = lit.index() >> 3;
		if self.vec.len() <= ix {
			InsertionTest::Alright
		} else {
			let chunk = unsafe { self.vec.get_unchecked(ix) };
			let mask = 0b11u8 << (lit.index() & 0b110usize);
			let submask = 1u8 << (lit.index() & 0b111usize);
			if mask & *chunk == 0u8 {
				InsertionTest::Alright
			} else if submask & *chunk != 0u8 {
				InsertionTest::Repeated
			} else {
				InsertionTest::Conflict
			}
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
			self.stack.push(lit);
			InsertionTest::Alright
		} else if submask & *mt != 0u8 {
			InsertionTest::Repeated
		} else {
			InsertionTest::Conflict
		}
	}
	pub fn level(&self) -> usize {
		self.stack.len() - 1usize
	}
	pub fn backtrack(&mut self, lvl: usize) {
		for lit in &self.stack[lvl + 1usize..] {
			let ix = lit.index() >> 3;
			if let Some(chunk) = self.vec.get_mut(ix) {
				let mask = !(0b11u8 << (lit.index() & 0b110usize));
				*chunk &= mask;
			}
		}
		self.stack.truncate(lvl + 1usize);
	}
}

pub struct Substitution {
	bst: BinarySearchTree<Variable, Literal>,
}
impl Substitution {
	pub fn new() -> Substitution {
		Substitution { bst: BinarySearchTree::<Variable, Literal>::new() }
	}
	pub fn explicit_map(&self, lit: Literal) -> Option<Literal> {
		let (var, pol) = lit.split()?;
		let l = self.bst.get(&var)?;
		Some(if pol {
			*l
		} else {
			l.complement()
		})
	}
	pub fn map(&self, lit: Literal) -> Literal {
		self.explicit_map(lit).unwrap_or(lit)
	}
	pub fn set(&mut self, var: Variable, lit: Literal) -> InsertionTest {
		match self.bst.insert(var, lit) {
			Some(()) => InsertionTest::Alright,
			None => {
				if self.bst.get(&var).unwrap() == &lit {
					InsertionTest::Repeated
				} else {
					InsertionTest::Conflict
				}
			}
		}
	}
	pub fn clear(&mut self) {
		self.bst.clear()
	}
	pub fn extract(&self) -> WitnessContainer {
		let vec = self.bst.iter().map(|(k, v)| (*k, *v)).collect();
		WitnessContainer(vec)
	}
}

#[cfg(test)]
mod test {
	use std::{
		convert::{TryFrom},
	};
	use rand::{self, Rng};
	use crate::{
		assignment::{Block, InsertionTest, Substitution},
		variable::{Variable, Literal},
	};

	#[test]
	fn test_backtrack_block() {
		let mut rng = rand::thread_rng();
		let minvar = Variable::try_from(1i64).unwrap();
		let maxvar = Variable::try_from(500i64).unwrap();
		let mut block = Block::new();
		for i in 0usize..100000usize {
			let dir: u8 = rng.gen();
			if dir < 30u8 {
				let maxlevel = block.level();
				let newlevel = rng.gen_range(0usize, maxlevel + 1usize);
				block.backtrack(newlevel);
			} else {
				let lit = Literal::random(&mut rng, Some(minvar), maxvar);
				block.set(lit);
			}
			if i % 1000usize == 0usize {
				for j in 1i64..501i64 {
					let lit = Literal::try_from(j).unwrap();
					let array = block.check(lit);
					let mut stack = InsertionTest::Alright;
					for lit0 in &block.stack {
						if lit0 == &lit {
							stack = InsertionTest::Repeated;
						}
						if lit0 == &lit.complement() {
							stack = InsertionTest::Conflict;
						}
					}
					assert!(stack == array);
				}
			}
		}
	}

	#[test]
	fn test_substitution() {
		let mut rng = rand::thread_rng();
		let minvar = Variable::try_from(1i64).unwrap();
		let maxvar = Variable::try_from(500i64).unwrap();
		let mut subst = Substitution::new();
		for _ in 0usize..100usize {
			let mut vec = Vec::<(Variable, Literal)>::new();
			for _ in 0usize..100usize {
				let var = Variable::random(&mut rng, minvar, maxvar);
				let lit = Literal::random(&mut rng, None, maxvar);
				if let InsertionTest::Alright = subst.set(var, lit) {
					vec.push((var, lit));
				}
			}
			let ext = subst.extract().0;
			assert!(subst.map(Literal::Top) == Literal::Top);
			assert!(subst.map(Literal::Bottom) == Literal::Bottom);
			for i in 1i64..501i64 {
				let var = Variable::try_from(i).unwrap();
				match subst.explicit_map(var.positive()) {
					Some(lit) => {
						assert!(subst.map(var.positive()) == lit);
						assert!(subst.map(var.negative()) == lit.complement());
						let mut found_vec = false;
						for pair in &vec {
							if &(var, lit) == pair {
								found_vec = true;
							}
						}
						let mut found_ext = false;
						for pair in &ext {
							if &(var, lit) == pair {
								found_ext = true;
							}
						}
						assert!(found_vec);
						assert!(found_ext);
					},
					None => {
						assert!(subst.map(var.positive()) == var.positive());
						assert!(subst.map(var.negative()) == var.negative());
						let mut found_vec = false;
						for (v, _) in &vec {
							if &var == v {
								found_vec = true;
							}
						}
						let mut found_ext = false;
						for (v, _) in &ext {
							if &var == v {
								found_ext = true;
							}
						}
						assert!(!found_vec);
						assert!(!found_ext);
					},
				}
			}
			subst.clear();
		}
	}

}