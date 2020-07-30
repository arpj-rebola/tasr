use std::{
	fmt::{self, Debug, Formatter},
};

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

pub struct Block {
	vec: Vec<u8>
}
impl Block {
	pub fn new() -> Block {
		let mut vec = Vec::<u8>::new();
		vec.push(0b0000_0001u8);
		Block { vec: vec }
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
	pub fn test(&self, lit: Literal) -> InsertionTest {
		let ix = lit.index() >> 3;
		if self.vec.len() <= ix {
			InsertionTest::Alright
		} else {
			let mt = unsafe { self.vec.get_unchecked(ix) };
			let mask = 0b11u8 << (lit.index() & 0b110usize);
			let submask = 1u8 << (lit.index() & 0b111usize);
			if mask & *mt == 0u8 {
				InsertionTest::Alright
			} else if submask & *mt != 0u8 {
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
			InsertionTest::Alright
		} else if submask & *mt != 0u8 {
			InsertionTest::Repeated
		} else {
			InsertionTest::Conflict
		}
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

pub struct BlockStack {
	block: Block,
	lits: Vec<Literal>,
	stack: Vec<usize>,
}
impl BlockStack {
	pub fn new() -> BlockStack {
		BlockStack {
			block: Block::new(),
			lits: Vec::<Literal>::new(),
			stack: Vec::<usize>::new(),
		}
	}
	pub fn check(&self, lit: Literal) -> bool {
		self.block.check(lit)
	}
	pub fn test(&self, lit: Literal) -> InsertionTest {
		self.block.test(lit)
	}
	pub fn set(&mut self, lit: Literal) -> InsertionTest {
		let test = self.block.set(lit);
		match test {
			InsertionTest::Alright => self.lits.push(lit),
			_ => (),
		}
		test
	}
	pub fn push(&mut self) {
		self.stack.push(self.lits.len())
	}
	pub fn pop(&mut self) {
		let n = self.stack.pop().unwrap_or(0usize);
		for lit in &self.lits[n..] {
			self.block.clear(*lit);
		}
		self.lits.truncate(n);
	}
	pub fn clear(&mut self) {
		for lit in &self.lits {
			self.block.clear(*lit);
		}
		self.lits.clear();
		self.stack.clear();
	}
}

pub struct BacktrackBlock {
	block: Block,
	lits: Vec<Literal>,
}
impl BacktrackBlock {
	pub fn new() -> BacktrackBlock {
		BacktrackBlock {
			block: Block::new(),
			lits: Vec::<Literal>::new(),
		}
	}
	pub unsafe fn mut_block(&mut self) -> &mut Block {
		&mut self.block
	}
	pub fn check(&self, lit: Literal) -> bool {
		self.block.check(lit)
	}
	pub fn test(&self, lit: Literal) -> InsertionTest {
		self.block.test(lit)
	}
	pub fn set(&mut self, lit: Literal) -> InsertionTest {
		let test = self.block.set(lit);
		match test {
			InsertionTest::Alright => self.lits.push(lit),
			_ => (),
		}
		test
	}
	pub fn backtrack(&mut self, n: usize) {
		for lit in &self.lits[n..] {
			self.block.clear(*lit);
		}
		self.lits.truncate(n);
	}
	pub fn level(&self) -> usize {
		self.lits.len()
	}
	pub fn clear(&mut self) {
		for lit in &self.lits {
			self.block.clear(*lit);
		}
		self.lits.clear();
	}
}
impl Debug for BacktrackBlock {
	fn fmt(&self , f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "{:?} || {:?}", self.block, self.lits)
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
		assignment::{Block, InsertionTest, BacktrackBlock, Substitution},
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
		lits.push(Literal::Top);
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
		assert!(block.test(Literal::Top) == InsertionTest::Repeated);
		assert!(block.test(Literal::Bottom) == InsertionTest::Conflict);
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
		let minvar = Variable::try_from(1i64).unwrap();
		let maxvar = Variable::try_from(500i64).unwrap();
		let mut vec = Vec::<TestOp>::new();
		for _ in 0..100000 {
			let dir: bool = rng.gen();
			let lit = Literal::random(&mut rng, Some(minvar), maxvar);
			let op = if dir { TestOp::In(lit) } else { TestOp::Out(lit) };
			vec.push(op);
		}
		check_block(&vec);
	}

	#[test]
	fn test_backtrack_block() {
		let mut rng = rand::thread_rng();
		let minvar = Variable::try_from(1i64).unwrap();
		let maxvar = Variable::try_from(500i64).unwrap();
		let mut block = BacktrackBlock::new();
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
					let array_pos = block.check(lit);
					let array_neg = block.check(lit.complement());
					let mut stack_pos = false;
					let mut stack_neg = false;
					for lit0 in &block.lits {
						if lit0 == &lit {
							stack_pos = true;
						}
						if lit0 == &lit.complement() {
							stack_neg = true;
						}
					}
					assert!(stack_pos == array_pos);
					assert!(stack_neg == array_neg);
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