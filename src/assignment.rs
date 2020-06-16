use std::{
	convert::{TryFrom},
	fmt::{self, Debug, Formatter},
};

use crate::{
	clausedb::{WitnessContainer},
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
	pub fn length(&self) -> usize {
		self.lits.len()
	}
	pub fn clear(&mut self) {
		for lit in &self.lits {
			self.block.clear(*lit);
		}
		self.lits.clear();
	}
}

pub struct Substitution {
	vec: Vec<Literal>,
}
impl Substitution {
	pub fn new() -> Substitution {
		Substitution { vec: Vec::<Literal>::new() }
	}
	pub fn map(&self, lit: Literal) -> Literal {
		match lit.split() {
			Some((var, pol)) => match self.vec.get(var.index()) {
				Some(l) => if pol {
					*l
				} else {
					l.complement()
				},
				None => lit,
			},
			None => lit,
		}
	}
	pub fn set(&mut self, var: Variable, lit: Literal) -> InsertionTest {
		let index = var.index();
		if index >= self.vec.len() {
			let size = usize::min(Variable::MaxIndex + 1usize, (index << 1) + 1usize);
			let mut new = Variable::try_from((self.vec.len() + 1usize) as i64).unwrap();
			self.vec.resize_with(size, || { let v = new; new = new.next() ; v.positive() });
		}
		let mt = unsafe { self.vec.get_unchecked_mut(var.index()) };
		if *mt == var.positive() && lit != *mt {
			*mt = lit;
			InsertionTest::Alright
		} else if lit == *mt {
			InsertionTest::Repeated
		} else {
			InsertionTest::Conflict
		}
	}
	pub fn clear(&mut self, var: Variable) {
		match self.vec.get_mut(var.index()) {
			Some(mt) => *mt = var.positive(),
			None => (),
		}
	}
}

pub struct SubstitutionStack {
	subst: Substitution,
	vars: Vec<Variable>,
}
impl SubstitutionStack {
	pub fn new() -> SubstitutionStack {
		SubstitutionStack {
			subst: Substitution::new(),
			vars: Vec::<Variable>::new(),
		}
	}
	pub fn map(&self, lit: Literal) -> Literal {
		self.subst.map(lit)
	}
	pub fn set(&mut self, var: Variable, lit: Literal) -> InsertionTest {
		let test = self.subst.set(var, lit);
		match test {
			InsertionTest::Alright => self.vars.push(var),
			_ => (),
		}
		test
	}
	pub fn clear(&mut self) {
		for var in &self.vars {
			self.subst.clear(*var);
		}
		self.vars.clear();
	}
	pub fn extract(&self) -> WitnessContainer {
		let mut out = Vec::<(Variable, Literal)>::new();
		for var in &self.vars {
			let lit = self.map(var.positive());
			out.push((*var, lit));
		}
		WitnessContainer(out)
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

}