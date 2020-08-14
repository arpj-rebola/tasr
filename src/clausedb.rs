use std::{
	fmt::{self, Debug, Formatter},
	iter::{Enumerate},
	mem::{self, ManuallyDrop},
	slice::{Iter},
	num::{NonZeroU32},
	convert::{TryFrom},
};

use crate::{
	assignment::{LiteralSet},
	hasher::{Hasher32, Hashable32, AmxHasher, UnwindHasher},
	variable::{Literal},
	basic::{ClauseIndex},
};

#[derive(Clone, Copy)]
struct DbAddress {
	val: NonZeroU32,
}
impl DbAddress {
	fn index(&self) -> usize {
		((self.val.get() - 1u32) as usize) << 3
	}
}
impl TryFrom<usize> for DbAddress {
	type Error = usize;
	fn try_from(val: usize) -> Result<DbAddress, usize> {
		if val & 0b111usize == 0usize && val <= ClauseDb::MaxAddress {
			Ok(DbAddress { val: unsafe {
				NonZeroU32::new_unchecked(((val >> 3) + 1usize) as u32)
			} })
		} else {
			Err(val)
		}
	}
}
impl Debug for DbAddress {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "{}", self.index())
	}
}

pub struct ClauseDb {
	vec: Vec<u32>,
	pool: Option<DbAddress>,
	index: Vec<Option<DbAddress>>,
}
impl ClauseDb {
	const DefaultSize: usize = 20;
	const ChunkPower: u32 = 3u32;
	const ChunkSize: usize = 1usize << ClauseDb::ChunkPower;
	const MaxAddress: usize = ((u32::max_value() as usize) - 1usize) << 3;
	#[inline]
	pub fn new() -> ClauseDb {
		ClauseDb::with_capacity(ClauseDb::DefaultSize)
	}
	pub fn with_capacity(cap: usize) -> ClauseDb {
		ClauseDb {
			vec: Vec::<u32>::with_capacity(cap >> ClauseDb::ChunkPower),
			pool: None,
			index: Vec::<Option<DbAddress>>::new(),
		}
	}
	pub fn open<'a, 'b: 'a>(&'a mut self, block: &'b mut LiteralSet, id: ClauseIndex) -> Option<ClauseDbWriter<'a>> {
		let index = id.index();
		if index >= self.index.len() {
			self.index.resize_with((index << 1) + 1usize, || None);
		}
		let slot = unsafe { self.index.get_unchecked(index).is_none() };
		if slot {
			let addr = ClauseDb::allocate(&mut self.vec, &mut self.pool);
			Some(ClauseDbWriter::<'a> {
				db: self,
				addr: addr,
				curr: addr,
				count: 1u32,
				id: id,
				block: block,
				semantics: Some(false),
			})
		} else {
			None
		}
	}
	pub fn retrieve<'a, 'b: 'a>(&'b self, index: ClauseIndex) -> Option<ClauseReference<'a>> {
		let addr = *self.index.get(index.index())?.as_ref()?;
		Some(ClauseReference::<'b> {
			db: self,
			addr: addr,
		})
	}
	pub fn delete(&mut self, index: ClauseIndex) -> Option<()> {
		let rf = self.index.get_mut(index.index())?;
		if let Some(addr) = rf {
			ClauseDb::deallocate(&mut self.vec, &mut self.pool, *addr);
			*rf = None;
			Some(())
		} else {
			None
		}
	}
	pub fn iter<'a, 'b: 'a>(&'b self) -> ClauseDbIterator<'a> {
		ClauseDbIterator::<'a> { it: self.index.iter().enumerate() }
	}
	pub fn extract(&self, id: ClauseIndex) -> Option<Vec<Literal>> {
		Some(self.retrieve(id)?.extract())
	}
	fn allocate(vec: &mut Vec<u32>, pool: &mut Option<DbAddress>) -> DbAddress {
		match *pool {
			Some(x) => {
				let mt = unsafe { ClauseDb::follower_mut(x, vec) };
				*pool = *mt;
				*mt = None;
				x
			},
			None => {
				let len = vec.len();
				vec.resize(len + ClauseDb::ChunkSize, unsafe { mem::transmute::<Option<DbAddress>, u32>(None) });
				DbAddress::try_from(len).expect("ClauseDb capacity exceeded")
			}
		}
	}
	fn deallocate(vec: &mut Vec<u32>, pool: &mut Option<DbAddress>, addr: DbAddress) {
		let last = *pool;
		let mut curr = addr;
		loop {
			let mt = unsafe { ClauseDb::follower_mut(curr, vec) };
			match mt {
				&mut Some(next) => {
					curr = next;
				},
				None => {
					*mt = last;
					*pool = Some(addr);
					break
				}
			}
		}
	}
	#[inline]
	unsafe fn follower_ref(addr: DbAddress, vec: &Vec<u32>) -> &Option<DbAddress> {
		let rf = vec.get_unchecked(addr.index() | (ClauseDb::ChunkSize - 1usize));
		mem::transmute::<&u32, &Option<DbAddress>>(rf)
	}
	#[inline]
	unsafe fn follower_mut(addr: DbAddress, vec: &mut Vec<u32>) -> &mut Option<DbAddress> {
		let mt = vec.get_unchecked_mut(addr.index() | (ClauseDb::ChunkSize - 1usize));
		mem::transmute::<&mut u32, &mut Option<DbAddress>>(mt)
	}
}
impl<'a> IntoIterator for &'a ClauseDb {
    type Item = ClauseIndex;
    type IntoIter = ClauseDbIterator<'a>;
    fn into_iter(self) -> ClauseDbIterator<'a> {
        self.iter()
    }
}
impl Debug for ClauseDb {
	fn fmt(&self , f: &mut Formatter<'_>) -> fmt::Result {
		let size = self.index.len();
		write!(f, "{{\n")?;
		for n in 0..size {
			let index = ClauseIndex::new(n as u32);
			match self.retrieve(index) {
				Some(rf) => write!(f, "\t{:?}: {:?}\n", index, rf)?,
				None => (),
			}
		}
		write!(f, "}}")
	}
}

pub struct ClauseDbIterator<'a> {
	it: Enumerate<Iter<'a, Option<DbAddress>>>
}
impl<'a> Iterator for ClauseDbIterator<'a> {
	type Item = ClauseIndex;
	fn next(&mut self) -> Option<ClauseIndex> {
		loop { match self.it.next() {
			Some((n, Some(_))) => break Some(ClauseIndex::new(n as u32)),
			Some(_) => (),
			None => break None,
		} }
	}
}

pub struct ClauseReport {
	semantics: Option<bool>,
}
impl ClauseReport {
	pub fn is_valid(&self) -> bool {
		self.semantics == Some(true)
	}
	pub fn is_contradiction(&self) -> bool {
		self.semantics == Some(false)
	}
}

pub struct ClauseDbWriter<'a> {
	db: &'a mut ClauseDb,
	addr: DbAddress,
	curr: DbAddress,
	count: u32,
	id: ClauseIndex,
	block: &'a mut LiteralSet,
	semantics: Option<bool>,
}
impl<'a> ClauseDbWriter<'a> {
	pub fn write(&mut self, lit: Literal) -> bool {
		let test = !self.block.check_set(lit);
		if test {
			if lit != Literal::Bottom {
				self.semantics = None;
			}
			if self.block.check(lit.complement()) || lit == Literal::Top {
				self.semantics = Some(true)
			}
			if self.count & ((ClauseDb::ChunkSize - 1usize) as u32) == ((ClauseDb::ChunkSize - 1usize) as u32) {
				let next = ClauseDb::allocate(&mut self.db.vec, &mut self.db.pool);
				*unsafe { ClauseDb::follower_mut(self.curr, &mut self.db.vec) } = Some(next);
				self.curr = next;
				self.count += 1u32;
			}
			let index = self.curr.index() | ((self.count as usize) & (ClauseDb::ChunkSize - 1usize));
			unsafe { *self.db.vec.get_unchecked_mut(index) = mem::transmute::<Literal, u32>(lit); }
			self.count += 1u32;
		}
		test
	}
	pub fn close(mut self) -> ClauseReport {
		self.revert();
		unsafe { *self.db.vec.get_unchecked_mut(self.addr.index()) = self.count; }
		unsafe { *self.db.index.get_unchecked_mut(self.id.index()) = Some(self.addr) }
		let md = ManuallyDrop::new(self);
		ClauseReport { semantics: md.semantics }
	}
	pub fn iter<'b: 'c, 'c>(&'b self) -> ClauseIterator<'c> where 'a: 'b {
		ClauseIterator::<'c>::new(self.db, self.addr, self.count)
	}
	pub fn extract(&self) -> Vec<Literal> {
		self.iter().copied().collect()
	}
	fn revert(&mut self) {
		for &lit in ClauseIterator::new(self.db, self.addr, self.count) {
			self.block.clear(lit);
		}
	}
// 	pub fn length(&self) -> usize {
// 		self.dbw.as_ref().unwrap().length()
// 	}
}
impl<'a> Drop for ClauseDbWriter<'a> {
	fn drop(&mut self) {
		self.revert();
	}
}

struct ClauseSetEntry {
	index: usize,
	size: usize,
	hash: u32,
}

pub struct ClauseSet {
	vec: Vec<Literal>,
	table: Vec<Vec<ClauseSetEntry>>,
}
impl ClauseSet {
	const DefaultSize: usize = 20;
	const Width: usize = (1usize << 22u32) - 1usize;
	pub fn new() -> ClauseSet {
		ClauseSet::with_capacity(ClauseSet::DefaultSize)
	}
	pub fn with_capacity(cap: usize) -> ClauseSet {
		let mut table = Vec::<Vec<ClauseSetEntry>>::new();
		for _ in 0..(ClauseSet::Width + 1usize) {
			table.push(Vec::<ClauseSetEntry>::new());
		}
		ClauseSet {
			vec: Vec::<Literal>::with_capacity(cap),
			table: table,
		}
	}
	pub fn open<'a, 'b: 'a, 'c: 'a>(&'c mut self, block: &'b mut LiteralSet) -> ClauseSetWriter<'a> {
		let offset = self.vec.len();
		ClauseSetWriter::<'a> {
			db: self,
			block: block,
			offset: offset,
			size: 0usize,
			main: UnwindHasher::new(),
			sub: AmxHasher::new(),
		}
	}
	pub fn remove<'a>(&mut self, cls: &ClauseReference<'a>, block: &mut LiteralSet) -> Option<()> {
		let (size, main, sub) = {
			let mut main = UnwindHasher::new();
			let mut sub = AmxHasher::new();
			for &lit in cls.iter() {
				lit.hash(&mut main);
				lit.hash(&mut sub);
				block.set(lit);
			}
			(cls.size(), main.finish() as usize & ClauseSet::Width, sub.finish() as u32)
		};
		let bucket = unsafe { self.table.get_unchecked_mut(main) };
		let opt_index = {
			let mut it = bucket.iter().enumerate();
			loop { match it.next() {
				Some((n, entry)) => {
					if entry.hash == sub && entry.size == size {
						let mut slcit = self.vec[entry.index .. entry.index + entry.size].iter();
						if slcit.all(|&lit| block.check(lit)) {
							break Some(n);
						}
					}
				},
				None => break None,
			} }
		};
		for &lit in cls.iter() {
			block.clear(lit);
		}
		bucket.swap_remove(opt_index?);
		Some(())
	}
	#[cfg(test)]
	pub fn test_remove<'a>(&mut self, cls: &Vec<Literal>, block: &mut LiteralSet) -> Option<()> {
		let (size, main, sub) = {
			let mut main = UnwindHasher::new();
			let mut sub = AmxHasher::new();
			let mut size = 0usize;
			for &lit in cls.iter() {
				size += 1usize;
				lit.hash(&mut main);
				lit.hash(&mut sub);
				block.set(lit);
			}
			(size, main.finish() as usize & ClauseSet::Width, sub.finish() as u32)
		};
		let bucket = unsafe { self.table.get_unchecked_mut(main) };
		let opt_index = {
			let mut it = bucket.iter().enumerate();
			loop { match it.next() {
				Some((n, entry)) => {
					if entry.hash == sub && entry.size == size {
						let mut slcit = self.vec[entry.index .. entry.index + entry.size].iter();
						if slcit.all(|&lit| block.check(lit)) {
							break Some(n);
						}
					}
				},
				None => break None,
			} }
		};
		for &lit in cls.iter() {
			block.clear(lit);
		}
		bucket.swap_remove(opt_index?);
		Some(())
	}
}

pub struct ClauseSetWriter<'a> {
	db: &'a mut ClauseSet,
	block: &'a mut LiteralSet,
	offset: usize,
	size: usize,
	main: UnwindHasher,
	sub: AmxHasher,
}
impl<'a> ClauseSetWriter<'a> {
	pub fn write(&mut self, lit: Literal) -> bool {
		let test = !self.block.check_set(lit);
		if test {
			self.db.vec.push(lit);
			lit.hash(&mut self.main);
			lit.hash(&mut self.sub);
			self.size += 1usize;
		}
		test
	}
	pub fn close(mut self) {
		self.revert();
		let index = (self.main.finish() as usize) & ClauseSet::Width;
		let bucket = unsafe { self.db.table.get_unchecked_mut(index) };
		let entry = ClauseSetEntry {
			index: self.offset,
			size: self.size,
			hash: self.sub.finish(),
		};
		bucket.push(entry);
		ManuallyDrop::new(self);
	}
	pub fn extract(&self) -> Vec<Literal> {
		self.db.vec[self.offset..self.offset + self.size].iter().copied().collect()
	}
	fn revert(&mut self) {
		for &lit in &self.db.vec[self.offset..self.offset + self.size] {
			self.block.clear(lit);
		}
	}
}
impl<'a> Drop for ClauseSetWriter<'a> {
	fn drop(&mut self) {
		self.revert();
	}
}

#[derive(Copy, Clone)]
pub struct ClauseReference<'a> {
	db: &'a ClauseDb,
	addr: DbAddress,
}
impl<'a> ClauseReference<'a> {
	pub fn iter<'b: 'c, 'c>(self) -> ClauseIterator<'c> where 'a: 'b {
		let size = unsafe { *self.db.vec.get_unchecked(self.addr.index()) };
		ClauseIterator::<'c>::new(self.db, self.addr, size)
	}
	pub fn extract(&self) -> Vec<Literal> {
		self.iter().copied().collect()
	}
	pub fn size(&self) -> usize {
		unsafe { *self.db.vec.get_unchecked(self.addr.index()) as usize }
	}
}
impl<'a> Hashable32 for ClauseReference<'a> {
	fn hash<H: Hasher32>(&self, hs: &mut H) {
		for lit in self.iter() {
			lit.hash(hs)
		}
	}
}
impl<'a: 'b, 'b> IntoIterator for &'b ClauseReference<'a> {
    type Item = &'b Literal;
    type IntoIter = ClauseIterator<'b>;
    fn into_iter(self) -> ClauseIterator<'b> {
        self.iter()
    }
}
impl<'a> Debug for ClauseReference<'a> {
	fn fmt(&self , f: &mut Formatter<'_>) -> fmt::Result {
		let mut vec = Vec::<Literal>::new();
		for lit in self.iter() {
			vec.push(*lit);
		}
		write!(f, "{:?}", vec)
	}
}

pub struct ClauseIterator<'a> {
	db: &'a ClauseDb,
	addr: DbAddress,
	count: u32,
	size: u32,
}
impl<'a> ClauseIterator<'a> {
	fn new<'b: 'a>(db: &'b ClauseDb, addr: DbAddress, size: u32) -> ClauseIterator<'a> {
		ClauseIterator::<'a> {
			db: db,
			addr: addr,
			count: 1u32,
			size: size,
		}
	}
	// fn size(&self) -> usize {
	// 	self.size as usize
	// }
}
impl<'a> Iterator for ClauseIterator<'a> {
	type Item = &'a Literal;
	fn next(&mut self) -> Option<&'a Literal> {
		if self.count == self.size {
			None
		} else {
			let rf = unsafe { self.db.vec.get_unchecked(self.addr.index() | ((self.count as usize) & (ClauseDb::ChunkSize - 1usize))) };
			self.count += 1u32;
			if self.count & ((ClauseDb::ChunkSize - 1usize) as u32) == (ClauseDb::ChunkSize - 1usize) as u32 {
				let flw = unsafe { ClauseDb::follower_ref(self.addr, &self.db.vec) };
				match flw {
					&Some(next) => {
						self.addr = next;
						self.count += 1u32;
					},
					&None => (),
				}
			}
			unsafe { Some(mem::transmute::<&u32, &Literal>(rf)) }
		}
	}
}

#[cfg(test)]
mod test {
	use std::{
		mem::{self},
	};
	use rand::{self, Rng,
		seq::{SliceRandom},
	};
	use crate::{
		assignment::{LiteralSet},
		clausedb::{ClauseSet, ClauseDb},
		basic::{ClauseIndex},
		variable::{Literal, Variable},
	};

	fn generate_clause<R: Rng + ?Sized>(rng: &mut R, block: &mut LiteralSet) -> Vec<Literal> {
		let size = rng.gen_range(0usize, 100usize);
		let mut vec = Vec::<Literal>::new();
		for _ in 0..size {
			let lit = Literal::random(rng, Some(Variable::MinVariable), Variable::new(10000000u32));
			let test = !block.check_set(lit);
			if test {
				vec.push(lit);
			}
		}
		for &lit in &vec {
			block.clear(lit)
		}
		let lit_slice: &mut [Literal] = &mut vec;
		let u32_slice: &mut [u32] = unsafe { mem::transmute::<&mut [Literal], &mut [u32]>(lit_slice) };
		u32_slice.sort();
		vec
	}

	fn check_clause(candidate: &Vec<Literal>, list: &Vec<Vec<Literal>>) -> bool {
		for vec in list {
			let mut it1 = candidate.iter();
			let mut it2 = vec.iter();
			loop {
				match (it1.next(), it2.next()) {
					(Some(x), Some(y)) => if x != y {
						break;
					},
					(None, None) => return false,
					_ => break,
				}
			}
		}
		true
	}

	fn check_table_size(set: &ClauseSet, size: usize) {
		let mut total = 0usize;
		for row in &set.table {
			total += row.len()
		}
		assert!(total == size);
	}

	#[test]
	fn test_clause_set() {
		let mut set = ClauseSet::new();
		let mut rng = rand::thread_rng();
		let mut inner = Vec::<Vec<Literal>>::new();
		let mut outer = Vec::<Vec<Literal>>::new();
		let mut block = LiteralSet::new();
		let inner_clauses = 1000usize;
		let outer_clauses = 500usize;
		for _ in 0..inner_clauses {
			let cls = generate_clause(&mut rng, &mut block);
			inner.push(cls);
		}
		for _ in 0..outer_clauses {
			let cls = generate_clause(&mut rng, &mut block);
			if check_clause(&cls, &inner) {
				outer.push(cls);
			}
		}
		check_table_size(&set, 0usize);
		for cls in &inner {
			let mut dbw = set.open(&mut block);
			for lit in cls {
				dbw.write(*lit);
			}
			dbw.close();
		}
		check_table_size(&set, inner_clauses);
		for cls in &outer {
			assert!(set.test_remove(cls, &mut block).is_none())
		}
		check_table_size(&set, inner_clauses);
		for cls in &mut inner {
			cls.shuffle(&mut rng);
			assert!(set.test_remove(cls, &mut block).is_some())
		}
		check_table_size(&set, 0usize);
		for cls in &mut inner {
			cls.shuffle(&mut rng);
			assert!(set.test_remove(cls, &mut block).is_none())
		}
		check_table_size(&set, 0usize);
	}

	#[derive(Debug)]
	enum TestOp {
		In(u32, Vec<Literal>),
		Out(u32),
	}

	#[test]
	fn test_clause_database() {
		let mut cdb = ClauseDb::new();
		let mut full = Vec::<u32>::new();
		let mut empty = Vec::<u32>::new();
		let mut ops = Vec::<TestOp>::new();
		let mut block = LiteralSet::new();
		let mut rng = rand::thread_rng();
		for index in 0..100u32 {
			empty.push(index)
		}
		for _ in 0..10000 {
			assert!(full.len() + empty.len() == 100usize);
			let mut dir: bool = rng.gen();
			if full.is_empty() {
				dir = true;
			}
			if empty.is_empty() {
				dir = false;
			}
			if dir {
				let index = empty.choose(&mut rng).unwrap();
				let mut vec = Vec::<Literal>::new();
				let size: usize = rng.gen_range(0usize, 3usize);
				for _ in 0..size {
					let lit = Literal::random(&mut rng, Some(Variable::MinVariable), Variable::new(10000000u32));
					let test = !block.check_set(lit);
					if test {
						vec.push(lit);
					}
				}
				for &lit in &vec {
					block.clear(lit)
				}
				ops.push(TestOp::In(*index, vec));
				full.push(*index);
				let (n, _) = empty.iter().enumerate().find(|(_, x)| x == &index).unwrap();
				empty.swap_remove(n);
			} else {
				let index = full.choose(&mut rng).unwrap();
				ops.push(TestOp::Out(*index));
				empty.push(*index);
				let (n, _) = full.iter().enumerate().find(|(_, x)| x == &index).unwrap();
				full.swap_remove(n);
			}
		}
		for op in &ops {
			match op {
				TestOp::In(index, vec) => {
					let index = ClauseIndex::new(*index);
					assert!(cdb.retrieve(index).is_none());
					let mut dbw = cdb.open(&mut block, index).unwrap();
					for lit in vec {
						let test = dbw.write(*lit);
						assert!(test);
					}
					dbw.close();
				},
				TestOp::Out(index) => {
					let index = ClauseIndex::new(*index);
					assert!(cdb.retrieve(index).is_some());
					let del = cdb.delete(index);
					assert!(del.is_some());
					assert!(cdb.delete(index).is_none());
				},
			}
		}
		let size = cdb.index.len();
		for n in 0usize..size {
			let m = n as u32;
			let mut cls: Option<&Vec<Literal>> = None;
			for op in &ops {
				match op {
					TestOp::In(index, vec) => if index == &m {
						cls = Some(&vec);
					},
					TestOp::Out(index) => if index == &m {
						cls = None;
					},
				}
			}
			let index = ClauseIndex::new(m);
			match cls {
				Some(vec) => {
					let rf = cdb.retrieve(index).unwrap();
					let mut it1 = rf.iter();
					let mut it2 = vec.iter();
					loop {
						match (it1.next(), it2.next()) {
							(Some(x), Some(y)) => {
								assert!(x == y);
							},
							(None, None) => break,
							_ => {
								assert!(false);
							}
						}
					}
					assert!(cdb.delete(index).is_some());
					assert!(cdb.delete(index).is_none());
					assert!(cdb.retrieve(index).is_none());
				},
				None => {
					assert!(cdb.delete(index).is_none());
				},
			}
		}
	}
}