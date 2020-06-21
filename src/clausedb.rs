use std::{
	convert::{TryFrom},
	fmt::{self, Debug, Formatter, Display},
	iter::{Enumerate},
	mem::{self, ManuallyDrop},
	ops::{BitOr, BitOrAssign},
	slice::{Iter},
};

use crate::{
	assignment::{Block, InsertionTest},
	hasher::{Hasher32, Hashable32, AmxHasher, UnwindHasher},
	variable::{Literal, Variable},
	chunkdb::{DbAddress, ChunkDb, ChunkDbReference, ChunkDbWriter, ChunkDbIterator},
};

#[derive(Debug)]
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
		for (index, clause) in &self.0 {
			write!(f, "{}: {}\n", index, clause)?;
		}
		Ok(())
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

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(transparent)]
pub struct ClauseIndex {
	val: u32,
}
impl ClauseIndex {
	pub const MaxValue: i64 = (u32::max_value() as i64) + 1i64;
	pub fn index(&self) -> usize {
		self.val as usize
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
		write!(f, "#{}", self.index())
	}
}
impl Display for ClauseIndex {
	fn fmt(&self , f: &mut Formatter<'_>) -> fmt::Result {
		write!(f, "#{}", self.index())
	}
}

pub struct ClauseDbMeta {
	// pub tautology: bool,
}

pub struct ClauseDbEntry {
	addr: DbAddress,
	meta: ClauseDbMeta
}
impl ClauseDbEntry {
	pub fn follow<'a, 'b: 'a>(&self, db: &'b ClauseDb) -> ClauseReference<'a> {
		unsafe { ClauseReference::<'a> { rf: db.db.retrieve(self.addr) } }
	}
	pub fn meta(&self) -> &ClauseDbMeta {
		&self.meta
	}
	pub fn mut_meta(&mut self) -> &mut ClauseDbMeta {
		&mut self.meta
	}
}

pub struct ClauseDb {
	db: ChunkDb,
	index: Vec<Option<ClauseDbEntry>>,
}
impl ClauseDb {
	const DefaultSize: usize = 20;
	#[inline]
	pub fn new() -> ClauseDb {
		ClauseDb::with_capacity(ClauseDb::DefaultSize)
	}
	pub fn with_capacity(cap: usize) -> ClauseDb {
		ClauseDb {
			db: ChunkDb::new(cap),
			index: Vec::<Option<ClauseDbEntry>>::new(),
		}
	}
	pub fn open<'a, 'b: 'a>(&'a mut self, block: &'b mut Block, id: ClauseIndex) -> Option<ClauseDbWriter<'a>> {
		let index = id.index();
		if index >= self.index.len() {
			self.index.resize_with(index << 1, || None);
		}
		let rf = unsafe { self.index.get_unchecked_mut(index) };
		if rf.is_none() {
			Some(ClauseDbWriter::<'a> {
				dbw: Some(self.db.open()),
				entry: rf,
				block: block,
			})
		} else {
			None
		}
	}
	pub fn entry(&self, index: ClauseIndex) -> Option<&ClauseDbEntry> {
		self.index.get(index.index()).map(|r| r.as_ref()).flatten()
	}
	pub fn retrieve<'a, 'b: 'a>(&'b self, index: ClauseIndex) -> Option<ClauseReference<'a>> {
		Some(ClauseReference::<'b> {
			rf: unsafe { self.db.retrieve(self.entry(index)?.addr) },
		})
	}
	pub fn delete(&mut self, index: ClauseIndex) -> Option<()> {
		let entry_opt = self.index.get_mut(index.index())?;
		match entry_opt {
			Some(entry) => {
				self.db.deallocate(entry.addr);
				*entry_opt = None;
				Some(())
			},
			None => None,
		}
	}
	pub fn iter<'a, 'b: 'a>(&'b self) -> ClauseDbIterator<'a> {
		ClauseDbIterator::<'a> { it: self.index.iter().enumerate() }
	}
	pub fn extract(&self, id: ClauseIndex) -> Option<ClauseContainer> {
		Some(self.retrieve(id)?.extract())
	}
	pub fn extract_chain(&self, chain: &[ClauseIndex], cutoff: usize) -> Option<ChainContainer> {
		let mut it = chain.iter().enumerate();
		let mut out = Vec::<(ClauseIndex, ClauseContainer)>::new();
		while let Some((n, id)) = it.next() {
			if n < cutoff {
				out.push((*id, self.extract(*id)?));
			} else {
				break;
			}
		}
		Some(ChainContainer(out))
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
			let index = ClauseIndex { val: n as u32 };
			match self.retrieve(index) {
				Some(rf) => write!(f, "\t{:?}: {:?}\n", index, rf)?,
				None => (),
			}
		}
		write!(f, "}}")
	}
}

pub struct ClauseDbIterator<'a> {
	it: Enumerate<Iter<'a, Option<ClauseDbEntry>>>
}
impl<'a> Iterator for ClauseDbIterator<'a> {
	type Item = ClauseIndex;
	fn next(&mut self) -> Option<ClauseIndex> {
		loop { match self.it.next() {
			Some((n, rf)) => if rf.is_some() {
				break Some(ClauseIndex { val: n as u32 });
			},
			None => break None,
		} }
	}
}


pub struct ClauseDbWriter<'a> {
	dbw: Option<ChunkDbWriter<'a>>,
	entry: &'a mut Option<ClauseDbEntry>,
	block: &'a mut Block,
}
impl<'a> ClauseDbWriter<'a> {
	pub fn write(&mut self, lit: Literal) -> InsertionTest {
		let test = self.block.set(lit);
		if test == InsertionTest::Alright {
			unsafe { self.dbw.as_mut().unwrap().write(mem::transmute::<Literal, u32>(lit)); }
		}
		test
	}
	pub fn close(mut self, meta: ClauseDbMeta) {
		self.revert();
		let mut md = ManuallyDrop::<ClauseDbWriter<'a>>::new(self);
		*md.entry = Some(ClauseDbEntry {
			addr: md.dbw.take().unwrap().close(),
			meta: meta,
		})
	}
	pub fn iter<'b: 'c, 'c>(&'b self) -> ClauseIterator<'c> where 'a: 'b {
		ClauseIterator::<'c> {
			it: self.dbw.as_ref().unwrap().iter()
		}
	}
	pub fn extract(&self) -> ClauseContainer {
		let it = self.iter();
		ClauseContainer(it.copied().collect())
	}
	fn revert(&mut self) {
		let it = ClauseIterator::<'_> { it: self.dbw.as_ref().unwrap().iter() };
		self.block.clear_iter(it);
	}
	pub fn length(&self) -> usize {
		self.dbw.as_ref().unwrap().length()
	}
}
impl<'a> Drop for ClauseDbWriter<'a> {
	fn drop(&mut self) {
		self.revert();
	}
}

pub struct ClauseSetEntry {
	addr: DbAddress,
	hash: u32,
}

pub struct ClauseSet {
	db: ChunkDb,
	table: Vec<Vec<ClauseSetEntry>>,
}
impl ClauseSet {
	const DefaultSize: usize = 20;
	const Width: usize = (1usize << 22u32) - 1usize;
	#[inline]
	pub fn new() -> ClauseSet {
		ClauseSet::with_capacity(ClauseSet::DefaultSize)
	}
	pub fn with_capacity(cap: usize) -> ClauseSet {
		let mut table = Vec::<Vec<ClauseSetEntry>>::new();
		for _ in 0..(ClauseSet::Width + 1usize) {
			table.push(Vec::<ClauseSetEntry>::new());
		}
		ClauseSet {
			db: ChunkDb::new(cap),
			table: table,
		}
	}
	pub fn open<'a, 'b: 'a>(&'a mut self, block: &'b mut Block) -> ClauseSetWriter<'a> {
		ClauseSetWriter::<'a> {
			dbw: Some(self.db.open()),
			block: block,
			table: &mut self.table,
			main: UnwindHasher::new(),
			sub: AmxHasher::new(),
		}
	}
	pub fn delete<'a, 'b: 'a, 'c: 'a, 'd: 'a, 'e: 'b>(&'b mut self, rf: &'b ClauseReference<'e>, block: &'d mut Block) -> Option<()> {
		let (comp, h1, h2) = ClauseSetComparer::<'a, 'e>::new(rf, block);
		let bucket = unsafe { self.table.get_unchecked_mut(h1) };
		let optn = {
			let mut it = bucket.iter().enumerate();
			loop {
				match it.next() {
					Some((i, entry)) => if entry.hash == h2 {
						let oth = ClauseReference::<'_> {
							rf: unsafe { self.db.retrieve(entry.addr) }
						};
						if comp.eq(&oth) {
							break Some(i)
						}
					},
					None => break None,
				}
			}
		};
		match optn {
			Some(n) => {
				bucket.swap_remove(n);
				Some(())
			},
			None => None,
		}
	}
}

pub struct ClauseSetWriter<'a> {
	dbw: Option<ChunkDbWriter<'a>>,
	block: &'a mut Block,
	table: &'a mut Vec<Vec<ClauseSetEntry>>,
	main: UnwindHasher,
	sub: AmxHasher,
}
impl<'a> ClauseSetWriter<'a> {
	pub fn write(&mut self, lit: Literal) -> InsertionTest {
		let test = self.block.set(lit);
		if test == InsertionTest::Alright {
			unsafe { self.dbw.as_mut().unwrap().write(mem::transmute::<Literal, u32>(lit)); }
			lit.hash(&mut self.main);
			lit.hash(&mut self.sub);
		}
		test
	}
	pub fn close(mut self) {
		self.revert();
		let mut md = ManuallyDrop::<ClauseSetWriter<'a>>::new(self);
		let entry = ClauseSetEntry {
			addr: md.dbw.take().unwrap().close(),
			hash: md.sub.finish() as u32,
		};
		let index = (md.main.finish() as usize) & ClauseSet::Width;
		let bucket = unsafe { md.table.get_unchecked_mut(index) };
		bucket.push(entry);
	}
	pub fn iter<'b: 'c, 'c>(&'b self) -> ClauseIterator<'c> where 'a: 'b {
		ClauseIterator::<'c> {
			it: self.dbw.as_ref().unwrap().iter()
		}
	}
	pub fn extract(self) -> ClauseContainer {
		ClauseContainer(self.iter().copied().collect())
	}
	fn revert(&mut self) {
		let it = ClauseIterator::<'_> { it: self.dbw.as_ref().unwrap().iter() };
		self.block.clear_iter(it);
	}
}
impl<'a> Drop for ClauseSetWriter<'a> {
	fn drop(&mut self) {
		self.revert();
	}
}

pub struct ClauseSetComparer<'a, 'b: 'a> {
	comp: &'a ClauseReference<'b>,
	block: &'a mut Block,
	size: usize,
}
impl<'a, 'b: 'a> ClauseSetComparer<'a, 'b> {
	fn new(comp: &'a ClauseReference<'b>, block: &'a mut Block) -> (ClauseSetComparer<'a, 'b> , usize, u32) {
		let mut hasher1 = UnwindHasher::new();
		let mut hasher2 = AmxHasher::new();
		let it = comp.iter();
		let size = it.size();
		for &lit in it {
			block.set(lit);
			lit.hash(&mut hasher1);
			lit.hash(&mut hasher2);
		}
		let hash1 = (hasher1.finish() as usize) & ClauseSet::Width;
		let hash2 = hasher2.finish() as u32;
		(ClauseSetComparer::<'a, 'b> {
			comp: comp,
			block: block,
			size: size,
		}, hash1, hash2)
	}
	fn eq<'c>(&self, rf: &ClauseReference<'c>) -> bool {
		let it = rf.iter();
		if it.size() == self.size {
			for &lit in rf.iter() {
				if !self.block.check(lit) {
					return false;
				}
			}
		}
		return true;
	}
}
impl<'a, 'b: 'a> Drop for ClauseSetComparer<'a, 'b> {
	fn drop(&mut self) {
		self.block.clear_iter(self.comp.iter())
	}
}

pub struct ClauseReference<'a> {
	rf: ChunkDbReference<'a>,
}
impl<'a> ClauseReference<'a> {
	pub fn iter<'b: 'c, 'c>(&'b self) -> ClauseIterator<'c> where 'a: 'b {
		ClauseIterator::<'c> { it: self.rf.iter() }
	}
	pub fn extract(&self) -> ClauseContainer {
		ClauseContainer(self.iter().copied().collect())
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
	it: ChunkDbIterator<'a>,
}
impl<'a> ClauseIterator<'a> {
	fn size(&self) -> usize {
		self.it.size()
	}
}
impl<'a> Iterator for ClauseIterator<'a> {
	type Item = &'a Literal;
	fn next(&mut self) -> Option<&'a Literal> {
		self.it.next().map(|rf| unsafe { mem::transmute::<&u32, &Literal>(rf) })
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
		assignment::{Block, InsertionTest},
		chunkdb::{ChunkDb},
		clausedb::{ClauseSet, ClauseReference, ClauseDb, ClauseDbMeta, ClauseIndex},
		variable::{Literal, Variable},
	};

	fn generate_clause<R: Rng + ?Sized>(rng: &mut R, block: &mut Block) -> Vec<Literal> {
		let size = rng.gen_range(0usize, 100usize);
		let mut vec = Vec::<Literal>::new();
		for _ in 0..size {
			let lit = Literal::random(rng, Some(Variable::MinVariable), Variable::new(10000000u32));
			let test = block.set(lit);
			if test == InsertionTest::Alright {
				vec.push(lit);
			}
		}
		block.clear_iter(vec.iter());
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
		let mut block = Block::new();
		let mut db = ChunkDb::new(3usize);
		for _ in 0..1000 {
			let cls = generate_clause(&mut rng, &mut block);
			inner.push(cls);
		}
		for _ in 0..500 {
			let cls = generate_clause(&mut rng, &mut block);
			if check_clause(&cls, &inner) {
				outer.push(cls);
			}
		}
		check_table_size(&set, 0usize);
		for cls in &mut inner {
			let lit_slice: &mut [Literal] = cls;
			let u32_slice: &mut [u32] = unsafe { mem::transmute::<&mut [Literal], &mut [u32]>(lit_slice) };
			u32_slice.shuffle(&mut rng);
			let mut dbw = set.open(&mut block);
			for lit in cls {
				dbw.write(*lit);
			}
			dbw.close();
		}
		check_table_size(&set, 1000usize);
		for cls in &mut outer {
			let lit_slice: &mut [Literal] = cls;
			let u32_slice: &mut [u32] = unsafe { mem::transmute::<&mut [Literal], &mut [u32]>(lit_slice) };
			u32_slice.shuffle(&mut rng);
			let mut dbw = db.open();
			for lit in cls {
				unsafe { dbw.write(mem::transmute::<Literal, u32>(*lit)); }
			}
			let addr = dbw.close();
			let rf = ClauseReference { rf: unsafe { db.retrieve(addr) } };
			assert!(set.delete(&rf, &mut block).is_none());
		}
		check_table_size(&set, 1000usize);
		for cls in &mut inner {
			let lit_slice: &mut [Literal] = cls;
			let u32_slice: &mut [u32] = unsafe { mem::transmute::<&mut [Literal], &mut [u32]>(lit_slice) };
			u32_slice.shuffle(&mut rng);
			let mut dbw = db.open();
			for lit in cls {
				unsafe { dbw.write(mem::transmute::<Literal, u32>(*lit)); }
			}
			let addr = dbw.close();
			let rf = ClauseReference { rf: unsafe { db.retrieve(addr) } };
			assert!(set.delete(&rf, &mut block).is_some());
		}
		check_table_size(&set, 0usize);
		for cls in &mut inner {
			let lit_slice: &mut [Literal] = cls;
			let u32_slice: &mut [u32] = unsafe { mem::transmute::<&mut [Literal], &mut [u32]>(lit_slice) };
			u32_slice.shuffle(&mut rng);
			let mut dbw = db.open();
			for lit in cls {
				unsafe { dbw.write(mem::transmute::<Literal, u32>(*lit)); }
			}
			let addr = dbw.close();
			let rf = ClauseReference { rf: unsafe { db.retrieve(addr) } };
			assert!(set.delete(&rf, &mut block).is_none());
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
		let mut block = Block::new();
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
					let test = block.set(lit);
					if test == InsertionTest::Alright {
						vec.push(lit);
					}
				}
				block.clear_iter(vec.iter());
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
					let index = ClauseIndex { val: *index };
					assert!(cdb.retrieve(index).is_none());
					let mut dbw = cdb.open(&mut block, index).unwrap();
					for lit in vec {
						let test = dbw.write(*lit);
						assert!(test == InsertionTest::Alright);
					}
					let meta = ClauseDbMeta {};
					dbw.close(meta);
				},
				TestOp::Out(index) => {
					let index = ClauseIndex { val: *index };
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
			let index = ClauseIndex { val: m };
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