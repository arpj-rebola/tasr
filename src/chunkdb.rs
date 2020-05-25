use std::{
	convert::{TryFrom},
	fmt::{self, Debug, Formatter},
	marker::{PhantomData},
	mem::{self, ManuallyDrop},
	num::{NonZeroU32},
};

pub struct DbAddress {
	val: NonZeroU32,
}
impl DbAddress {
	fn index(&self) -> usize {
		((self.val.get() - 1u32) as usize) << 3
	}
}
impl Clone for DbAddress {
	fn clone(&self) -> DbAddress {
		DbAddress { val: self.val }
	}
}
impl Copy for DbAddress {}
impl TryFrom<usize> for DbAddress {
	type Error = usize;
	fn try_from(val: usize) -> Result<DbAddress, usize> {
		if val & 0b111usize == 0usize && val <= ChunkDb::MaxAddress {
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

pub struct ChunkDb {
	vec: Vec<u32>,
	pool: Option<DbAddress>,
}
impl ChunkDb {
	const ChunkPower: u32 = 3u32;
	const ChunkSize: usize = 1usize << ChunkDb::ChunkPower;
	const MaxAddress: usize = ((u32::max_value() as usize) - 1usize) << 3;
	pub fn new(cap: usize) -> ChunkDb {
		ChunkDb {
			vec: Vec::<u32>::with_capacity(cap >> ChunkDb::ChunkPower),
			pool: None,
		}
	}
	pub fn open<'a>(&'a mut self) -> ChunkDbWriter<'a> {
		let addr = self.allocate();
		ChunkDbWriter::<'a> {
			db: self as *mut ChunkDb,
			addr: addr,
			curr: addr,
			count: 1u32,
			_ph: PhantomData,
		}
	}
	pub unsafe fn retrieve<'a, 'b: 'a>(&'b self, addr: DbAddress) -> ChunkDbReference<'b> {
		ChunkDbReference::<'b> {
			db: self,
			addr: addr,
		}
	}
	fn allocate(&mut self) -> DbAddress {
		match self.pool {
			Some(x) => {
				let mt = unsafe { ChunkDb::follower_mut(x, &mut self.vec) };
				self.pool = *mt;
				*mt = None;
				x
			},
			None => {
				let len = self.vec.len();
				self.vec.resize(len + ChunkDb::ChunkSize, unsafe { mem::transmute::<Option<DbAddress>, u32>(None) });
				DbAddress::try_from(len).expect("Database capacity exceeded")
			}
		}
	}
	pub fn deallocate(&mut self, addr: DbAddress) {
		let last = self.pool;
		let mut curr = addr;
		loop {
			let mt = unsafe { ChunkDb::follower_mut(curr, &mut self.vec) };
			match mt {
				&mut Some(next) => {
					curr = next;
				},
				None => {
					*mt = last;
					self.pool = Some(addr);
					break
				}
			}
		}
	}
	unsafe fn follower_ref(addr: DbAddress, vec: &Vec<u32>) -> &Option<DbAddress> {
		let rf = vec.get_unchecked(addr.index() | (ChunkDb::ChunkSize - 1usize));
		mem::transmute::<&u32, &Option<DbAddress>>(rf)
	}
	unsafe fn follower_mut(addr: DbAddress, vec: &mut Vec<u32>) -> &mut Option<DbAddress> {
		let mt = vec.get_unchecked_mut(addr.index() | (ChunkDb::ChunkSize - 1usize));
		mem::transmute::<&mut u32, &mut Option<DbAddress>>(mt)
	}
}

pub struct ChunkDbWriter<'a> {
	db: *mut ChunkDb,
	addr: DbAddress,
	curr: DbAddress,
	count: u32,
	_ph: PhantomData<&'a mut ChunkDb>
}
impl<'a> ChunkDbWriter<'a> {
	pub fn write(&mut self, val: u32) {
		let db = unsafe { &mut *self.db };
		if self.count & ((ChunkDb::ChunkSize - 1usize) as u32) == ((ChunkDb::ChunkSize - 1usize) as u32) {
			let next = db.allocate();
			*unsafe { ChunkDb::follower_mut(self.curr, &mut db.vec) } = Some(next);
			self.curr = next;
			self.count += 1u32;
		}
		let index = self.curr.index() | ((self.count as usize) & (ChunkDb::ChunkSize - 1usize));
		unsafe { *db.vec.get_unchecked_mut(index) = val; }
		self.count += 1u32;
	}
	pub fn close(self) -> DbAddress {
		let db = unsafe { &mut *self.db };
		unsafe { *db.vec.get_unchecked_mut(self.addr.index()) = self.count; }
		let md = ManuallyDrop::<ChunkDbWriter<'a>>::new(self);
		md.addr
    }
    pub fn iter<'b: 'c, 'c>(&'b self) -> ChunkDbIterator<'c> where 'a: 'b {
        let db = unsafe { &*self.db };
        ChunkDbIterator::<'c> {
            db: db,
            addr: self.addr,
            count: 1u32,
            size: self.count
        }   
    }
}
impl<'a> Drop for ChunkDbWriter<'a> {
    fn drop(&mut self) {
		let db = unsafe { &mut *self.db };
		db.deallocate(self.addr);
    }
}

pub struct ChunkDbReference<'a> {
	db: &'a ChunkDb,
	addr: DbAddress,
}
impl<'a> ChunkDbReference<'a> {
	pub fn age<'b>(self) -> ChunkDbReference<'b> where 'a: 'b {
		self
    }
	pub fn iter<'b: 'c, 'c>(&'b self) -> ChunkDbIterator<'c> where 'a: 'b {
		let size = unsafe { *self.db.vec.get_unchecked(self.addr.index()) };
		ChunkDbIterator::<'c> {
			db: self.db,
			addr: self.addr,
			count: 1u32,
			size: size,
		}
	}
}

pub struct ChunkDbIterator<'a> {
	db: &'a ChunkDb,
	addr: DbAddress,
	count: u32,
	size: u32,
}
impl<'a> ChunkDbIterator<'a> {
	pub fn size(&self) -> usize {
		self.size as usize
	}
}
impl<'a> Iterator for ChunkDbIterator<'a> {
	type Item = &'a u32;
	fn next(&mut self) -> Option<&'a u32> {
		if self.count == self.size {
			None
		} else {
			let rf = unsafe { self.db.vec.get_unchecked(self.addr.index() | ((self.count as usize) & (ChunkDb::ChunkSize - 1usize))) };
			self.count += 1u32;
			if self.count & ((ChunkDb::ChunkSize - 1usize) as u32) == (ChunkDb::ChunkSize - 1usize) as u32 {
				let flw = unsafe { ChunkDb::follower_ref(self.addr, &self.db.vec) };
				match flw {
					&Some(next) => {
						self.addr = next;
						self.count += 1u32;
					},
					&None => (),
				}
			}
			Some(rf)
		}
	}
}

mod test {
	use rand::{self, Rng};
	use crate::{
		chunkdb::{ChunkDb, DbAddress},
	};

	#[derive(Debug)]
	enum TestOp {
		In(Vec<u32>),
		Forget(Vec<u32>),
		Out(usize),
	}

	fn run_chunk_database(vec: &Vec<TestOp>, db: &mut ChunkDb) -> Vec<(DbAddress, Vec<u32>)> {
		let mut records = Vec::<(DbAddress, Vec<u32>)>::new();
		for elem in vec {
			match elem {
				TestOp::In(record) => {
					let mut dbw = db.open();
					for u in record {
						dbw.write(*u);
					}
					records.push((dbw.close(), record.clone()));
				},
				TestOp::Forget(record) => {
					let mut dbw = db.open();
					for u in record {
						dbw.write(*u);
					}
				},
				TestOp::Out(item) => {
					let (addr, _) = records.get(*item).unwrap();
					db.deallocate(*addr);
					records.remove(*item);
				},
			}
		}
		records
	}

	fn check_chunk_database(vec: &Vec<(DbAddress, Vec<u32>)>, db: &mut ChunkDb) {
		let mut array = Vec::<bool>::new();
		array.resize(db.vec.len() >> 3, false);
		for (addr, record) in vec {
			let mut curr = *addr;
			loop {
				let flag = array.get_mut(curr.index() >> 3).unwrap();
				assert!(!*flag);
				*flag = true;
				match unsafe { ChunkDb::follower_ref(curr, &mut db.vec) } {
					&Some(next) => curr = next,
					None => break,
				}
			}
			let rf = unsafe { db.retrieve(*addr) };
			let mut it1 = rf.iter();
			let mut it2 = record.iter();
			loop {
				match (it1.next(), it2.next()) {
					(Some(x), Some(y)) => assert!(x == y),
					(None, None) => break,
					_ => assert!(false),
				}
			}
		}
		let mut curr = db.pool;
		loop {
			match curr {
				Some(next) => {
					let flag = array.get_mut(next.index() >> 3).unwrap();
					assert!(!*flag);
					*flag = true;
					curr = *unsafe {ChunkDb::follower_ref(next, &mut db.vec) } ;
				},
				None => break,
			}
		}
		for x in array {
			assert!(x);
		}
	}

	#[test]
	fn generate_chunk_database() {
		let mut rng = rand::thread_rng();
		let mut size: usize = 0usize;
		let mut vec = Vec::<TestOp>::new();
		for _ in 0..10000 {
			let dir: bool = rng.gen();
			if dir || size == 0usize {
				let sz: usize = rng.gen_range(0usize, 100usize);
				let mut record = Vec::<u32>::new();
				for _ in 0..sz {
					record.push(rng.gen());
				}
				vec.push(TestOp::In(record));
				size += 1usize;
			} else {
				let fgt: bool = rng.gen();
				if fgt {
					let sz: usize = rng.gen_range(0usize, 100usize);
					let mut record = Vec::<u32>::new();
					for _ in 0..sz {
						record.push(rng.gen());
					}
					vec.push(TestOp::Forget(record));
				} else {
					let x = rng.gen_range(0usize, size);
					vec.push(TestOp::Out(x));
					size -= 1usize;
				}
			}
		}
		let mut db = ChunkDb::new(2usize);
		let recs = run_chunk_database(&vec, &mut db);
		check_chunk_database(&recs, &mut db);
	}
}