use std::{
    num::{NonZeroU32},
    mem::{self, MaybeUninit, ManuallyDrop},
};

#[derive(Copy, Clone, Debug)]
pub struct DatabaseAddress {
    main: u8,
    sub: NonZeroU32,
}
impl DatabaseAddress {
    pub fn new(main: u8, sub: NonZeroU32) -> DatabaseAddress {
        DatabaseAddress { main: main, sub: sub }
    }
    pub fn serialize(self) -> (u32, u32) {
        (self.main as u32, self.sub.get() as u32)
    }
}

struct ChunkDatabase {
    array: Vec<u32>,
    free: Option<NonZeroU32>,
    score: u32,
}
impl ChunkDatabase {
    const ThresholdScore: u32 = 1024u32;
    fn new(cap: Option<usize>) -> ChunkDatabase {
        let score = cap.unwrap_or(u32::max_value() as usize).min((u32::max_value() - 1u32) as usize) as u32;
        ChunkDatabase {
            array: Vec::new(),
            free: None,
            score: score,
        }
    }
    #[inline(always)]
    fn full(&self) -> bool {
        self.score == 0u32
    }
    unsafe fn allocate(&mut self) -> NonZeroU32 {
        self.score -= 1u32;
        if let Some(addr) = self.free {
            let slot = ChunkDatabase::next_mut(&mut self.array, addr);
            self.free = *slot;
            *slot = None;
            addr
        } else {
            let len = self.array.len();
            self.array.resize(len + 8usize, mem::transmute::<Option<NonZeroU32>, u32>(None));
            let addr = NonZeroU32::new_unchecked((len >> 3) as u32 + 1u32);
            addr
        }
    }
    unsafe fn deallocate(&mut self, addr: NonZeroU32) {
        let last = {
            let mut current = addr;
            loop { 
                self.score += 1u32;
                match ChunkDatabase::next(&self.array, current) {
                    Some(next) => current = *next,
                    None => break current,
                }
            }
        };
        *ChunkDatabase::next_mut(&mut self.array, last) = self.free;
        self.free = Some(addr);
    }
    #[inline(always)]
    fn address(pos: usize) -> NonZeroU32 {
        unsafe { NonZeroU32::new_unchecked(((pos >> 3) + 1usize) as u32) }
    }
    #[inline(always)]
    fn go(addr: NonZeroU32) -> usize {
        ((addr.get() - 1u32) as usize) << 3
    }
    #[inline]
    unsafe fn next(vec: &Vec<u32>, addr: NonZeroU32) -> &Option<NonZeroU32> {
        mem::transmute::<&u32, &Option<NonZeroU32>>(vec.get_unchecked(ChunkDatabase::go(addr)))
    }
    #[inline]
    unsafe fn next_mut(vec: &mut Vec<u32>, addr: NonZeroU32) -> &mut Option<NonZeroU32> {
        mem::transmute::<&mut u32, &mut Option<NonZeroU32>>(vec.get_unchecked_mut(ChunkDatabase::go(addr)))
    }
    unsafe fn retrieve<'a, 'b: 'a>(&'b self, addr: NonZeroU32) -> Record<'a> {
        Record::<'a> {
            cdb: self,
            addr: addr,
        }
    }
    unsafe fn copy_record<'a, I>(&mut self, iter: I) -> (NonZeroU32, usize) where
        I: Iterator<Item = &'a u32>
    {
        let addr = self.allocate();
        let mut count = 0u32;
        let mut pos = ChunkDatabase::go(addr) + 1usize;
        for &elem in iter {
            if pos & 0b111usize == 0b111usize {
                let next = self.allocate();
                *self.array.get_unchecked_mut(pos & !0b111usize) = mem::transmute::<Option<NonZeroU32>, u32>(Some(next));
                pos = ChunkDatabase::go(next);
            }
            pos += 1usize;
            count += 1u32;
            *self.array.get_unchecked_mut(pos) = elem;
        }
        *self.array.get_unchecked_mut(ChunkDatabase::go(addr) + 1usize) = count;
        (addr, pos)
    }
}

pub struct Database {
    cdbs: [MaybeUninit<ChunkDatabase>; 256],
    active: usize,
    length: usize,
    capacity: Option<usize>,
}
impl Database {
    pub fn new() -> Database {
        Database::make(None)
    }
    pub fn with_capacity(cap: usize) -> Database {
        Database::make(Some(cap))
    }
    fn make(cap: Option<usize>) -> Database {
        let mut cdbs: [MaybeUninit<ChunkDatabase>; 256] = unsafe { MaybeUninit::uninit().assume_init() };
        cdbs[0] = MaybeUninit::new(ChunkDatabase::new(cap));
        Database {
            cdbs: cdbs,
            active: 0usize,
            length: 1usize,
            capacity: cap
        }
    }
    pub fn open<'a, 'b: 'a>(&'b mut self) -> DatabaseWriter<'a> {
        let active = if unsafe { self.cdbs[self.active].get_ref().full() } {
            self.request_cdb(ChunkDatabase::ThresholdScore)
        } else {
            self.active
        };
        let addr = unsafe { self.cdbs[active].get_mut().allocate() };
        DatabaseWriter::<'a> {
            db: self,
            active: active,
            pos: ChunkDatabase::go(addr) + 1usize,
            count: 0u32,
            addr: addr,
        }
    }
    pub unsafe fn retrieve<'a, 'b: 'a>(&'b self, addr: DatabaseAddress) -> Record<'a> {
        self.cdbs[addr.main as usize].get_ref().retrieve(addr.sub)
    }
    pub unsafe fn iterator_unsafe(&self, addr: DatabaseAddress) -> UnsafeRecordIterator {
        let rf = &self.cdbs[addr.main as usize].get_ref().array;
        let pos = ChunkDatabase::go(addr.sub) + 1usize;
        let count = *rf.get_unchecked(pos);
        UnsafeRecordIterator {
            array: rf,
            count: count,
            pos: pos,
        }
    }
    pub unsafe fn remove(&mut self, addr: DatabaseAddress) {
        self.cdbs[addr.main as usize].get_mut().deallocate(addr.sub)
    }
    fn request_cdb(&mut self, cap: u32) -> usize {
        let mut best_score = 0u32;
        let mut best_index = 0usize;
        for n in 0usize .. self.length {
            let score = unsafe { self.cdbs[n].get_mut().score };
            if score > best_score {
                best_score = score;
                best_index = n;
            }
        }
        if best_score < cap && self.length < 256usize {
            self.cdbs[self.length] = MaybeUninit::new(ChunkDatabase::new(self.capacity));
            best_index = self.length;
            self.length += 1usize;
        }
        if unsafe { self.cdbs[best_index].get_ref().score < cap } {
            Database::capacity_exceeded();
        }
        best_index
    }
    fn capacity_exceeded() -> ! {
        panick!("database capacity exceeded", lock, {
            append!(lock, "Could not allocate database space.");
        })
    }
    fn length_exceeded() -> ! {
        panick!("record capacity exceeded", lock, {
            append!(lock, "A record exceeded the maximum record length in the database.");
        })
    }
}
impl Drop for Database {
    fn drop(&mut self) {
        for n in 0usize .. self.length {
            let mut mu = MaybeUninit::<ChunkDatabase>::uninit();
            mem::swap(&mut mu, &mut self.cdbs[n]);
            unsafe { mu.assume_init(); }
        }
    }
}

#[derive(Copy, Clone)]
pub struct Record<'a> {
    cdb: &'a ChunkDatabase,
    addr: NonZeroU32,
}
impl<'a> Record<'a> {
    #[inline]
    pub fn length(&self) -> usize {
        unsafe { *self.cdb.array.get_unchecked(ChunkDatabase::go(self.addr) + 1usize) as usize }
    }
    pub fn get(&self, n: usize) -> Option<&'a u32> {
        if n < self.length() {
            let chunk = (n + 1usize) / 7usize;
            let pos = ((n + 1usize) % 7usize) + 1usize;
            let mut addr = self.addr;
            for _ in 0 .. chunk {
                addr = unsafe { *ChunkDatabase::next(&self.cdb.array, addr).as_ref().unwrap() };
            }
            unsafe { Some(self.cdb.array.get_unchecked(ChunkDatabase::go(addr) + pos)) }
        } else {
            None
        }
    }
}
impl<'a> IntoIterator for Record<'a> {
    type Item = &'a u32;
    type IntoIter = RecordIterator<'a>;
    fn into_iter(self) -> RecordIterator<'a> {
        let pos = ChunkDatabase::go(self.addr) + 1usize;
        let count = *unsafe { self.cdb.array.get_unchecked(pos) };
        RecordIterator::<'a> {
            cdb: self.cdb,
            count: count,
            pos: pos,
        }
    }
}
impl<'a, 'b: 'a> IntoIterator for &'a Record<'b> {
    type Item = &'a u32;
    type IntoIter = RecordIterator<'a>;
    fn into_iter(self) -> RecordIterator<'a> {
        let pos = ChunkDatabase::go(self.addr) + 1usize;
        let count = *unsafe { self.cdb.array.get_unchecked(pos) };
        RecordIterator::<'a> {
            cdb: self.cdb,
            count: count,
            pos: pos,
        }
    }
}

pub struct RecordIterator<'a> {
    cdb: &'a ChunkDatabase,
    count: u32,
    pos: usize,
}
impl<'a> Iterator for RecordIterator<'a> {
    type Item = &'a u32;
    fn next(&mut self) -> Option<&'a u32> {
        if self.count == 0u32 {
            None
        } else {
            if self.pos & 0b111usize == 0b111usize {
                self.pos = unsafe { ChunkDatabase::go(*ChunkDatabase::next(&self.cdb.array, ChunkDatabase::address(self.pos)).as_ref().unwrap()) };
            }
            self.pos += 1usize;
            self.count -= 1u32;
            self.cdb.array.get(self.pos)
        }
    }
}

pub struct UnsafeRecordIterator {
    array: *const Vec<u32>,
    count: u32,
    pos: usize,
}
impl UnsafeRecordIterator {
    pub unsafe fn next(&mut self) -> Option<u32> {
        if self.count == 0u32 {
            None
        } else {
            let rf = &*self.array;
            if self.pos & 0b111usize == 0b111usize {
                self.pos = ChunkDatabase::go(*ChunkDatabase::next(rf, ChunkDatabase::address(self.pos)).as_ref().unwrap());
            }
            self.pos += 1usize;
            self.count -= 1u32;
            Some(*rf.get_unchecked(self.pos))
        }
    }
}

pub struct DatabaseWriter<'a> {
    db: &'a mut Database,
    active: usize,
    pos: usize,
    count: u32,
    addr: NonZeroU32,
}
impl<'a> DatabaseWriter<'a> {
    pub fn write(&mut self, elem: u32) {
        if self.count == u32::max_value() {
            Database::length_exceeded();
        }
        if self.pos & 0b111usize == 0b111usize {
            self.allocate();
        }
        self.pos += 1usize;
        self.count += 1u32;
        *unsafe { self.db.cdbs[self.active].get_mut().array.get_unchecked_mut(self.pos) } = elem;
    }
    pub fn close(self) -> DatabaseAddress {
        *unsafe { self.db.cdbs[self.active].get_mut().array.get_unchecked_mut(ChunkDatabase::go(self.addr) + 1usize) } = self.count;
        self.db.active = self.active;
        let md = ManuallyDrop::new(self);
        DatabaseAddress {
            main: md.active as u8,
            sub: md.addr,
        }
    }
    fn allocate(&mut self) {
        {
            let rf = unsafe { self.db.cdbs[self.active].get_mut() };
            if rf.full() {
                *unsafe { rf.array.get_unchecked_mut(ChunkDatabase::go(self.addr) + 1usize) } = self.count;
                self.reallocate();
            }
        }
        let rf = unsafe { self.db.cdbs[self.active].get_mut() };
        let next = unsafe { rf.allocate() };
        unsafe { *rf.array.get_unchecked_mut(self.pos & !0b111usize) = mem::transmute::<Option<NonZeroU32>, u32>(Some(next)); }
        self.pos = ChunkDatabase::go(next);
    }
    fn reallocate(&mut self) {
        let cap_raw = ((self.count as u64) + 1u64) / 7u64 + 1u64;
        let cap = ((u32::max_value() - 1u32) as u64).min(cap_raw) as u32;
        let newactive = self.db.request_cdb(cap);
        let oldcdb = unsafe { (*self.db.cdbs.as_mut_ptr().add(self.active)).get_mut() };
        let newcdb = unsafe { (*self.db.cdbs.as_mut_ptr().add(newactive)).get_mut() };
        let (newaddr, newpos) = unsafe { newcdb.copy_record(oldcdb.retrieve(self.addr).into_iter()) };
        unsafe { oldcdb.deallocate(self.addr); }
        self.active = newactive;
        self.pos = newpos;
        self.addr = newaddr;
    }
}
impl<'a> Drop for DatabaseWriter<'a> {
    fn drop(&mut self) {
        unsafe { self.db.cdbs[self.active].get_mut().deallocate(self.addr); }
        self.db.active = self.active;
    }
}

#[cfg(test)]
pub mod test {
	use rand::{self, Rng};
	use crate::{
        database::{Database, DatabaseAddress},
    };

	fn generate_record<R: Rng>(rng: &mut R, maxlength: usize) -> Vec<u32> {
        let length = rng.gen_range(0usize, maxlength + 1usize);
        let mut vec = Vec::new();
        for _ in 0 .. length {
            vec.push(rng.gen());
        }
        vec
    }
    
    #[test]
    fn test_database() {
        let mut rng = rand::thread_rng();
        let mut db = Database::with_capacity(10000usize);
        for _ in 0 .. 100 {
            let mut records = Vec::<(DatabaseAddress, Vec<u32>)>::new();
            for _ in 0 .. 1000 {
                let record = generate_record(&mut rng, 500usize);
                let mut wt = db.open();
                for &elem in &record {
                    wt.write(elem);
                }
                let addr = wt.close();
                records.push((addr, record));
            }
            for (addr, record) in &records {
                let mut dbit = unsafe { db.retrieve(*addr).into_iter() };
                let mut rcit = record.iter();
                loop { match (dbit.next(), rcit.next()) {
                    (Some(n), Some(m)) => assert!(n == m),
                    (None, None) => break,
                    _ => assert!(false),
                } }
                unsafe { db.remove(*addr); }
            }
            let len = db.length;
            for n in 0 .. len {
                unsafe { assert!(db.cdbs[n].get_ref().score as usize == db.capacity.unwrap()); }
            }
        }
    }
    
}