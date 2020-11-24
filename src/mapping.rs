use std::{
    collections::{BTreeSet, BTreeMap},
};

use crate::{
    basic::{ClauseIndex, MaybeClauseIndex, Literal, InstructionNumber},
};

/// Representation of a set of literals.
pub struct LiteralSet {
	vec: Vec<u8>,
}
impl LiteralSet {
    /// Creates a new `LiteralSet`.
	pub fn new() -> LiteralSet {
		let mut vec = Vec::<u8>::new();
		vec.push(0b0000_0000u8);
		LiteralSet { vec: vec }
    }
    /// Checks if a literal occurs in the set.
	pub fn check(&self, lit: Literal) -> bool {
		match self.vec.get(lit.index() >> 3) {
			Some(chunk) => {
				let mask = 1u8 << (lit.index() & 0b111usize);
				mask & chunk != 0u8
			},
			None => false,
		}
    }
    /// Introduces a literal in the set.
	pub fn set(&mut self, lit: Literal) {
		let ix = lit.index() >> 3;
		if self.vec.len() <= ix {
			self.vec.resize((ix << 1) + 1usize, 0u8);
		}
		let mask = 1u8 << (lit.index() & 0b111usize);
		*unsafe { self.vec.get_unchecked_mut(ix) } |= mask;
    }
    /// Removes a literal from the set.
	pub fn clear(&mut self, lit: Literal) {
		let ix = lit.index() >> 3;
		if let Some(chunk) = self.vec.get_mut(ix) {
			let mask = !(1u8 << (lit.index() & 0b111usize));
			*chunk &= mask;
		}
    }
    /// Checks if a literal occurs in the set, and then introduces that literal.
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
    /// Checks if a literal occurs in the set, and then removes that literal.
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

pub struct InstructionLoader {
    set: BTreeMap<ClauseIndex, InstructionNumber>,
}
impl InstructionLoader {
    pub fn new() -> InstructionLoader {
        InstructionLoader { set: BTreeMap::new() }
    }
    pub fn insert(&mut self, id: ClauseIndex, num: InstructionNumber) {
        self.set.insert(id, num);
    }
    pub fn remove(&mut self, id: ClauseIndex) {
        self.set.remove(&id);
    }
    pub fn extract(self) -> Vec<InstructionNumber> {
        let mut vec: Vec<InstructionNumber> = self.set.values().copied().collect();
        vec.sort();
        vec
    }
}

pub struct IndexMapping {
    mapping: Vec<Option<ClauseIndex>>,
    free: BTreeSet<ClauseIndex>,
    last: MaybeClauseIndex,
}
impl IndexMapping {
    pub fn new() -> IndexMapping {
        IndexMapping {
            mapping: Vec::with_capacity(1024usize),
            free: BTreeSet::new(),
            last: MaybeClauseIndex::new(None),
        }
    }
    pub fn map(&self, id: ClauseIndex) -> Option<ClauseIndex> {
        *self.mapping.get(id.index())?
    }
    pub fn allocate(&mut self, id: ClauseIndex) -> Option<ClauseIndex> {
        let index = id.index();
        if index >= self.mapping.len() {
            self.mapping.resize((index << 1) + 1usize, None);
        }
        let slot = unsafe { self.mapping.get_unchecked_mut(index) };
        match slot {
            None => {
                let pop = self.free.pop_first();
                let new = if pop.is_some() {
                    pop
                } else {
                    self.last = unsafe { self.last.succ() };
                    self.last.get()
                };
                *slot = new;
                new
            },
            Some(_) => None,
        }
    }
    pub fn deallocate(&mut self, id: ClauseIndex) {
        if let Some(slot) = self.mapping.get_mut(id.index()) {
            if slot.is_some() {
                self.free.insert(slot.take().unwrap());
            }
        }
    }
    pub fn take(&mut self, id: ClauseIndex) -> Option<ClauseIndex> {
        let opt = self.mapping.get_mut(id.index())?.take();
        if let Some(nid) = opt {
            self.free.insert(nid);
        }
        opt
    }
}

/// Representation of a set of clause indices.
pub struct IndexSet {
	vec: Vec<u8>,
}
impl IndexSet {
    /// Creates a new `IndexSet`.
	pub fn new() -> IndexSet {
		let mut vec = Vec::<u8>::new();
		vec.push(0b0000_0000u8);
		IndexSet { vec: vec }
    }
    /// Checks if a index occurs in the set.
	pub fn check(&self, id: ClauseIndex) -> bool {
		match self.vec.get(id.index() >> 3) {
			Some(chunk) => {
				let mask = 1u8 << (id.index() & 0b111usize);
				mask & chunk != 0u8
			},
			None => false,
		}
    }
    /// Introduces a index in the set.
	pub fn set(&mut self, id: ClauseIndex) {
		let ix = id.index() >> 3;
		if self.vec.len() <= ix {
			self.vec.resize((ix << 1) + 1usize, 0u8);
		}
		let mask = 1u8 << (id.index() & 0b111usize);
		*unsafe { self.vec.get_unchecked_mut(ix) } |= mask;
    }
    /// Removes a index from the set.
	pub fn clear(&mut self, id: ClauseIndex) {
		let ix = id.index() >> 3;
		if let Some(chunk) = self.vec.get_mut(ix) {
			let mask = !(1u8 << (id.index() & 0b111usize));
			*chunk &= mask;
		}
    }
    /// Checks if a index occurs in the set, and then introduces that index.
	pub fn check_set(&mut self, id: ClauseIndex) -> bool {
		let ix = id.index() >> 3;
		if self.vec.len() <= ix {
			self.vec.resize((ix << 1) + 1usize, 0u8);
		}
		let chunk = unsafe { self.vec.get_unchecked_mut(ix) };
		let mask = 1u8 << (id.index() & 0b111usize);
		let result = mask & *chunk != 0u8;
		*chunk |= mask;
		result
    }
    /// Checks if a index occurs in the set, and then removes that index.
	pub fn check_clear(&mut self, id: ClauseIndex) -> bool {
		let ix = id.index() >> 3;
		if let Some(chunk) = self.vec.get_mut(ix) {
			let mask = 1u8 << (id.index() & 0b111usize);
			let result = mask & *chunk != 0u8;
			*chunk &= !mask;
			result
		} else {
			false
		}
	}
}

#[cfg(test)]
pub mod test {
	use rand::{self, Rng};
	use crate::{
        basic::{Literal, ClauseIndex, test::{generate_literal, generate_external_literal, generate_index}},
        mapping::{LiteralSet, IndexSet, IndexMapping},
    };

    pub fn generate_clause<R: Rng>(rng: &mut R, maxsize: usize, limit: u32) -> Vec<Literal> {
        let size = rng.gen_range(0usize, maxsize + 1usize);
        let mut vec = Vec::<Literal>::new();
        for _ in 0..size {
            let lit = generate_literal(rng, Some(limit));
            vec.push(lit);
        }
        vec.sort_by(|&lit1, &lit2| lit1.index().cmp(&lit2.index()));
        vec.dedup();
        vec
    }
    pub fn generate_index_set<R: Rng>(rng: &mut R, maxsize: usize, limit: u32) -> Vec<ClauseIndex> {
        let size = rng.gen_range(0usize, maxsize + 1usize);
        let mut vec = Vec::<ClauseIndex>::new();
        for _ in 0..size {
            let id = generate_index(rng, Some(limit));
            vec.push(id);
        }
        vec.sort();
        vec.dedup();
        vec
    }

    #[test]
    fn test_literal_set() {
        let mut rng = rand::thread_rng();
        let mut set = LiteralSet::new();
        for _ in 0..1000 {
            let cls = generate_clause(&mut rng, 400usize, 1000000u32);
            for &lit in &cls {
                assert!(!set.check(lit));
                set.set(lit);
                assert!(set.check(lit));
                set.clear(lit);
                assert!(!set.check(lit));
                assert!(!set.check_set(lit));
                assert!(set.check_set(lit));
                assert!(set.check_clear(lit));
                assert!(!set.check_clear(lit));
                set.set(lit);
            }
            for _ in 0..400 {
                let ext = generate_external_literal(&mut rng, &cls, Some(1000000u32));
                assert!(!set.check(ext));
            }
            for &lit in &cls {
                assert!(set.check_clear(lit));
            }
        }
    }

    #[test]
    fn test_index_mapping() {
        let mut rng = rand::thread_rng();
        let mut mapping = IndexMapping::new();
        for _ in 0..1000 {
            let mut oids = generate_index_set(&mut rng, 2000usize, 1000000u32);
            let mut vec = Vec::<(ClauseIndex, ClauseIndex)>::new();
            for &oid in &oids {
                let id = mapping.allocate(oid).unwrap();
                vec.push((oid, id));
            }
            for &(oid, id) in &vec {
                assert!(mapping.map(oid).unwrap() == id);
            }
            let mut deleted = Vec::<ClauseIndex>::new();
            let len = oids.len() / 2;
            let mut old = Vec::<ClauseIndex>::new();
            for _ in 0 .. len {
                let i = rng.gen_range(0usize, oids.len());
                let oid = oids.remove(i);
                let (oidx, idx) = vec.remove(i);
                let id = mapping.map(oid).unwrap();
                assert!(oid == oidx);
                assert!(id == idx);
                mapping.deallocate(oid);
                deleted.push(idx);
                old.push(oidx);
            }
            deleted.sort();
            let mut diter = deleted.iter();
            let mut oiter = old.iter();
            loop { match (diter.next(), oiter.next()) {
                (Some(&id), Some(&oid)) => {
                    let nid = mapping.allocate(oid).unwrap();
                    assert!(nid == id);
                },
                (None, None) => break,
                _ => assert!(false),
            } }
            for &oid in &old {
                mapping.deallocate(oid)
            }
            for &oid in &oids {
                mapping.deallocate(oid)
            }
        }
    }

    // #[test]
    // fn test_clause_marker() {
    //     let mut rng = rand::thread_rng();
    //     let mut marker = ClauseMarker::new();
    //     for _ in 0..1000 {
    //         let fm = generate_index_set(&mut rng, 400usize, 1000000u32);
    //         for &id in &fm {
    //             assert!(marker.mark(id).is_none());
    //         }
    //         for &id in &fm {
    //             assert!(marker.mark(id).is_some());
    //         }
    //         for &id in &fm {
    //             marker.unmark(id)
    //         }
    //     }
    // }

}