use std::{
    convert::{TryFrom},
    mem::{self, ManuallyDrop},
};

use crate::{
    bst::{BinarySearchTree},
    basic::{ClauseIndex},
};

pub struct ChainDb {
    vec: Vec<u32>,
    index: BinarySearchTree<ClauseIndex, usize>,
}
impl ChainDb {
    pub fn new() -> ChainDb {
        ChainDb {
            vec: Vec::<u32>::new(),
            index: BinarySearchTree::<ClauseIndex, usize>::new(),
        }
    }
    pub fn open<'a, 'b: 'a>(&'b mut self, id: ClauseIndex) -> Option<ChainDbWriter<'a>> {
        match self.index.get(&id) {
            Some(_) => None,
            None => {
                let origin = self.vec.len();
                self.vec.push(0u32);
                Some(ChainDbWriter::<'a> {
                    db: self,
                    id: id,
                    origin: origin,
                })
            }
        }
    }
    pub fn exists(&self, id: ClauseIndex) -> bool {
        self.index.get(&id).is_some()
    }
    pub fn retrieve<'a, 'b: 'a>(&'b self, id: ClauseIndex) -> Option<&'a [ClauseIndex]> {
        let origin = *self.index.get(&id)?;
        let length = unsafe { *self.vec.get_unchecked(origin) as usize };
        let slice = &self.vec[(origin + 1usize)..(origin + 1usize + length)];
        unsafe { Some(mem::transmute::<&[u32], &[ClauseIndex]>(slice)) }
    }
    pub fn extract(&self, id: ClauseIndex) -> Option<Vec<ClauseIndex>> {
        Some(self.retrieve(id)?.iter().copied().collect())
    }
    pub fn clear(&mut self) {
        self.vec.clear();
        self.index.clear();
    }
}

pub struct ChainDbWriter<'a> {
    db: &'a mut ChainDb,
    id: ClauseIndex,
    origin: usize,
}
impl<'a> ChainDbWriter<'a> {
    pub fn write(&mut self, id: ClauseIndex) {
        unsafe { self.db.vec.push(mem::transmute::<ClauseIndex, u32>(id)); }
    }
    pub fn close(self) {
        let len = u32::try_from(self.db.vec.len() - self.origin - 1usize).expect("ChainDb record length exceeded.");
        unsafe { *self.db.vec.get_unchecked_mut(self.origin) = len; }
        self.db.index.insert(self.id, self.origin);
        ManuallyDrop::<ChainDbWriter<'a>>::new(self);
    }
}
impl<'a> Drop for ChainDbWriter<'a> {
    fn drop(&mut self) {
        self.db.vec.truncate(self.origin);
    }
}

#[cfg(test)]
mod test {
    use std::{
        convert::{TryFrom},
    };
	use rand::{self, Rng};
	use crate::{
        basic::{ClauseIndex},
		chaindb::{ChainDb},
    };
    
    #[test]
    fn test_chain_database() {
        let mut rng = rand::thread_rng();
        let mut chain = ChainDb::new();
        for _ in 0..100usize {
            let mut vec = Vec::<(ClauseIndex, Vec<ClauseIndex>)>::new();
            let size = rng.gen_range(0usize, 1000usize);
            for _ in 0..size {
                let index = ClauseIndex::try_from(rng.gen_range(1i64, 1000i64)).unwrap();
                let length = rng.gen_range(0usize, 100usize);
                match chain.open(index) {
                    Some(mut chw) => {
                        let mut found = false;
                        for (id, _) in &vec {
                            if id == &index {
                                found = true;
                            }
                        }
                        assert!(!found);
                        let mut record = Vec::<ClauseIndex>::new();
                        for _ in 0..length {
                            let id = ClauseIndex::try_from(rng.gen_range(1i64, 1000i64)).unwrap();
                            chw.write(id);
                            record.push(id);
                        }
                        chw.close();
                        vec.push((index, record));
                    },
                    None => {
                        let mut found = false;
                        for (id, _) in &vec {
                            if id == &index {
                                found = true;
                            }
                        }
                        assert!(found);
                    },
                }
            }
            for (index, record) in &vec {
                let chn = chain.retrieve(*index).unwrap();
                let mut it1 = chn.iter();
                let mut it2 = record.iter();
                loop { match (it1.next(), it2.next()) {
                    (Some(x), Some(y)) => assert!(x == y),
                    (None, None) => break,
                    _ => assert!(false),
                } }
            }
            chain.clear();
        }
    }

}
