use std::{
    convert::{TryFrom},
    mem::{self},
};

use crate::{
    bst::{BinarySearchTree},
    clausedb::{ClauseIndex},
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
    }
}
impl<'a> Drop for ChainDbWriter<'a> {
    fn drop(&mut self) {
        self.db.vec.truncate(self.origin);
    }
}