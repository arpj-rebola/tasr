use std::{
    mem::{self},
};

use crate::{
    clausedb::{ClauseIndex},
};

pub struct ChainDb {
    vec: Vec<u32>,
    index: Vec<usize>,
}
impl ChainDb {
    pub fn new() -> ChainDb {
        ChainDb {
            vec: Vec::<u32>::new(),
            index: Vec::<usize>::new(),
        }
    }
    pub fn open<'a, 'b: 'a>(&'b mut self, optid: Option<ClauseIndex>) -> Option<ChainDbWriter<'a>> {
        let index = optid.map(|id| id.index() + 1usize).unwrap_or(0usize);
        if self.index.len() >= index {
            self.vec.resize((index >> 1) + 1usize, 0u32);
        }
        if unsafe { *self.index.get_unchecked(index) == 0usize } {
            let origin = self.vec.len();
            self.vec.push(0u32);
            Some(ChainDbWriter::<'a> {
                db: self,
                index: index,
                origin: origin,
            })
        } else {
            None
        }
    }
    pub fn exists(&self, id: ClauseIndex) -> bool {
        match self.index.get(id.index() + 1usize) {
            None | Some(0usize) => false,
            _ => true,
        }
    }
    pub fn retrieve<'a, 'b: 'a>(&'b self, id: Option<ClauseIndex>) -> Option<&'a [ClauseIndex]> {
        let index = id.map(|n| n.index() + 1usize).unwrap_or(0usize);
        let origin = *self.index.get(index)?;
        if origin == 0usize {
            None
        } else {
            let length = unsafe { *self.vec.get_unchecked(origin - 1usize) as usize };
            let slice = &self.vec[origin..(origin + length)];
            unsafe { Some(mem::transmute::<&[u32], &[ClauseIndex]>(slice)) }
        }
    }
    pub fn extract(&self, id: Option<ClauseIndex>) -> Vec<ClauseIndex> {
        let mut vec = Vec::<ClauseIndex>::new();
        match self.retrieve(None) {
            Some(chn) => for cid in chn {
                vec.push(*cid);
            },
            None => (),
        }
        if id.is_some() {
            match self.retrieve(id) {
                Some(chn) => for cid in chn {
                    vec.push(*cid);
                },
                None => (),
            }
        }
        vec
    }
    pub fn clear(&mut self) {
        self.vec.clear();
        self.index.clear();
    }
}

pub struct ChainDbWriter<'a> {
    db: &'a mut ChainDb,
    index: usize,
    origin: usize,
}
impl<'a> ChainDbWriter<'a> {
    pub fn write(&mut self, id: ClauseIndex) {
        self.db.vec.push(id.index() as u32);
    }
    pub fn close(mut self) {
        let len = (self.db.vec.len() - self.origin - 1usize) as u32;
        unsafe { *self.db.vec.get_unchecked_mut(self.origin) = len; }
        unsafe { *self.db.index.get_unchecked_mut(self.index) = self.origin };
    }
}
impl<'a> Drop for ChainDbWriter<'a> {
    fn drop(&mut self) {
        self.db.vec.truncate(self.origin);
    }
}