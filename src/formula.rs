use std::{
    collections::{BTreeSet},
};

use crate::{
    basic::{ClauseIndex},
    checkerdb::{ClauseAddress},
};

pub struct Formula {
    vec: Vec<Option<ClauseAddress>>,
    ids: BTreeSet<ClauseIndex>,
}
impl Formula {
    pub fn new() -> Formula {
        Formula {
            vec: Vec::new(),
            ids: BTreeSet::new(),
        }
    }
    pub fn insert(&mut self, id: ClauseIndex, addr: ClauseAddress) -> Option<()> {
        let index = id.index();
        if index >= self.vec.len() {
            self.vec.resize((index + 1usize) << 1, None);
        }
        let slot = unsafe { self.vec.get_unchecked_mut(index) };
        if let None = slot {
            *slot = Some(addr);
            self.ids.insert(id);
            Some(())
        } else {
            None
        }
    }
    pub fn take(&mut self, id: ClauseIndex) -> Option<ClauseAddress> {
        let index = id.index();
        let slot = self.vec.get_mut(index)?;
        if slot.is_none() {
            None
        } else {
            self.ids.remove(&id);
            slot.take()
        }
    }
    pub fn get(&self, id: ClauseIndex) -> Option<ClauseAddress> {
        *self.vec.get(id.index())?
    }
    pub fn clauses<'a>(&'a self) -> impl 'a + Iterator<Item = ClauseIndex> {
        self.ids.iter().copied()
    }
}