use std::{
    collections::{BTreeMap},
};

use crate::{
    basic::{ClauseIndex},
    checkerdb::{Chain},
};

pub struct ChainHolder {
    vec: Vec<ClauseIndex>,
    index: BTreeMap<ClauseIndex, Option<(usize, usize)>>,
    current: Option<usize>,
}
impl ChainHolder {
    pub fn new() -> ChainHolder {
        ChainHolder {
            vec: Vec::new(),
            index: BTreeMap::new(),
            current: None,
        }
    }
    pub fn push_id(&mut self, id: ClauseIndex) {
        self.vec.push(id);
    }
    pub fn open_chain(&mut self) {
        self.current = Some(self.vec.len())
    }
    pub fn push_chain(&mut self, id: ClauseIndex) {
        if let Some(begin) = self.current {
            self.index.insert(id, Some((begin, self.vec.len())));
        }
    }
    pub fn push_deletion(&mut self, id: ClauseIndex) {
        self.index.insert(id, None);
    }
    pub fn clear(&mut self) {
        self.vec.clear();
        if self.current.is_some() {
            self.index.clear();
        }
        self.current = None;
    }
    pub fn chain(&self) -> Chain<'_> {
        Chain::from(&self.vec[..])
    }
    pub fn spec(&self, lat: ClauseIndex) -> Option<Option<Chain<'_>>> {
        match self.index.get(&lat) {
            None => Some(None),
            Some(&None) => None,
            Some(&Some((begin, end))) => Some(Some(Chain::from(&self.vec[begin .. end]))),
        }
    }
    pub fn deletions<'a>(&'a self) -> impl 'a + Iterator<Item = &ClauseIndex> {
        self.index.iter().filter_map(|(k, v)| match v {
            Some(_) => None,
            None => Some(k),
        })
    }
}