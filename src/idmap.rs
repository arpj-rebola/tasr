use std::{
    convert::{TryFrom},
    collections::{BTreeSet, BTreeMap, btree_map::Entry},
};

use crate::{
    basic::{ClauseIndex},
    io::{FilePosition},
    checkerdb::{ClauseAddress},
};

pub struct IndexMapping {
    mapping: BTreeMap<ClauseIndex, (ClauseIndex, ClauseAddress, FilePosition)>,
    free: BTreeSet<ClauseIndex>,
    next: ClauseIndex,
}
impl IndexMapping {
    pub fn new() -> IndexMapping {
        IndexMapping {
            mapping: BTreeMap::<ClauseIndex, (ClauseIndex, ClauseAddress, FilePosition)>::new(),
            free: BTreeSet::new(),
            next: ClauseIndex::try_from(1i64).unwrap(),
        }
    }
    pub fn map(&mut self, oid: ClauseIndex, addr: ClauseAddress, pos: FilePosition) -> Option<ClauseIndex> {
        let entry = self.mapping.entry(oid);
        match entry {
            Entry::Vacant(vac) => {
                let opt_nid = self.free.pop_first();
                let nid = if let Some(nid) = opt_nid {
                    nid
                } else {
                    let nid = self.next;
                    self.next = self.next.next();
                    nid
                };
                vac.insert((nid, addr, pos));
                Some(nid)
            },
            Entry::Occupied(_) => None,
        }
    }
    pub fn id(&self, oid: ClauseIndex) -> Option<ClauseIndex> {
        let (nid, _, _) = self.mapping.get(&oid)?;
        Some(*nid)
    }
    pub fn unmap(&mut self, oid: ClauseIndex) -> Option<(ClauseIndex, ClauseAddress, FilePosition)> {
        let (nid, addr, pos) = self.mapping.remove(&oid)?;
        self.free.insert(nid);
        Some((nid, addr, pos))
    }
    pub fn extract(&self) -> Vec<(ClauseIndex, ClauseAddress, FilePosition)> {
        self.mapping.values().copied().collect()
    }
}

