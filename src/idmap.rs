use std::{
    convert::{TryFrom},
    collections::{BTreeSet, BTreeMap, btree_map::Entry},
    mem::{self},
};

use crate::{
    basic::{ClauseIndex},
    io::{FilePosition},
    checkerdb::{ClauseAddress},
};

// pub struct IndexMapping {
//     mapping: BTreeMap<ClauseIndex, (ClauseIndex, ClauseAddress, FilePosition)>,
//     free: BTreeSet<ClauseIndex>,
//     next: ClauseIndex,
// }
// impl IndexMapping {
//     pub fn new() -> IndexMapping {
//         IndexMapping {
//             mapping: BTreeMap::<ClauseIndex, (ClauseIndex, ClauseAddress, FilePosition)>::new(),
//             free: BTreeSet::new(),
//             next: ClauseIndex::try_from(1i64).unwrap(),
//         }
//     }
//     pub fn map(&mut self, oid: ClauseIndex, addr: ClauseAddress, pos: FilePosition) -> Option<ClauseIndex> {
//         let entry = self.mapping.entry(oid);
//         match entry {
//             Entry::Vacant(vac) => {
//                 let opt_nid = self.free.pop_first();
//                 let nid = if let Some(nid) = opt_nid {
//                     nid
//                 } else {
//                     let nid = self.next;
//                     self.next = self.next.next();
//                     nid
//                 };
//                 vac.insert((nid, addr, pos));
//                 Some(nid)
//             },
//             Entry::Occupied(_) => None,
//         }
//     }
//     pub fn id(&self, oid: ClauseIndex) -> Option<ClauseIndex> {
//         let (nid, _, _) = self.mapping.get(&oid)?;
//         Some(*nid)
//     }
//     pub fn unmap(&mut self, oid: ClauseIndex) -> Option<(ClauseIndex, ClauseAddress, FilePosition)> {
//         let (nid, addr, pos) = self.mapping.remove(&oid)?;
//         self.free.insert(nid);
//         Some((nid, addr, pos))
//     }
//     pub fn extract(&self) -> Vec<(ClauseIndex, ClauseAddress, FilePosition)> {
//         self.mapping.values().copied().collect()
//     }
// }

struct IndexMappingItem {
    oid: ClauseIndex,
    nid: ClauseIndex,
    addr: ClauseAddress,
    pos: FilePosition,
}

pub struct IndexMapping {
    table: Vec<Vec<IndexMappingItem>>,
    free: BTreeSet<ClauseIndex>,
    next: ClauseIndex,
}
impl IndexMapping {
    const Size: usize = (1usize << 18) - 1usize;
    pub fn new() -> IndexMapping {
        let mut vec = Vec::with_capacity(IndexMapping::Size + 1usize);
        vec.resize_with(IndexMapping::Size + 1usize, || Vec::with_capacity(3usize));
        IndexMapping {
            table: vec,
            free: BTreeSet::new(),
            next: ClauseIndex::try_from(1i64).unwrap(),
        }
    }
    pub fn map(&mut self, oid: ClauseIndex, addr: ClauseAddress, pos: FilePosition) -> Option<ClauseIndex> {
        let hash = unsafe { (mem::transmute::<ClauseIndex, u64>(oid) as usize) & IndexMapping::Size };
        let row = unsafe { self.table.get_unchecked_mut(hash) };
        if row.iter().all(|x| x.oid != oid) {
            let opt_nid = self.free.pop_first();
            let nid = if let Some(nid) = opt_nid {
                nid
            } else {
                let nid = self.next;
                self.next = self.next.next();
                nid
            };
            row.push(IndexMappingItem {
                oid: oid,
                nid: nid,
                addr: addr,
                pos: pos,
            });
            Some(nid)
        } else {
            None
        }
    }
    pub fn id(&self, oid: ClauseIndex) -> Option<ClauseIndex> {
        let hash = unsafe { (mem::transmute::<ClauseIndex, u64>(oid) as usize) & IndexMapping::Size };
        let item = unsafe { self.table.get_unchecked(hash).iter().find(|i| i.oid == oid)? };
        Some(item.nid)
    }
    pub fn unmap(&mut self, oid: ClauseIndex) -> Option<(ClauseIndex, ClauseAddress, FilePosition)> {
        let hash = unsafe { (mem::transmute::<ClauseIndex, u64>(oid) as usize) & IndexMapping::Size };
        let row = unsafe { self.table.get_unchecked_mut(hash) };
        let (n, _) = row.iter().enumerate().find(|(_, item)| item.oid == oid)?;
        let item = row.swap_remove(n);
        self.free.insert(item.nid);
        Some((item.nid, item.addr, item.pos))
    }
    pub fn extract(&self) -> Vec<(ClauseIndex, ClauseAddress, FilePosition)> {
        let mut vec = Vec::new();
        for row in &self.table {
            for item in row {
                vec.push((item.nid, item.addr, item.pos))
            }
        }
        vec
    }
}