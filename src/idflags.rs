use std::{
    collections::{BTreeMap, btree_map::Entry},
};

use crate::{
    basic::{ClauseIndex},
};

#[repr(transparent)]
#[derive(Copy, Clone)]
pub struct ClauseFlags {
    val: u8
}
impl ClauseFlags {
    const MchainSpec: u8 = 0b0000_0001u8;
    const CheckSchedule: u8 = 0b0000_0010u8;
    #[inline(always)]
    pub fn new() -> ClauseFlags {
        ClauseFlags { val: 0u8 }
    }
    #[inline(always)]
    pub fn has_mchain_spec(&self) -> bool {
        self.val & ClauseFlags::MchainSpec != 0u8
    }
    #[inline(always)]
    pub fn set_mchain_spec(&mut self) {
        self.val |= ClauseFlags::MchainSpec;
    }
    #[inline(always)]
    pub fn clear_mchain_spec(&mut self) {
        self.val &= !ClauseFlags::MchainSpec;
    }
    #[inline(always)]
    pub fn has_check_schedule(&self) -> bool {
        self.val & ClauseFlags::CheckSchedule != 0u8
    }
    #[inline(always)]
    pub fn set_check_schedule(&mut self) {
        self.val |= ClauseFlags::CheckSchedule;
    }
    #[inline(always)]
    pub fn clear_check_schedule(&mut self) {
        self.val &= !ClauseFlags::CheckSchedule;
    }
}

pub struct ClauseIndexFlags<T> {
    map: BTreeMap<ClauseIndex, (T, ClauseFlags)>
}
impl<T> ClauseIndexFlags<T> {
    #[inline(always)]
    pub fn new() -> ClauseIndexFlags<T> {
        ClauseIndexFlags::<T> { map: BTreeMap::new() }
    }
    pub fn insert(&mut self, id: ClauseIndex, meta: T) -> Result<(), ()> {
        let entry = self.map.entry(id);
        match entry {
            Entry::Vacant(_) => {
                entry.or_insert((meta, ClauseFlags::new()));
                Ok(())
            },
            Entry::Occupied(_) => Err(()),
        }
    }
    #[inline(always)]
    pub fn contains(&self, id: ClauseIndex) -> bool {
        self.map.contains_key(&id)
    }
    #[inline(always)]
    pub fn flags(&self, id: ClauseIndex) -> Option<&ClauseFlags> {
        self.map.get(&id).map(|(_, f)| f)
    }
    #[inline(always)]
    pub fn flags_mut(&mut self, id: ClauseIndex) -> Option<&mut ClauseFlags> {
        self.map.get_mut(&id).map(|(_, f)| f)
    }
    #[inline(always)]
    pub fn meta(&self, id: ClauseIndex) -> Option<&T> {
        self.map.get(&id).map(|(meta, _)| meta)
    }
    #[inline]
    pub fn remove(&mut self, id: ClauseIndex) -> Result<(), ()> {
        if let Some(_) = self.map.remove(&id) {
            Ok(())
        } else {
            Err(())
        }
    }
    #[inline]
    pub fn take(&mut self, id: ClauseIndex) -> Option<(T, ClauseFlags)> {
        self.map.remove(&id)
    }
    #[inline]
    pub fn clauses(&self) -> impl Iterator<Item = (&ClauseIndex, &(T, ClauseFlags))> {
        self.map.iter()
    }
}
impl ClauseIndexFlags<u64> {
    pub fn instructions(&self) -> Vec<u64> {
        let mut vec = Vec::new();
        for (_, (num, _)) in self.map.iter() {
            vec.push(*num);
        }
        vec
    }
}
