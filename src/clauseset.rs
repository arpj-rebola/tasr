use std::{
    mem::{self},
};

use crate::{
    basic::{Literal},
    model::{Model, ModelValue},
    checkerdb::{CheckerDb, Clause, ClauseAddress},
};

pub struct MainHasher {
    sum: usize,
    prod: usize,
    xor: usize,
}
impl MainHasher {
    fn new() -> MainHasher {
        MainHasher {
            sum: 0usize,
            prod: 1usize,
            xor: 0usize,
        }
    }
    fn hash(&mut self, x: u32) {
        self.sum = self.sum.wrapping_add(x as usize);
        self.prod = self.prod.wrapping_mul(x as usize | 1usize);
        self.xor ^= x as usize;
    }
    fn get(self) -> usize {
        1023usize.wrapping_mul(self.sum).wrapping_add(self.prod ^ self.xor)
    }
    fn hash_clause(mut self, clause: Clause) -> usize {
        for &lit in clause {
            unsafe { self.hash(mem::transmute::<Literal, u32>(lit)); } 
        }
        self.get()
    }
}

struct SubHasher {
    val: u32,
}
impl SubHasher {
    fn new() -> SubHasher {
        SubHasher { val: 0u32 }
    }
    fn hash(&mut self, x: u32) {
        self.val ^= x;
        self.val ^= x.rotate_left(19u32);
        self.val ^= x.rotate_right(23u32);
        self.val ^= 4222234741u32;
    }
    fn get(self) -> u32 {
        self.val
    }
    fn hash_clause(mut self, clause: Clause) -> u32 {
        for &lit in clause {
            unsafe { self.hash(mem::transmute::<Literal, u32>(lit)); }
        }
        self.get()
    }
}

pub struct ClauseSet {
    table: Vec<Vec<(ClauseAddress, u32)>>,
    model: Model,
}
impl ClauseSet {
    const Width: usize = (1usize << 16) - 1usize;
    pub fn new() -> ClauseSet {
        let mut table = Vec::with_capacity(ClauseSet::Width + 1usize);
        table.resize_with(ClauseSet::Width + 1usize, || Vec::with_capacity(4usize));
        ClauseSet {
            table: table,
            model: Model::new(),
        }
    }
    pub fn insert(&mut self, db: &CheckerDb, addr: ClauseAddress) {
        let clause = db.retrieve_clause(addr);
        let main = MainHasher::new().hash_clause(clause) & ClauseSet::Width;
        let sub = SubHasher::new().hash_clause(clause);
        let row = unsafe { self.table.get_unchecked_mut(main) };
        row.push((addr, sub));
    }
    pub fn take(&mut self, db: &CheckerDb, clause: Clause) -> Option<ClauseAddress> {
        let main = MainHasher::new().hash_clause(clause) & ClauseSet::Width;
        let sub = SubHasher::new().hash_clause(clause);
        let length = clause.length();
        for &lit in clause {
            self.model.set(lit);
        }
        let row = unsafe { self.table.get_unchecked_mut(main) };
        let mut iter = row.iter().enumerate();
        let model = &mut self.model;
        let opt_i = loop { match iter.next() {
            Some((n, &(target_addr, target_sub))) => if target_sub == sub {
                let target_clause = db.retrieve_clause(target_addr);
                if target_clause.length() == length {
                    if target_clause.into_iter().all(|&lit| match model.value(lit) {
                        ModelValue::True => true,
                        _ => false,
                    }) {
                        break Some(n)
                    }
                }
            },
            None => break None,
        } };
        for &lit in clause {
            self.model.clear(lit);
        }
        let (addr, _) = row.swap_remove(opt_i?);
        Some(addr)
    }
    pub fn deallocate_clauses(&mut self, db: &mut CheckerDb) {
        for row in &self.table {
            for (addr, _) in row {
                db.deallocate_clause(*addr)
            }
        }
    }
}