use std::{
    mem::{self},
};

use crate::{
    basic::{Literal},
    clause::{ClauseDatabase, ClauseAddress},
    database::{Database},
    mapping::{LiteralSet}
};

struct MainHash {
    sum: usize,
    prod: usize,
    xor: usize,
}
impl MainHash {
    fn new() -> MainHash {
        MainHash {
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
        1023usize.wrapping_mul(self.sum).wrapping_add(self.prod ^ self.xor) & ClauseSet::Width
    }
}

struct SubHash {
    val: u32,
}
impl SubHash {
    fn new() -> SubHash {
        SubHash { val: 0u32 }
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
}

pub struct ClauseSet {
    table: Vec<Vec<(ClauseAddress, u32)>>,
    clausedb: ClauseDatabase,
    lits: LiteralSet,
}
impl ClauseSet {
    const Width: usize = (1usize << 16) - 1usize;
    pub fn new() -> ClauseSet {
        let mut table = Vec::with_capacity(ClauseSet::Width);
        table.resize_with(ClauseSet::Width, || Vec::with_capacity(4usize));
        ClauseSet {
            table: table,
            clausedb: ClauseDatabase,
            lits: LiteralSet::new(),
        }
    }
    pub fn insert(&mut self, database: &Database, addr: ClauseAddress) {
        let clause = unsafe { self.clausedb.retrieve(database, addr) };
        let mut main = MainHash::new();
        let mut sub = SubHash::new();
        for lit in clause {
            main.hash(*unsafe { mem::transmute::<&Literal, &u32>(lit) });
            sub.hash(*unsafe { mem::transmute::<&Literal, &u32>(lit) });
        }
        let row = unsafe { self.table.get_unchecked_mut(main.get()) };
        row.push((addr, sub.get()));
    }
    pub fn remove(&mut self, database: &Database, clause: &[Literal]) -> Option<ClauseAddress> {
        let mut main = MainHash::new();
        let mut sub = SubHash::new();
        let length = clause.len();
        for &lit in clause {
            main.hash(unsafe { mem::transmute::<Literal, u32>(lit) });
            sub.hash(unsafe { mem::transmute::<Literal, u32>(lit) });
            self.lits.set(lit);
        }
        let subhash = sub.get();
        let row = unsafe { self.table.get_unchecked_mut(main.get()) };
        let mut iter = row.iter().enumerate();
        let lits_rf = &mut self.lits;
        let opti = loop {
            match iter.next() {
                Some((n, &(target_addr, target_sub))) => if target_sub == subhash {
                    let target_clause = unsafe { self.clausedb.retrieve(database, target_addr) };
                    if target_clause.length() == length {
                        if target_clause.into_iter().all(|&lit| lits_rf.check(lit)) {
                            break Some(n)
                        }
                    }
                },
                None => break None,
            }
        };
        for &lit in clause {
            self.lits.clear(lit);
        }
        Some(row.swap_remove(opti?).0)
    }
    pub fn delete_all(&mut self, database: &mut Database) {
        for row in &self.table {
            for &(addr, _) in row {
                unsafe { ClauseDatabase.remove(database, addr); }
            }
        }
    }
}