use std::{
    mem::{self},
};

use crate::{
    basic::{Literal},
    database::{DatabaseAddress, Database, DatabaseWriter, Record, RecordIterator},
};

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct ClauseAddress {
    addr: DatabaseAddress
}
impl ClauseAddress {
    pub unsafe fn new(addr: DatabaseAddress) -> ClauseAddress {
        ClauseAddress { addr: addr }
    }
    pub unsafe fn get(&self) -> &DatabaseAddress {
        &self.addr
    }
}

pub struct ClauseDatabase;
impl ClauseDatabase {
    #[inline(always)]
    pub fn open<'a, 'b: 'a>(&self, db: &'b mut Database) -> ClauseWriter<'a> {
        ClauseWriter::<'a> { wt: db.open() }
    }
    #[inline(always)]
    pub unsafe fn retrieve<'a, 'b: 'a>(&self, db: &'b Database, addr: ClauseAddress) -> Clause<'a> {
        Clause::<'a> { rf: db.retrieve(addr.addr) }
    }
    pub unsafe fn remove(&self, db: &mut Database, addr: ClauseAddress) {
        db.remove(addr.addr)
    }
}

#[repr(transparent)]
pub struct ClauseWriter<'a> {
    wt: DatabaseWriter<'a>
}
impl<'a> ClauseWriter<'a> {
    #[inline(always)]
    pub fn write(&mut self, lit: Literal) {
        unsafe { self.wt.write(mem::transmute::<Literal, u32>(lit)) }
    }
    #[inline(always)]
    pub fn close(self) -> ClauseAddress {
        ClauseAddress { addr: self.wt.close() }
    }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct Clause<'a> {
    rf: Record<'a>
}
impl<'a> Clause<'a> {
    #[inline(always)]
    pub fn length(&self) -> usize {
        self.rf.length()
    }
}
impl<'a> IntoIterator for Clause<'a> {
    type Item = &'a Literal;
    type IntoIter = ClauseIterator<'a>;
    #[inline(always)]
    fn into_iter(self) -> ClauseIterator<'a> {
        ClauseIterator::<'a> { it: self.rf.into_iter() }
    }
}
impl<'a, 'b: 'a> IntoIterator for &'a Clause<'b> {
    type Item = &'a Literal;
    type IntoIter = ClauseIterator<'a>;
    #[inline(always)]
    fn into_iter(self) -> ClauseIterator<'a> {
        ClauseIterator::<'a> { it: self.rf.into_iter() }
    }
}

#[repr(transparent)]
pub struct ClauseIterator<'a> {
    it: RecordIterator<'a>,
}
impl<'a> Iterator for ClauseIterator<'a> {
    type Item = &'a Literal;
    #[inline(always)]
    fn next(&mut self) -> Option<&'a Literal> {
        self.it.next().map(|x| unsafe { mem::transmute::<&u32, &Literal>(x) })
    }
}