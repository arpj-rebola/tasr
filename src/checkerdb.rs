use std::{
    mem::{self},
    slice::{Iter as SliceIter},
};

use crate::{
    basic::{Variable, Literal, ClauseIndex},
    database::{DbAddress, Database},
};

pub struct CheckerDb {
    db: Database,
}
impl CheckerDb {
    pub fn new() -> CheckerDb {
        CheckerDb { db: Database::new() }
    }
    pub fn allocate_clause(&mut self, clause: Clause) -> ClauseAddress {
        let sl32 = unsafe { mem::transmute::<&[Literal], &[u32]>(clause.sl) };
        ClauseAddress::new(self.db.allocate32(sl32).unwrap_or_else(|| CheckerDb::capacity_exceeded("clause", clause.length())))
    }
    pub fn allocate_chain(&mut self, chain: Chain) -> ChainAddress {
        let sl64 = unsafe { mem::transmute::<&[ClauseIndex], &[u64]>(chain.sl) };
        ChainAddress::new(self.db.allocate64(sl64).unwrap_or_else(|| CheckerDb::capacity_exceeded("RUP chain", chain.length())))
    }
    pub fn allocate_witness(&mut self, witness: Witness) -> WitnessAddress {
        let sl64 = unsafe { mem::transmute::<&[(Variable, Literal)], &[u64]>(witness.sl) };
        WitnessAddress::new(self.db.allocate64(sl64).unwrap_or_else(|| CheckerDb::capacity_exceeded("witness substitution", witness.length())))
    }
    pub fn allocate_multichain(&mut self, mchain: Multichain) -> MultichainAddress {
        let sl128 = unsafe { mem::transmute::<&[(Option<ClauseIndex>, Option<ChainAddress>)], &[u128]>(mchain.sl) };
        MultichainAddress::new(self.db.allocate128(sl128).unwrap_or_else(|| CheckerDb::capacity_exceeded("WSR multichain", mchain.length())))
    }
    pub fn retrieve_clause(&self, addr: ClauseAddress) -> Clause<'_> {
        let sl32 = self.db.retrieve32(addr.addr);
        unsafe { Clause::from(mem::transmute::<&[u32], &[Literal]>(sl32)) }
    }
    pub fn retrieve_chain(&self, addr: ChainAddress) -> Chain<'_> {
        let sl64 = self.db.retrieve64(addr.addr);
        unsafe { Chain::from(mem::transmute::<&[u64], &[ClauseIndex]>(sl64)) }
    }
    pub fn retrieve_witness(&self, addr: WitnessAddress) -> Witness<'_> {
        let sl64 = self.db.retrieve64(addr.addr);
        unsafe { Witness::from(mem::transmute::<&[u64], &[(Variable, Literal)]>(sl64)) }
    }
    pub fn retrieve_multichain(&self, addr: MultichainAddress) -> Multichain<'_> {
        let sl128 = self.db.retrieve128(addr.addr);
        unsafe { Multichain::from(mem::transmute::<&[u128], &[(Option<ClauseIndex>, Option<ChainAddress>)]>(sl128)) }
    }
    pub fn deallocate_clause(&mut self, addr: ClauseAddress) {
        self.db.deallocate32(addr.addr)
    }
    pub fn deallocate_chain(&mut self, addr: ChainAddress) {
        self.db.deallocate64(addr.addr)
    }
    pub fn deallocate_witness(&mut self, addr: WitnessAddress) {
        self.db.deallocate64(addr.addr)
    }
    pub fn deallocate_multichain(&mut self, addr: MultichainAddress) {
        self.db.deallocate128(addr.addr)
    }
    pub fn capacity_exceeded(kind: &str, size: usize) -> ! {
        panick!("database record capacity exceeded", lock, {
            append!(lock, "Tried to allocate a record for a size {} {}, which exceeds record capacity in the database.", size, kind);
        })
    }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct ClauseAddress {
    addr: DbAddress
}
impl ClauseAddress {
    fn new(addr: DbAddress) -> ClauseAddress {
        ClauseAddress { addr: addr }
    }
    fn get(&self) -> &DbAddress {
        &self.addr
    }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct Clause<'a> {
    sl: &'a [Literal]
}
impl<'a> Clause<'a> {
    #[inline(always)]
    pub fn length(&self) -> usize {
        self.sl.len()
    }
}
impl<'a> IntoIterator for Clause<'a> {
    type Item = &'a Literal;
    type IntoIter = SliceIter<'a, Literal>;
    #[inline(always)]
    fn into_iter(self) -> SliceIter<'a, Literal> {
        self.sl.iter()
    }
}
impl<'a, 'b: 'a> IntoIterator for &'a Clause<'b> {
    type Item = &'a Literal;
    type IntoIter = SliceIter<'a, Literal>;
    #[inline(always)]
    fn into_iter(self) -> SliceIter<'a, Literal> {
        self.sl.iter()
    }
}
impl<'a> From<&'a [Literal]> for Clause<'a> {
    fn from(sl: &'a [Literal]) -> Clause<'a> {
        Clause::<'a> { sl: sl }
    }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct ChainAddress {
    addr: DbAddress
}
impl ChainAddress {
    fn new(addr: DbAddress) -> ChainAddress {
        ChainAddress { addr: addr }
    }
    fn get(&self) -> &DbAddress {
        &self.addr
    }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct Chain<'a> {
    sl: &'a[ClauseIndex]
}
impl<'a> Chain<'a> {
    #[inline(always)]
    pub fn length(&self) -> usize {
        self.sl.len()
    }
}
impl<'a> IntoIterator for Chain<'a> {
    type Item = &'a ClauseIndex;
    type IntoIter = SliceIter<'a, ClauseIndex>;
    #[inline(always)]
    fn into_iter(self) -> SliceIter<'a, ClauseIndex> {
        self.sl.iter()
    }
}
impl<'a, 'b: 'a> IntoIterator for &'a Chain<'b> {
    type Item = &'a ClauseIndex;
    type IntoIter = SliceIter<'a, ClauseIndex>;
    #[inline(always)]
    fn into_iter(self) -> SliceIter<'a, ClauseIndex> {
        self.sl.iter()
    }
}
impl<'a> From<&'a [ClauseIndex]> for Chain<'a> {
    fn from(sl: &'a [ClauseIndex]) -> Chain<'a> {
        Chain::<'a> { sl: sl }
    }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct WitnessAddress {
    addr: DbAddress
}
impl WitnessAddress {
    fn new(addr: DbAddress) -> WitnessAddress {
        WitnessAddress { addr: addr }
    }
    fn get(&self) -> &DbAddress {
        &self.addr
    }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct Witness<'a> {
    sl: &'a[(Variable, Literal)]
}
impl<'a> Witness<'a> {
    #[inline(always)]
    pub fn length(&self) -> usize {
        self.sl.len()
    }
}
impl<'a> IntoIterator for Witness<'a> {
    type Item = &'a (Variable, Literal);
    type IntoIter = SliceIter<'a, (Variable, Literal)>;
    #[inline(always)]
    fn into_iter(self) -> SliceIter<'a, (Variable, Literal)> {
        self.sl.iter()
    }
}
impl<'a, 'b: 'a> IntoIterator for &'a Witness<'b> {
    type Item = &'a (Variable, Literal);
    type IntoIter = SliceIter<'a, (Variable, Literal)>;
    #[inline(always)]
    fn into_iter(self) -> SliceIter<'a, (Variable, Literal)> {
        self.sl.iter()
    }
}
impl<'a> From<&'a [(Variable, Literal)]> for Witness<'a> {
    fn from(sl: &'a [(Variable, Literal)]) -> Witness<'a> {
        Witness::<'a> { sl: sl }
    }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct MultichainAddress {
    addr: DbAddress
}
impl MultichainAddress {
    fn new(addr: DbAddress) -> MultichainAddress {
        MultichainAddress { addr: addr }
    }
    fn get(&self) -> &DbAddress {
        &self.addr
    }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct Multichain<'a> {
    sl: &'a[(Option<ClauseIndex>, Option<ChainAddress>)]
}
impl<'a> Multichain<'a> {
    #[inline(always)]
    pub fn length(&self) -> usize {
        self.sl.len() - 1usize
    }
    pub fn witness<'b>(&'b self) -> WitnessAddress where 'a: 'b {
        let (_, addr) = unsafe { self.sl.get_unchecked(0usize) };
        unsafe { mem::transmute::<ChainAddress, WitnessAddress>(*addr.as_ref().unwrap()) }
    }
}
impl<'a> IntoIterator for Multichain<'a> {
    type Item = &'a (ClauseIndex, Option<ChainAddress>);
    type IntoIter = SliceIter<'a, (ClauseIndex, Option<ChainAddress>)>;
    #[inline(always)]
    fn into_iter(self) -> SliceIter<'a, (ClauseIndex, Option<ChainAddress>)> {
        unsafe {
            mem::transmute::<&'a[(Option<ClauseIndex>, Option<ChainAddress>)],
                &'a [(ClauseIndex, Option<ChainAddress>)]>(&self.sl[1 ..]).iter()
        }
    }
}
impl<'a, 'b: 'a> IntoIterator for &'a Multichain<'b> {
    type Item = &'a (ClauseIndex, Option<ChainAddress>);
    type IntoIter = SliceIter<'a, (ClauseIndex, Option<ChainAddress>)>;
    #[inline(always)]
    fn into_iter(self) -> SliceIter<'a, (ClauseIndex, Option<ChainAddress>)> {
        unsafe {
            mem::transmute::<&'a[(Option<ClauseIndex>, Option<ChainAddress>)],
                &'a [(ClauseIndex, Option<ChainAddress>)]>(&self.sl[1 ..]).iter()
        }
    }
}
impl<'a> From<&'a [(Option<ClauseIndex>, Option<ChainAddress>)]> for Multichain<'a> {
    fn from(sl: &'a [(Option<ClauseIndex>, Option<ChainAddress>)]) -> Multichain<'a> {
        Multichain::<'a> { sl: sl }
    }
}