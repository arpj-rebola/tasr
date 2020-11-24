use std::{
    num::{NonZeroU32},
    mem::{self, ManuallyDrop},
};

use crate::{
    basic::{ClauseIndex, Variable, Literal},
    clause::{ClauseAddress, ClauseDatabase, Clause},
    formula::{Formula},
    database::{DatabaseAddress, Database, DatabaseWriter, Record, RecordIterator, UnsafeRecordIterator},
    mapping::{IndexMapping},
};


#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct ChainAddress {
    addr: DatabaseAddress
}
impl ChainAddress {
    pub unsafe fn new(addr: DatabaseAddress) -> ChainAddress {
        ChainAddress { addr: addr }
    }
    pub unsafe fn get(&self) -> &DatabaseAddress {
        &self.addr
    }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct MultichainAddress {
    addr: DatabaseAddress
}

pub enum SubchainAddress {
    Chain(ClauseIndex, ChainAddress),
    Deletion(ClauseIndex, ClauseAddress),
}

pub struct MultichainBuffer {
    witness: Option<ChainAddress>,
    subchains: Vec<SubchainAddress>,
    deletions: Vec<ClauseIndex>,
}
impl MultichainBuffer {
    pub fn new() -> MultichainBuffer {
        MultichainBuffer {
            witness: None,
            subchains: Vec::new(),
            deletions: Vec::new(),
        }
    }
    fn set_witness(&mut self, addr: ChainAddress) {
        self.witness = Some(addr);
    }
    fn set_chain(&mut self, id: ClauseIndex, addr: ChainAddress) {
        self.subchains.push(SubchainAddress::Chain(id, addr));
    }
    fn set_deletion(&mut self, oid: ClauseIndex, id: ClauseIndex, addr: ClauseAddress) {
        self.deletions.push(oid);
        self.subchains.push(SubchainAddress::Deletion(id, addr));
    }
    fn clear_db(&mut self, db: &mut Database) {
        if let Some(addr) = &self.witness {
            unsafe { db.remove(*addr.get()); }
        }
        for subchain in &self.subchains {
            if let SubchainAddress::Chain(_, addr) = subchain {
                unsafe { db.remove(*addr.get()) }
            }
        }
        self.clear();
    }
    pub fn clear(&mut self) {
        self.witness = None;
        self.subchains.clear();
        self.deletions.clear();
    }
}

pub struct ChainDatabase;
impl ChainDatabase {
    const MultichainDeletionFlag: u32 = 1u32 << 16;
    #[inline(always)]
    pub fn open_chain<'a, 'b: 'a, 'c: 'a>(&self, db: &'b mut Database, map: Option<&'c IndexMapping>) -> ChainWriter<'a> {
        ChainWriter::<'a> {
            wt: db.open(),
            map: map,
            post: None,
        }
    }
    #[inline(always)]
    pub fn open_multichain<'a, 'b: 'a, 'c: 'a, 'd: 'a>(&self, db: &'b mut Database, map: Option<&'c mut IndexMapping>, buffer: &'d mut MultichainBuffer) -> WitnessWriter<'a> {
        let ptr: *mut Database = db;
        WitnessWriter::<'a> {
            wt: db.open(),
            db: ptr,
            map: map,
            buffer: buffer,
        }
    }
    #[inline(always)]
    pub unsafe fn retrieve_chain<'a, 'b: 'a>(&self, db: &'b Database, addr: ChainAddress) -> Chain<'a> {
        Chain::<'a> { rf: db.retrieve(addr.addr) }
    }
    #[inline(always)]
    pub unsafe fn retrieve_multichain<'a, 'b: 'a>(&self, db: &'b Database, addr: MultichainAddress) -> Multichain<'a> {
        Multichain::<'a> {
            rf: db.retrieve(addr.addr),
            db: db,
        }
    }
    pub unsafe fn iterate_multichain<'a, 'b: 'a>(&self, db: &'b mut Database, addr: MultichainAddress) -> UnsafeMultichainIterator {
        let mut it = db.iterator_unsafe(addr.addr);
        it.next();
        it.next();
        it.next();
        UnsafeMultichainIterator {
            db: db,
            it: it,
        }
    }
    #[inline(always)]
    pub unsafe fn remove_chain(&self, db: &mut Database, addr: ChainAddress) {
        db.remove(addr.addr)
    }
    #[inline(always)]
    pub unsafe fn remove_multichain(&self, db: &mut Database, addr: MultichainAddress) {
        let mut it = db.iterator_unsafe(addr.addr);
        it.next();
        let witness = {
            let main = it.next().unwrap() as u8;
            let sub = NonZeroU32::new_unchecked(it.next().unwrap());
            DatabaseAddress::new(main, sub)
        };
        db.remove(witness);
        let mut subit = UnsafeMultichainIterator {
            db: db,
            it: it,
        };
        while let Some(subc) = subit.next() {
            if let SubchainAddress::Chain(_, caddr) = subc {
                db.remove(caddr.addr)
            }
        }
        db.remove(addr.addr)
    }
}

pub struct ChainWriter<'a> {
    wt: DatabaseWriter<'a>,
    map: Option<&'a IndexMapping>,
    post: Option<(ClauseIndex, &'a mut MultichainBuffer)>,
}
impl<'a> ChainWriter<'a> {
    #[inline(always)]
    pub fn write(&mut self, oid: ClauseIndex) {
        unsafe { self.wt.write(mem::transmute::<ClauseIndex, u32>(self.map.map_or_else(|| oid, |m| m.map(oid).unwrap()))) }
    }
    #[inline]
    pub fn close(self) -> ChainAddress {
        let addr = self.wt.close();
        if let Some((id, buffer)) = self.post {
            buffer.set_chain(id, ChainAddress { addr: addr});
        }
        ChainAddress { addr: addr }
    }
}

pub struct WitnessWriter<'a> {
    wt: DatabaseWriter<'a>,
    db: *mut Database,
    map: Option<&'a mut IndexMapping>,
    buffer: &'a mut MultichainBuffer,
}
impl<'a> WitnessWriter<'a> {
    #[inline(always)]
    pub fn write(&mut self, var: Variable, lit: Literal) {
        unsafe { self.wt.write(mem::transmute::<Variable, u32>(var)) };
        unsafe { self.wt.write(mem::transmute::<Literal, u32>(lit)) };
    }
    #[inline]
    pub fn multichain(self) -> MultichainWriter<'a> {
        let addr = ChainAddress { addr: self.wt.close() };
        self.buffer.set_witness(addr);
        MultichainWriter::<'a> {
            db: unsafe { &mut *self.db },
            map: self.map,
            buffer: self.buffer,
        }
    }
}

pub struct MultichainWriter<'a> {
    db: &'a mut Database,
    map: Option<&'a mut IndexMapping>,
    buffer: &'a mut MultichainBuffer,
}
impl<'a> MultichainWriter<'a> {
    #[inline(always)]
    pub fn proper_chain<'c, 'b: 'c>(&'b mut self, oid: ClauseIndex) -> ChainWriter<'c> where 'a: 'b {
        let nid = self.map.as_ref().map_or_else(|| oid, |m| m.map(oid).unwrap());
        ChainWriter::<'c> {
            wt: self.db.open(),
            map: match &mut self.map {
                Some(rf) => Some(&mut *rf),
                None => None,
            },
            post: Some((nid, &mut *self.buffer)),
        }
    }
    pub fn deleted_chain(&mut self, oid: ClauseIndex, fm: &Formula) {
        let nid = self.map.as_ref().map_or_else(|| oid, |m| m.map(oid).unwrap());
        let (_, addr) = fm.get(nid).unwrap();
        self.buffer.set_deletion(oid, nid, *addr);
    }
    pub fn close(self) -> (MultichainAddress, MultichainBufferCleaner<'a>) {
        let mut wt = self.db.open();
        let substser = unsafe { self.buffer.witness.as_ref().unwrap().get().serialize() };
        wt.write(0u32);
        wt.write(substser.0);
        wt.write(substser.1);
        for subchain in &self.buffer.subchains {
            let (id, addr, del) = match subchain {
                SubchainAddress::Chain(id, addr) => unsafe { (id, addr.get(), false) },
                SubchainAddress::Deletion(id, addr) => unsafe { (id, addr.get(), true) },
            };
            unsafe { wt.write(mem::transmute::<ClauseIndex, u32>(*id)); }
            let mut addrser = addr.serialize();
            if del {
                addrser.0 |= ChainDatabase::MultichainDeletionFlag;
            }
            wt.write(addrser.0);
            wt.write(addrser.1);
        }
        let mcaddr = MultichainAddress { addr: wt.close() };
        let buffer = {
            let ptr: *mut MultichainBuffer = self.buffer;
            ManuallyDrop::new(self);
            ptr
        };
        (mcaddr, MultichainBufferCleaner::<'_> { buf: unsafe { &mut *buffer } })
    }
}
impl<'a> Drop for MultichainWriter<'a> {
    fn drop(&mut self) {
        self.buffer.clear_db(self.db);
    }
}

pub struct MultichainBufferCleaner<'a> {
    buf: &'a mut MultichainBuffer
}
impl<'a> MultichainBufferCleaner<'a> {
    pub fn deletions(&self) -> &[ClauseIndex] {
        &self.buf.deletions
    }
}
impl<'a> Drop for MultichainBufferCleaner<'a> {
    fn drop(&mut self) {
        self.buf.clear()
    }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct Chain<'a> {
    rf: Record<'a>
}
impl<'a> IntoIterator for Chain<'a> {
    type Item = &'a ClauseIndex;
    type IntoIter = ChainIterator<'a>;
    #[inline(always)]
    fn into_iter(self) -> ChainIterator<'a> {
        ChainIterator::<'a> { it: self.rf.into_iter() }
    }
}
impl<'a, 'b: 'a> IntoIterator for &'a Chain<'b> {
    type Item = &'a ClauseIndex;
    type IntoIter = ChainIterator<'a>;
    #[inline(always)]
    fn into_iter(self) -> ChainIterator<'a> {
        ChainIterator::<'a> { it: self.rf.into_iter() }
    }
}

pub enum Subchain<'a> {
    Chain(&'a ClauseIndex, Chain<'a>),
    Deletion(&'a ClauseIndex, Clause<'a>),
}

#[derive(Clone, Copy)]
pub struct Multichain<'a> {
    db: &'a Database,
    rf: Record<'a>,
}
impl<'a> Multichain<'a> {
    pub fn witness<'b>(&self) -> WitnessIterator<'b> where 'a: 'b {
        let addr = {
            let main = *self.rf.get(1usize).unwrap() as u8;
            let sub = unsafe { NonZeroU32::new_unchecked(*self.rf.get(2usize).unwrap()) };
            DatabaseAddress::new(main, sub)
        };
        WitnessIterator::<'b> { it: unsafe { self.db.retrieve(addr).into_iter() } }
    }
}
impl<'a> IntoIterator for Multichain<'a> {
    type Item = Subchain<'a>;
    type IntoIter = MultichainIterator<'a>;
    fn into_iter(self) -> MultichainIterator<'a> {
        let mut it = self.rf.into_iter();
        it.next();
        it.next();
        it.next();
        MultichainIterator::<'a> {
            db: self.db,
            it: it,
        }
    }
}
impl<'a, 'b: 'a> IntoIterator for &'a Multichain<'b> {
    type Item = Subchain<'a>;
    type IntoIter = MultichainIterator<'a>;
    fn into_iter(self) -> MultichainIterator<'a> {
        let mut it = self.rf.into_iter();
        it.next();
        it.next();
        it.next();
        MultichainIterator::<'a> {
            db: self.db,
            it: it,
        }
    }
}

#[repr(transparent)]
pub struct ChainIterator<'a> {
    it: RecordIterator<'a>,
}
impl<'a> Iterator for ChainIterator<'a> {
    type Item = &'a ClauseIndex;
    #[inline(always)]
    fn next(&mut self) -> Option<&'a ClauseIndex> {
        self.it.next().map(|x| unsafe { mem::transmute::<&u32, &ClauseIndex>(x) })
    }
}

pub struct WitnessIterator<'a> {
    it: RecordIterator<'a>,
}
impl<'a> Iterator for WitnessIterator<'a> {
    type Item = (&'a Variable, &'a Literal);
    fn next(&mut self) -> Option<(&'a Variable, &'a Literal)> {
        let var = unsafe { mem::transmute::<&u32, &Variable>(self.it.next()?) };
        let lit = unsafe { mem::transmute::<&u32, &Literal>(self.it.next().unwrap()) };
        Some((var, lit))
    }
}

pub struct MultichainIterator<'a> {
    db: &'a Database,
    it: RecordIterator<'a>,
}
impl<'a> Iterator for MultichainIterator<'a> {
    type Item = Subchain<'a>;
    fn next(&mut self) -> Option<Subchain<'a>> {
        let id = unsafe { mem::transmute::<&u32, &ClauseIndex>(self.it.next()?) };
        let (del, addr) = {
            let main = *self.it.next().unwrap();
            let del = main & ChainDatabase::MultichainDeletionFlag != 0u32;
            unsafe { (del, DatabaseAddress::new(main as u8, NonZeroU32::new_unchecked(*self.it.next().unwrap()))) }
        };
        Some(if del {
            unsafe { Subchain::Deletion(id, ClauseDatabase.retrieve(self.db, ClauseAddress::new(addr))) }
        } else {
            unsafe { Subchain::Chain(id, ChainDatabase.retrieve_chain(self.db, ChainAddress::new(addr))) }
        })
    }
}

pub struct UnsafeMultichainIterator {
    db: *mut Database,
    it: UnsafeRecordIterator,
}
impl UnsafeMultichainIterator {
    pub unsafe fn next(&mut self) -> Option<SubchainAddress> {
        let id = mem::transmute::<u32, ClauseIndex>(self.it.next()?);
        let (del, addr) = {
            let main = self.it.next().unwrap();
            let del = main & ChainDatabase::MultichainDeletionFlag != 0u32;
            (del, DatabaseAddress::new(main as u8, NonZeroU32::new_unchecked(self.it.next().unwrap())))
        };
        Some(if del {
            SubchainAddress::Deletion(id, ClauseAddress::new(addr))
        } else {
            SubchainAddress::Chain(id, ChainAddress::new(addr))
        })
    }
}

// #[cfg(test)]
// pub mod test {
// 	use rand::{self, Rng};
// 	use crate::{
//         database::{Database, DatabaseAddress},
//         basic::{ClauseIndex, Literal, Variable,
//             test::{generate_index}
//         },
//         mapping::test::{generate_clause},
//         chain::{MultichainBuffer, ChainDatabase, ChainAddress, MultichainAddress, SubchainAddress},
//         substitution::test::{generate_substitution},
//     };

//     #[derive(Debug)]
//     enum Subc {
//         Chain(Vec<ClauseIndex>),
//         Clause(ClauseAddress, Vec<Literal>),
//     }

//     fn generate_chain<R: Rng>(rng: &mut R, size: usize, limit: Option<u32>) -> Vec<ClauseIndex> {
//         let mut vec = Vec::new();
//         let length = rng.gen_range(0usize, size + 1usize);
//         for _ in 0 .. length {
//             vec.push(generate_index(rng, limit));
//         }
//         vec
//     }

//     fn generate_subc<R: Rng>(rng: &mut R, db: &mut Database, fm: &mut Formula, size: usize, limit: Option<u32>) -> Vec<(ClauseIndex, Subc)> {
//         let length = rng.gen_range(0usize, size);
//         let mut laterals = generate_chain(rng, length, None);
//         laterals.sort();
//         laterals.dedup();
//         let mut vec = Vec::new();
//         for lat in laterals {
//             let del: bool = rng.gen();
//             if del {
//                 let clause = generate_clause(rng, size, 6000u32);
//                 let wt = ClauseDatabase.open(db);
//                 for lit in &clause {
//                     wt.write(lit);
//                 }
//                 let addr = wt.close();
//                 fm.insert(lat, InstructionNumber::new(InstructionNumberKind::Proof), addr);
//                 vec.push((lat, Subc::Clause(addr, clause)));
//             } else {
//                 vec.push((lat, Subc::Chain(generate_chain(rng, size, None))));
//             }
//         }
//         vec
//     }

//     #[test]
//     fn test_chain_database() {
//         let mut rng = rand::thread_rng();
//         let mut db = Database::new();
//         let mut chaindb = ChainDatabase;
//         let mut formula = Formula::new();
//         let mut buf = MultichainBuffer::new();

//         let mut chains = Vec::<(ChainAddress, Vec<ClauseIndex>)>::new();
//         let mut mchains = Vec::<(MultichainAddress, Vec<(Variable, Literal)>, Vec<(ClauseIndex, Subc)>)>::new();
//         for _ in 0 .. 1000 {
//             let chain = generate_chain(&mut rng, 100usize, None);
//             let mut wt = chaindb.open_chain(&mut db, None);
//             for id in &chain {
//                 wt.write(*id);
//             }
//             let addr = wt.close();
//             chains.push((addr, chain));
//         }
//         for _ in 0 .. 1000 {
//         // for _ in 0 .. 1 {
//             assert!(buf.witness.is_none());
//             assert!(buf.subchains.is_empty());
//             assert!(buf.deletions.is_empty());
//             let witness = generate_substitution(&mut rng, 100usize);
//             // let witness = generate_substitution(&mut rng, 4usize);
//             let subc = generate_subc(&mut rng, 50usize, None);
//             // let subc = generate_subc(&mut rng, 4usize, None);
//             // println!("witness: {:?}", witness);
//             // println!("subchains: {:?}", subc);
//             let mut swt = chaindb.open_multichain(&mut db, None, &mut buf);
//             for (var, lit) in &witness {
//                 swt.write(*var, *lit);
//             }
//             let mut mcwt = swt.multichain();
//             for (id, sc) in &subc {
//                 match sc {
//                     Subc::Chain(chain) => {
//                         let mut cwt = mcwt.proper_chain(*id);
//                         for cid in chain {
//                             cwt.write(*cid);
//                         }
//                         cwt.close();
//                         {
//                             let addr = *unsafe { match mcwt.buffer.subchains.last().unwrap() {
//                                 SubchainAddress::Chain(_, a) => a.get(),
//                                 SubchainAddress::Deletion(_, a) => a.get(),
//                             } };
//                             let mut rec = unsafe { (&*mcwt.db).retrieve(addr).into_iter() };
//                             // print!("Written chain record @ {:?}: ", addr);
//                             // for num in rec {
//                             //     print!("{} ", num);
//                             // }
//                             // print!(";\n");
//                         }
//                     },
//                     Subc::Clause(addr, clause) => {
//                         mcwt.
//                     },
//                 }
//             }
//             let (addr, _) = mcwt.close();
//             // print!("Written multichain record @ {:?}: ", addr.addr);
//             // let mut rec = unsafe { db.retrieve(addr.addr) };
//             // for num in rec {
//             //     print!("{} ", num);
//             // }
//             // print!(";\n");
//             mchains.push((addr, witness, subc));
//         }
//         for (addr, chain) in &chains {
//             let mut ch1 = unsafe { chaindb.retrieve_chain(&db, *addr).into_iter() };
//             let mut ch2 = chain.iter();
//             loop { match (ch1.next(), ch2.next()) {
//                 (Some(x), Some(y)) => assert!(x == y),
//                 (None, None) => break,
//                 _ => assert!(false),
//             } }
//         }
//         for (addr, witness, mchain)  in &mchains {
//             let mut wt1 = unsafe { chaindb.retrieve_multichain(&db, *addr).witness().into_iter() };
//             let mut wt2 = witness.iter();
//             loop { match (wt1.next(), wt2.next()) {
//                 (Some((var1, lit1)), Some((var2, lit2))) => assert!(var1 == var2 && lit1 == lit2),
//                 (None, None) => break,
//                 _ => assert!(false),
//             } }
//             let mut mc1 = unsafe { chaindb.retrieve_multichain(&db, *addr).into_iter() };
//             let mut mc2 = mchain.iter();
//             loop { match (mc1.next(), mc2.next()) {
//                 (Some(s1), Some(s2)) => (),
//                 (None, None) => break,
//                 _ => assert!(false),
//             } }
//         }
//     }
// }