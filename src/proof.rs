use std::{
    fmt::{Display, Formatter, Result as FmtResult},
    io::{Write},
};

use crate::{
    basic::{ClauseIndex, InstructionNumber},
    clause::{ClauseAddress, Clause, ClauseDatabase},
    chain::{ChainAddress, MultichainAddress, Chain, Multichain, ChainDatabase, Subchain},
    database::{Database},
    io::{OutputWriter},
    formula::{FormulaIterator},
    mapping::{IndexSet},
};

pub struct CoreInstruction<'a>(ClauseIndex, InstructionNumber, Clause<'a>);
impl<'a> Display for CoreInstruction<'a> {
    fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "k {}", self.0)?;
        if let Some(n) = self.1.offset() {
            write!(f, "l {}", n.get())?;
        }
        for lit in self.2 {
            write!(f, "{} ", lit)?;
        }
        write!(f, "0\n")
    }
}

pub struct CoreIterator<'a> {
    iter: FormulaIterator<'a>,
    marks: &'a IndexSet,
    database: &'a Database,
    clausedb: ClauseDatabase,
    count: &'a mut u64,
}
impl<'a> CoreIterator<'a> {
    pub fn new<'b: 'a, 'c: 'a, 'd: 'a>(iter: FormulaIterator<'a>, database: &'b Database, marks: &'c IndexSet, count: &'d mut u64) -> CoreIterator<'a> {
        CoreIterator::<'a> {
            iter: iter,
            marks: marks,
            database: database,
            clausedb: ClauseDatabase,
            count: count,
        }
    }
    pub fn next(&mut self) -> Option<CoreInstruction<'_>> {
        loop {
            let (nid, num, addr) = self.iter.next()?;
            if self.marks.check(nid) {
                *self.count += 1u64;
                break Some(CoreInstruction(nid, num, unsafe { self.clausedb.retrieve(self.database, addr) }))
            }
        }
    }
    pub fn dump(mut self, wt: &mut OutputWriter<'_>) {
        while let Some(k) = self.next() {
            write!(wt, "{}", k).unwrap_or_else(|err| panic!(format!("{}", err)));
        }
    }
}

enum DirectInstruction {
    Rup(ClauseIndex, InstructionNumber, ClauseAddress, ChainAddress),
    Wsr(ClauseIndex, InstructionNumber, ClauseAddress, MultichainAddress),
    Del(ClauseIndex, InstructionNumber),
}

#[repr(transparent)]
pub struct Proof {
    vec: Vec<DirectInstruction>,
}
impl Proof {
    pub fn new() -> Proof {
        Proof { vec: Vec::new() }
    }
    pub fn insert_rup(&mut self, nid: ClauseIndex, num: InstructionNumber, clause: ClauseAddress, chain: ChainAddress) {
        self.vec.push(DirectInstruction::Rup(nid, num, clause, chain))
    }
    pub fn insert_wsr(&mut self, nid: ClauseIndex, num: InstructionNumber, clause: ClauseAddress, mchain: MultichainAddress) {
        self.vec.push(DirectInstruction::Wsr(nid, num, clause, mchain))
    }
    pub fn insert_del(&mut self, nid: ClauseIndex, num: InstructionNumber) {
        self.vec.push(DirectInstruction::Del(nid, num))
    }
    pub fn extract<'a, 'b: 'a, 'c: 'a, 'd: 'a>(&'b mut self, db: &'c mut Database, removals: &'d mut Vec<ClauseAddress>) -> ProofIterator<'a> {
        let len = self.vec.len();
        ProofIterator::<'a> {
            proof: self,
            removals: removals,
            database: db,
            clausedb: ClauseDatabase,
            chaindb: ChainDatabase,
            count: len,
        }
    }
}

pub enum DirectProofInstruction<'a> {
    Rup(ClauseIndex, InstructionNumber, Clause<'a>, Chain<'a>),
    Wsr(ClauseIndex, InstructionNumber, Clause<'a>, Multichain<'a>),
    Del(ClauseIndex, InstructionNumber),
}
impl<'a> Display for DirectProofInstruction<'a> {
    fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
        match self {
            DirectProofInstruction::Rup(id, num, clause, chain) => {
                write!(f, "r {} ", id)?;
                if let Some(n) = num.offset() {
                    write!(f, "l {}", n.get())?;
                }
                for lit in clause {
                    write!(f, "{} ", lit)?;
                }
                write!(f, "0 ")?;
                for cid in chain {
                    write!(f, "{} ", cid)?;
                }
                write!(f, "0\n")?;
            },
            DirectProofInstruction::Wsr(id, num, clause, mchain) => {
                write!(f, "w {} ", id)?;
                if let Some(n) = num.offset() {
                    write!(f, "l {}", n.get())?;
                }
                for lit in clause {
                    write!(f, "{} ", lit)?;
                }
                write!(f, "0 ")?;
                for (var, lit) in mchain.witness() {
                    write!(f, "{} {} ", var, lit)?;
                }
                write!(f, "0 ")?;
                for subchain in mchain {
                    match subchain {
                        Subchain::Chain(lid, chain) => {
                            if lid != id {
                                write!(f, "{} ", lid)?;
                            }
                            for cid in chain {
                                write!(f, "{} ", cid)?;
                            }
                            write!(f, "0 ")?;
                        },
                        Subchain::Deletion(did, _) => {
                            write!(f, "{} d", did)?;
                        },
                    }
                }
                write!(f, "0\n")?;
            },
            DirectProofInstruction::Del(id, num) => {
                write!(f, "d {} ", id)?;
                if let Some(n) = num.offset() {
                    write!(f, "l {}", n.get())?;
                }
            },
        }
        Ok(())
    }
}

pub struct ProofIterator<'a> {
    proof: &'a mut Proof,
    removals: &'a mut Vec<ClauseAddress>,
    database: &'a mut Database,
    clausedb: ClauseDatabase,
    chaindb: ChainDatabase,
    count: usize,
}
impl<'a> ProofIterator<'a> {
    fn next(&mut self) -> Option<DirectProofInstruction<'_>> {
        if self.count == 0usize {
            None
        } else {
            self.count -= 1usize;
            Some(match unsafe { self.proof.vec.get_unchecked(self.count) } {
                DirectInstruction::Rup(nid, num, clause, chain) => unsafe { DirectProofInstruction::Rup(*nid, *num, self.clausedb.retrieve(self.database, *clause), self.chaindb.retrieve_chain(self.database, *chain)) },
                DirectInstruction::Wsr(nid, num, clause, mchain) => unsafe { DirectProofInstruction::Wsr(*nid, *num, self.clausedb.retrieve(self.database, *clause), self.chaindb.retrieve_multichain(self.database, *mchain)) },
                DirectInstruction::Del(nid, num) => DirectProofInstruction::Del(*nid, *num),
            })
        }
    }
    pub fn dump(mut self, wt: &mut OutputWriter<'_>) {
        while let Some(ins) = self.next() {
            write!(wt, "{}", ins).unwrap_or_else(|err| panic!(format!("{}", err)));
        }
    }
}
impl<'a> Drop for ProofIterator<'a> {
    fn drop(&mut self) {
        for ins in &self.proof.vec {
            match ins {
                DirectInstruction::Rup(_, _, clause, chain) => {
                    unsafe { self.clausedb.remove(self.database, *clause); }
                    unsafe { self.chaindb.remove_chain(self.database, *chain); }
                },
                DirectInstruction::Wsr(_, _, clause, mchain) => {
                    unsafe { self.clausedb.remove(self.database, *clause); }
                    unsafe { self.chaindb.remove_multichain(self.database, *mchain); }
                },
                _ => (),
            }
        }
        self.proof.vec.clear();
        for clause in &*self.removals {
            unsafe { self.clausedb.remove(self.database, *clause); }
        }
        self.removals.clear();
    }
}

enum ReverseInstruction {
    Rup(ClauseIndex, InstructionNumber, ChainAddress),
    Wsr(ClauseIndex, InstructionNumber, MultichainAddress),
    Del(ClauseIndex, InstructionNumber, ClauseAddress),
}

#[repr(transparent)]
pub struct ProofReversion {
    vec: Vec<ReverseInstruction>,
}
impl ProofReversion {
    pub fn new() -> ProofReversion {
        ProofReversion { vec: Vec::new() }
    }
    pub fn insert_rup(&mut self, nid: ClauseIndex, num: InstructionNumber, chain: ChainAddress) {
        self.vec.push(ReverseInstruction::Rup(nid, num, chain))
    }
    pub fn insert_wsr(&mut self, nid: ClauseIndex, num: InstructionNumber, mchain: MultichainAddress) {
        self.vec.push(ReverseInstruction::Wsr(nid, num, mchain))
    }
    pub fn insert_del(&mut self, nid: ClauseIndex, num: InstructionNumber, clause: ClauseAddress) {
        self.vec.push(ReverseInstruction::Del(nid, num, clause))
    }
    pub fn extract<'a, 'b: 'a, 'c: 'a>(&'b mut self, db: &'c mut Database) -> ProofReversionIterator<'a> {
        let len = self.vec.len();
        ProofReversionIterator::<'a> {
            proof: self,
            database: db,
            clausedb: ClauseDatabase,
            chaindb: ChainDatabase,
            count: len,
        }
    }
}

pub enum ReverseProofInstruction<'a> {
    Rup(ClauseIndex, InstructionNumber, Chain<'a>),
    Wsr(ClauseIndex, InstructionNumber, Multichain<'a>),
    Del(ClauseIndex, InstructionNumber, Clause<'a>),
}
impl<'a> Display for ReverseProofInstruction<'a> {
    fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
        match self {
            ReverseProofInstruction::Rup(id, num, chain) => {
                write!(f, "r {} ", id)?;
                if let Some(n) = num.offset() {
                    write!(f, "l {}", n.get())?;
                }
                for cid in chain {
                    write!(f, "{} ", cid)?;
                }
            },
            ReverseProofInstruction::Wsr(id, num, mchain) => {
                for subchain in mchain {
                    if let Subchain::Deletion(did, clause) = subchain {
                        write!(f, "d {} ", did)?;
                        if let Some(n) = num.offset() {
                            write!(f, "l {} ", n.get())?;
                        }
                        for lit in clause {
                            write!(f, "{} ", lit)?;
                        }
                        write!(f, "0\n")?;
                    }
                }
                write!(f, "w {} ", id)?;
                if let Some(n) = num.offset() {
                    write!(f, "l {}", n.get())?;
                }
                for (var, lit) in mchain.witness() {
                    write!(f, "{} {} ", var, lit)?;
                }
                write!(f, "0 ")?;
                for subchain in mchain {
                    if let Subchain::Chain(lid, chain) = subchain {
                        if lid != id {
                            write!(f, "{} ", lid)?;
                        }
                        for cid in chain {
                            write!(f, "{} ", cid)?;
                        }
                        write!(f, "0 ")?;
                    }
                }
            },
            ReverseProofInstruction::Del(id, num, clause) => {
                write!(f, "d {} ", id)?;
                if let Some(n) = num.offset() {
                    write!(f, "l {}", n.get())?;
                }
                for lit in clause {
                    write!(f, "{} ", lit)?;
                }
            },
        }
        write!(f, "0\n")
    }
}


pub struct ProofReversionIterator<'a> {
    proof: &'a mut ProofReversion,
    database: &'a mut Database,
    clausedb: ClauseDatabase,
    chaindb: ChainDatabase,
    count: usize,
}
impl<'a> ProofReversionIterator<'a> {
    pub fn next(&mut self) -> Option<ReverseProofInstruction<'_>> {
        if self.count == 0usize {
            None
        } else {
            self.count -= 1usize;
            Some(match unsafe { self.proof.vec.get_unchecked(self.count) } {
                ReverseInstruction::Rup(nid, num, chain) => unsafe { ReverseProofInstruction::Rup(*nid, *num, self.chaindb.retrieve_chain(self.database, *chain)) },
                ReverseInstruction::Wsr(nid, num, mchain) => unsafe { ReverseProofInstruction::Wsr(*nid, *num, self.chaindb.retrieve_multichain(self.database, *mchain)) },
                ReverseInstruction::Del(nid, num, clause) => unsafe { ReverseProofInstruction::Del(*nid, *num, self.clausedb.retrieve(self.database, *clause)) },                
            })
        }
    }
    pub fn dump(mut self, wt: &mut OutputWriter<'_>) {
        while let Some(ins) = self.next() {
            write!(wt, "{}", ins).unwrap_or_else(|err| panic!(format!("{}", err)));
        }
    }
}
impl<'a> Drop for ProofReversionIterator<'a> {
    fn drop(&mut self) {
        for ins in &self.proof.vec {
            match ins {
                ReverseInstruction::Rup(_, _, chain) => unsafe { self.chaindb.remove_chain(self.database, *chain) },
                ReverseInstruction::Wsr(_, _, mchain) => unsafe { self.chaindb.remove_multichain_and_clauses(self.database, *mchain) },
                ReverseInstruction::Del(_, _, clause) => unsafe { self.clausedb.remove(self.database, *clause) },
            }
        }
        self.proof.vec.clear();
    }
}