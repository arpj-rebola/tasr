use std::{
    io::{Result as IoResult, Write},
};

use crate::{
    basic::{ClauseIndex},
    io::{FilePosition},
    checkerdb::{ChainAddress, ClauseAddress, MultichainAddress, CheckerDb, WitnessAddress},
};

pub enum BackwardsProofInstruction {
    Rup(ClauseIndex, FilePosition, ChainAddress),
    Wsr(ClauseIndex, FilePosition, MultichainAddress),
    Del(ClauseIndex, FilePosition, ClauseAddress),
}
impl BackwardsProofInstruction {
    fn text<W: Write>(&self, db: &CheckerDb, wt: &mut W) -> IoResult<()> {
        match self {
            BackwardsProofInstruction::Rup(id, pos, chain_addr) => {
                write!(wt, "r {} l {} ", id.text(), pos.text())?;
                for cid in db.retrieve_chain(*chain_addr) {
                    write!(wt, "{} ", cid.text())?;
                }
            },
            BackwardsProofInstruction::Wsr(id, pos, mchain_addr) => {
                write!(wt, "w {} l {} ", id.text(), pos.text())?;
                let mchain = db.retrieve_multichain(*mchain_addr);
                for (var, lit) in db.retrieve_witness(mchain.witness()) {
                    write!(wt, "{} {} ", var.text(), lit.text())?;
                }
                write!(wt, "0 ")?;
                for (lat, spec) in mchain {
                    if let Some(chain_addr) = spec {
                        if lat != id {
                            write!(wt, "{} ", lat.text())?;
                        }
                        for cid in db.retrieve_chain(*chain_addr) {
                            write!(wt, "{} ", cid.text())?;
                        }
                        write!(wt, "0 ")?;
                    }
                }
            },
            BackwardsProofInstruction::Del(id, pos, clause_addr) => {
                write!(wt, "d {} l {} ", id.text(), pos.text())?;
                for lit in db.retrieve_clause(*clause_addr) {
                    write!(wt, "{} ", lit.text())?;
                }
            },
        }
        write!(wt, "0\n")
    }
}

pub struct BackwardsProof {
    vec: Vec<BackwardsProofInstruction>,
    delete_chain: Vec<ChainAddress>,
    delete_witness: Vec<WitnessAddress>
}
impl BackwardsProof {
    pub fn new() -> BackwardsProof {
        BackwardsProof {
            vec: Vec::new(),
            delete_chain: Vec::new(),
            delete_witness: Vec::new(),
        }
    }
    pub fn insert_rup(&mut self, id: ClauseIndex, pos: FilePosition, addr: ChainAddress) {
        self.vec.push(BackwardsProofInstruction::Rup(id, pos, addr));
    }
    pub fn insert_wsr(&mut self, id: ClauseIndex, pos: FilePosition, addr: MultichainAddress) {
        self.vec.push(BackwardsProofInstruction::Wsr(id, pos, addr));
    }
    pub fn insert_del(&mut self, id: ClauseIndex, pos: FilePosition, addr: ClauseAddress) {
        self.vec.push(BackwardsProofInstruction::Del(id, pos, addr))
    }
    pub fn write<W: Write>(&mut self, db: &CheckerDb, wt: &mut W) -> IoResult<()> {
        for ins in self.vec.iter().rev() {
            ins.text(db, wt)?;
        }
        Ok(())
    }
    pub fn clean(&mut self, db: &mut CheckerDb) {
        for ins in self.vec.iter() {
            match ins {
                BackwardsProofInstruction::Rup(_, _, addr) => db.deallocate_chain(*addr),
                BackwardsProofInstruction::Wsr(_, _, addr) => {
                    let mchain = db.retrieve_multichain(*addr);
                    self.delete_witness.push(mchain.witness());
                    for (_, spec) in mchain {
                        if let Some(chain_addr) = spec {
                            self.delete_chain.push(*chain_addr);
                        }
                    }
                    db.deallocate_multichain(*addr);
                },
                BackwardsProofInstruction::Del(_, _, addr) => db.deallocate_clause(*addr),
            }
        }
        for &addr in &self.delete_chain {
            db.deallocate_chain(addr);
        }
        for &addr in &self.delete_witness {
            db.deallocate_witness(addr);
        }
        self.delete_chain.clear();
        self.delete_witness.clear();
        self.vec.clear();
    }
}

pub enum ForwardsProofInstruction {
    Rup(ClauseIndex, FilePosition, ClauseAddress, ChainAddress),
    Wsr(ClauseIndex, FilePosition, ClauseAddress, MultichainAddress),
    Del(ClauseIndex, FilePosition),
}
impl ForwardsProofInstruction {
    fn text<W: Write>(&self, db: &CheckerDb, wt: &mut W) -> IoResult<()> {
        match self {
            ForwardsProofInstruction::Rup(id, pos, clause_addr, chain_addr) => {
                write!(wt, "r {} l {} ", id.text(), pos.text())?;
                for lit in db.retrieve_clause(*clause_addr) {
                    write!(wt, "{} ", lit.text())?;
                }
                write!(wt, "0 ")?;
                for cid in db.retrieve_chain(*chain_addr) {
                    write!(wt, "{} ", cid.text())?;
                }
                write!(wt, "0\n")?;
            },
            ForwardsProofInstruction::Wsr(id, pos, clause_addr, mchain_addr) => {
                write!(wt, "w {} l {} ", id.text(), pos.text())?;
                for lit in db.retrieve_clause(*clause_addr) {
                    write!(wt, "{} ", lit.text())?;
                }
                write!(wt, "0 ")?;
                let mchain = db.retrieve_multichain(*mchain_addr);
                for (var, lit) in db.retrieve_witness(mchain.witness()) {
                    write!(wt, "{} {} ", var.text(), lit.text())?;
                }
                write!(wt, "0 ")?;
                for (lat, spec) in mchain {
                    if let Some(chain_addr) = spec {
                        if lat != id {
                            write!(wt, "{} ", lat.text())?;
                        }
                        for cid in db.retrieve_chain(*chain_addr) {
                            write!(wt, "{} ", cid.text())?;
                        }
                        write!(wt, "0 ")?;
                    } else {
                        write!(wt, "{} d ", lat.text())?;
                    }
                }
                write!(wt, "0\n")?;
            },
            ForwardsProofInstruction::Del(id, pos) => {
                write!(wt, "d {} l {} \n", id.text(), pos.text())?;
            },
        }
        Ok(())
    }
}
pub struct ForwardsProof {
    vec: Vec<ForwardsProofInstruction>,
    delete_chain: Vec<ChainAddress>,
    delete_witness: Vec<WitnessAddress>
}
impl ForwardsProof {
    pub fn new() -> ForwardsProof {
        ForwardsProof {
            vec: Vec::new(),
            delete_chain: Vec::new(),
            delete_witness: Vec::new(),
        }
    }
    pub fn insert_rup(&mut self, id: ClauseIndex, pos: FilePosition, clause_addr: ClauseAddress, chain_addr: ChainAddress) {
        self.vec.push(ForwardsProofInstruction::Rup(id, pos, clause_addr, chain_addr));
    }
    pub fn insert_wsr(&mut self, id: ClauseIndex, pos: FilePosition, clause_addr: ClauseAddress, mchain_addr: MultichainAddress) {
        self.vec.push(ForwardsProofInstruction::Wsr(id, pos, clause_addr, mchain_addr));
    }
    pub fn insert_del(&mut self, id: ClauseIndex, pos: FilePosition) {
        self.vec.push(ForwardsProofInstruction::Del(id, pos))
    }
    pub fn write<W: Write>(&mut self, db: &CheckerDb, wt: &mut W) -> IoResult<()> {
        for ins in self.vec.iter().rev() {
            ins.text(db, wt)?;
        }
        Ok(())
    }
    pub fn clean(&mut self, db: &mut CheckerDb) {
        for ins in self.vec.iter() {
            match ins {
                ForwardsProofInstruction::Rup(_, _, clause_addr, chain_addr) => {
                    db.deallocate_clause(*clause_addr);
                    db.deallocate_chain(*chain_addr);
                },
                ForwardsProofInstruction::Wsr(_, _, clause_addr, mchain_addr) => {
                    let mchain = db.retrieve_multichain(*mchain_addr);
                    self.delete_witness.push(mchain.witness());
                    for (_, spec) in mchain {
                        if let Some(chain_addr) = spec {
                            self.delete_chain.push(*chain_addr);
                        }
                    }
                    db.deallocate_clause(*clause_addr);
                    db.deallocate_multichain(*mchain_addr);
                },
                ForwardsProofInstruction::Del(_, _) => (),
            }
        }
        for &addr in &self.delete_chain {
            db.deallocate_chain(addr);
        }
        for &addr in &self.delete_witness {
            db.deallocate_witness(addr);
        }
        self.delete_chain.clear();
        self.delete_witness.clear();
        self.vec.clear();
    }
}