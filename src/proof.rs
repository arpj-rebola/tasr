use std::{
    io::{Result as IoResult, Write},
};

use crate::{
    basic::{ClauseIndex},
    io::{FilePosition},
    checkerdb::{ChainAddress, ClauseAddress, MultichainAddress, CheckerDb, WitnessAddress},
    lexer::{AsrLexer},
    parser::{AsrParser},
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
    fn binary<W: Write>(&self, db: &CheckerDb, wt: &mut W) -> IoResult<()> {
        match self {
            BackwardsProofInstruction::Rup(id, pos, chain_addr) => {
                wt.write_all(&[0x01, b'r'])?;
                id.binary(wt)?;
                wt.write_all(&[0x01, b'l'])?;
                pos.as_binary(wt)?;
                for cid in db.retrieve_chain(*chain_addr) {
                    cid.binary(wt)?;
                }
            },
            BackwardsProofInstruction::Wsr(id, pos, mchain_addr) => {
                wt.write_all(&[0x01, b'w'])?;
                id.binary(wt)?;
                wt.write_all(&[0x01, b'l'])?;
                pos.as_binary(wt)?;
                let mchain = db.retrieve_multichain(*mchain_addr);
                for (var, lit) in db.retrieve_witness(mchain.witness()) {
                    var.binary(wt)?;
                    lit.binary(wt)?;
                }
                wt.write_all(&[0x00])?;
                for (lat, spec) in mchain {
                    if let Some(chain_addr) = spec {
                        if lat != id {
                            lat.binary(wt)?;
                        }
                        for cid in db.retrieve_chain(*chain_addr) {
                            cid.binary(wt)?;
                        }
                        wt.write_all(&[0x00])?;
                    }
                }
            },
            BackwardsProofInstruction::Del(id, pos, clause_addr) => {
                wt.write_all(&[0x01, b'd'])?;
                id.binary(wt)?;
                wt.write_all(&[0x01, b'l'])?;
                pos.as_binary(wt)?;
                for lit in db.retrieve_clause(*clause_addr) {
                    lit.binary(wt)?;
                }
            },
        }
        wt.write_all(&[0x00])
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
    pub fn write_text<W: Write>(&mut self, db: &CheckerDb, wt: &mut W) -> IoResult<()> {
        for ins in self.vec.iter().rev() {
            ins.text(db, wt)?;
        }
        Ok(())
    }
    pub fn write_binary<W: Write>(&mut self, db: &CheckerDb, wt: &mut W) -> IoResult<()> {
        wt.write_all(&[0x00])?;
        for ins in self.vec.iter().rev() {
            ins.binary(db, wt)?;
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
                write!(wt, "0\n")
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
                write!(wt, "0\n")
            },
            ForwardsProofInstruction::Del(id, pos) => {
                write!(wt, "d {} l {} \n", id.text(), pos.text())
            },
        }
    }
    fn binary<W: Write>(&self, db: &CheckerDb, wt: &mut W) -> IoResult<()> {
        match self {
            ForwardsProofInstruction::Rup(id, pos, clause_addr, chain_addr) => {
                wt.write_all(&[0x01, b'r'])?;
                id.binary(wt)?;
                wt.write_all(&[0x01, b'l'])?;
                pos.as_binary(wt)?;
                for lit in db.retrieve_clause(*clause_addr) {
                    lit.binary(wt)?;
                }
                wt.write_all(&[0x00])?;
                for cid in db.retrieve_chain(*chain_addr) {
                    cid.binary(wt)?;
                }
                wt.write_all(&[0x00])
            },
            ForwardsProofInstruction::Wsr(id, pos, clause_addr, mchain_addr) => {
                wt.write_all(&[0x01, b'w'])?;
                id.binary(wt)?;
                wt.write_all(&[0x01, b'l'])?;
                pos.as_binary(wt)?;
                for lit in db.retrieve_clause(*clause_addr) {
                    lit.binary(wt)?;
                }
                wt.write_all(&[0x00])?;
                let mchain = db.retrieve_multichain(*mchain_addr);
                for (var, lit) in db.retrieve_witness(mchain.witness()) {
                    var.binary(wt)?;
                    lit.binary(wt)?;
                }
                wt.write_all(&[0x00])?;
                for (lat, spec) in mchain {
                    if let Some(chain_addr) = spec {
                        if lat != id {
                            lat.binary(wt)?;
                        }
                        for cid in db.retrieve_chain(*chain_addr) {
                            cid.binary(wt)?;
                        }
                        wt.write_all(&[0x00])?;
                    } else {
                        lat.binary(wt)?;
                        wt.write_all(&[0x01, b'd'])?;
                    }
                }
                wt.write_all(&[0x00])
            },
            ForwardsProofInstruction::Del(id, pos) => {
                wt.write_all(&[0x01, b'd'])?;
                id.binary(wt)?;
                wt.write_all(&[0x01, b'l'])?;
                pos.as_binary(wt)
            },
        }
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
    pub fn write_text<W: Write>(&mut self, db: &CheckerDb, wt: &mut W) -> IoResult<()> {
        for ins in self.vec.iter().rev() {
            ins.text(db, wt)?;
        }
        Ok(())
    }
    pub fn write_binary<W: Write>(&mut self, db: &CheckerDb, wt: &mut W) -> IoResult<()> {
        for ins in self.vec.iter().rev() {
            ins.binary(db, wt)?;
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

pub struct ProofBuffer {
    pub vec: Vec<u8>,
}
impl ProofBuffer {
    pub fn new() -> ProofBuffer {
        let mut vec = Vec::with_capacity(1usize << 18);
        vec.extend_from_slice(&[0x00u8, 0x01u8, b'p', b'p', b'r', b'o', b'o', b'f', 0x00u8]);
        ProofBuffer { vec: vec }
    }
    pub fn core_instruction<L: AsrLexer>(&mut self, id: ClauseIndex, fp: FilePosition, asr: &mut AsrParser<L>) -> IoResult<()> {
        let output = &mut self.vec;
        output.write_all(&[0x01, b'k'])?;
        id.binary(output)?;
        output.write_all(&[0x01, b'l'])?;
        fp.as_binary(output)?;
        let mut clause_ps = asr.parse_clause();
        while let Some(lit) = clause_ps.next() {
            lit.binary(output)?;
        }
        output.write_all(&[0x00])
    }
    pub fn rup_instruction<L: AsrLexer>(&mut self, id: ClauseIndex, fp: FilePosition, asr: &mut AsrParser<L>) -> IoResult<()> {
        let output = &mut self.vec;
        output.write_all(&[0x01, b'r'])?;
        id.binary(output)?;
        output.write_all(&[0x01, b'l'])?;
        fp.as_binary(output)?;
        {
            let mut clause_ps = asr.parse_clause();
            while let Some(lit) = clause_ps.next() {
                lit.binary(output)?;
            }
        }
        output.write_all(&[0x00])?;
        {
            let mut chain_ps = asr.parse_chain();
            while let Some(cid) = chain_ps.next() {
                cid.binary(output)?;
            }
        }
        output.write_all(&[0x00])
    }
    pub fn wsr_instruction<L: AsrLexer>(&mut self, id: ClauseIndex, fp: FilePosition, asr: &mut AsrParser<L>) -> IoResult<()> {
        let output = &mut self.vec;
        output.write_all(&[0x01, b'w'])?;
        id.binary(output)?;
        output.write_all(&[0x01, b'l'])?;
        fp.as_binary(output)?;
        {
            let mut clause_ps = asr.parse_clause();
            while let Some(lit) = clause_ps.next() {
                lit.binary(output)?;
            }
        }
        output.write_all(&[0x00])?;
        {
            let mut witness_ps = asr.parse_witness();
            while let Some((var, lit)) = witness_ps.next() {
                var.binary(output)?;
                lit.binary(output)?;
            }
        }
        output.write_all(&[0x00])?;
        {
            let mut mchain_ps = asr.parse_multichain(id);
            while let Some((lid, pchain)) = mchain_ps.next() {
                if lid != id {
                    lid.binary(output)?;
                }
                match pchain {
                    Some(mut chain_ps) => {
                        while let Some(cid) = chain_ps.next() {
                            cid.binary(output)?;
                        }
                        output.write_all(&[0x00])?;
                    },
                    None => output.write_all(&[0x01, b'd'])?,
                }
            }
        }
        output.write_all(&[0x00])
    }
    pub fn del_instruction<L: AsrLexer>(&mut self, id: ClauseIndex, fp: FilePosition, _: &mut AsrParser<L>) -> IoResult<()> {
        let output = &mut self.vec;
        output.write_all(&[0x01, b'd'])?;
        id.binary(output)?;
        output.write_all(&[0x01, b'l'])?;
        fp.as_binary(output)
    }
    pub fn reader(&self) -> &[u8] {
        &self.vec[..]
    }
}