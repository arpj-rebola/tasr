use std::{
    mem::{self},
    io::{Write, Result as IoResult},
};

use crate::{
    checkerdb::{CheckerDb, ClauseAddress, ChainAddress, WitnessAddress},
    buffer::{UncheckedChainBuffer, UncheckedClauseBuffer, UncheckedMultichainBuffer, UncheckedWitnessBuffer},
    idflags::{ClauseIndexFlags},
    basic::{ClauseIndex},
    proof::{ForwardsProof},
    split::{SplitterData, PreprocessingStats},
    io::{FilePosition},
    lexer::{AsrLexer},
    parser::{AsrParser, AsrClauseParser, AsrChainParser, AsrWitnessParser, ParsedInstructionKind, ParsedInstruction},
};

pub struct Trimmer {
    db: CheckerDb,
    clause: UncheckedClauseBuffer,
    chain: UncheckedChainBuffer,
    witness: UncheckedWitnessBuffer,
    mchain: UncheckedMultichainBuffer,
    idflags: ClauseIndexFlags<(FilePosition, ClauseAddress)>,
    dels: Vec<ClauseIndex>,
    proof: ForwardsProof,
    stats: PreprocessingStats,
}
impl Trimmer {
    pub fn new(data: SplitterData) -> Trimmer {
        let mut idflags = ClauseIndexFlags::new();
        for (id, addr, pos) in data.result {
            idflags.insert(id, (pos, addr)).unwrap();
        }
        idflags.flags_mut(data.qed).unwrap().set_check_schedule();
        Trimmer {
            db: data.db,
            clause: UncheckedClauseBuffer::new(),
            chain: UncheckedChainBuffer::new(),
            witness: UncheckedWitnessBuffer::new(),
            mchain: UncheckedMultichainBuffer::new(),
            idflags: idflags,
            dels: Vec::new(),
            proof: ForwardsProof::new(),
            stats: data.stats,
        }
    }
    pub fn process<L: AsrLexer>(&mut self, mut asr: AsrParser<L>) -> TrimmedFragment<'_> {
        while let Some(ins) = asr.parse_instruction(false, true) {
            match ins.kind() {
                ParsedInstructionKind::Rup => self.process_rup_instruction(&mut asr, &ins),
                ParsedInstructionKind::Del => self.process_del_instruction(&mut asr, &ins),
                ParsedInstructionKind::Wsr => self.process_wsr_instruction(&mut asr, &ins),
                _ => (),
            }
        }
        TrimmedFragment::<'_> {
            db: &mut self.db,
            proof: &mut self.proof,
        }
    }
    pub fn core<W: Write>(mut self, wt: &mut W, binary: bool) -> PreprocessingStats {
        if binary {
            self.process_cores_binary(wt)
        } else {
            self.process_cores_text(wt)
        }.unwrap_or_else(|e| panic!(format!("{}", e)));
        self.stats.record_trim_time();
        self.stats
    }
    fn process_rup_instruction<L: AsrLexer>(&mut self, asr: &mut AsrParser<L>, ins: &ParsedInstruction) {
        let id = *ins.index();
        let ((pos, clause_addr), flags) = self.idflags.take(id).unwrap();
        if flags.has_check_schedule() {
            let chain_addr = self.process_chain(asr.parse_chain());
            self.process_explicit_deletions(pos);
            self.proof.insert_rup(id, pos, clause_addr, chain_addr);
            self.stats.new_inference();
        } else {
            self.db.deallocate_clause(clause_addr);
            mem::drop(asr.parse_chain());
        }
    }
    fn process_wsr_instruction<L: AsrLexer>(&mut self, asr: &mut AsrParser<L>, ins: &ParsedInstruction) {
        let id = *ins.index();
        if self.idflags.flags(id).unwrap().has_check_schedule() {
            let witness_addr = self.process_witness(asr.parse_witness());
            self.mchain.set_witness(witness_addr);
            let mut mchain_ps = asr.parse_multichain(id);
            while let Some((lid, Some(chain_ps))) = mchain_ps.next() {
                if self.idflags.flags_mut(lid).unwrap().has_check_schedule() {
                    let chain_addr = self.process_chain(chain_ps);
                    self.mchain.push_chain(lid, chain_addr);
                }
            }
            self.process_implicit_deletions();
            let mchain_addr = self.db.allocate_multichain(self.mchain.get());
            self.mchain.clear();
            let ((pos, clause_addr), _) = self.idflags.take(id).unwrap();
            self.proof.insert_wsr(id, pos, clause_addr, mchain_addr);
            self.stats.new_inference();
        } else {
            let ((_, clause_addr), _) = self.idflags.take(id).unwrap();
            self.db.deallocate_clause(clause_addr);
            mem::drop(asr.parse_witness());
            mem::drop(asr.parse_multichain(id));
        }
    }
    fn process_del_instruction<L: AsrLexer>(&mut self, asr: &mut AsrParser<L>, ins: &ParsedInstruction) {
        let id = ins.index();
        let clause_addr = self.process_clause(asr.parse_clause());
        let pos = ins.default_position();
        self.idflags.insert(*id, (pos, clause_addr)).unwrap();
    }
    fn process_clause<L: AsrLexer>(&mut self, mut ps: AsrClauseParser<'_, L>) -> ClauseAddress {
        while let Some(lit) = ps.next() {
            self.clause.push(lit);
        }
        let addr = self.db.allocate_clause(self.clause.get());
        self.clause.clear();
        addr
    }
    fn process_chain<L: AsrLexer>(&mut self, mut ps: AsrChainParser<'_, L>) -> ChainAddress {
        while let Some(cid) = ps.next() {
            self.chain.push(cid);
            if !self.idflags.flags_mut(cid).unwrap().has_check_schedule() {
                self.dels.push(cid);
            }
        }
        let addr = self.db.allocate_chain(self.chain.get());
        self.chain.clear();
        addr
    }
    fn process_witness<L: AsrLexer>(&mut self, mut ps: AsrWitnessParser<'_, L>) -> WitnessAddress {
        while let Some((var, lit)) = ps.next() {
            self.witness.push(var, lit);
        }
        let addr = self.db.allocate_witness(self.witness.get());
        self.witness.clear();
        addr
    }
    fn process_explicit_deletions(&mut self, pos: FilePosition) {
        self.dels.sort();
        self.dels.dedup();
        for &did in &self.dels {
            self.idflags.flags_mut(did).unwrap().set_check_schedule();
            self.proof.insert_del(did, pos);
            self.stats.new_deletion_instruction();
        }
        self.dels.clear();
    }
    fn process_implicit_deletions(&mut self) {
        self.dels.sort();
        self.dels.dedup();
        for &did in &self.dels {
            self.idflags.flags_mut(did).unwrap().set_check_schedule();
            self.mchain.push_deletion(did);
            self.stats.new_implicit_deletion();
        }
        self.dels.clear();
    }
    fn process_cores_text<W: Write>(&mut self, wt: &mut W) -> IoResult<()> {
        for (id, ((pos, clause_addr), flags)) in self.idflags.clauses() {
            if flags.has_check_schedule() {
                write!(wt, "k {} l {} ", id.text(), pos.text())?;
                for &lit in self.db.retrieve_clause(*clause_addr) {
                    write!(wt, "{} ", lit.text())?;
                }
                write!(wt, "0\n")?;
                self.stats.new_core();
            }
        }
        Ok(())
    }
    fn process_cores_binary<W: Write>(&mut self, wt: &mut W) -> IoResult<()> {
        for (id, ((pos, clause_addr), flags)) in self.idflags.clauses() {
            if flags.has_check_schedule() {
                wt.write_all(&[0x01, b'k'])?;
                id.binary(wt)?;
                wt.write_all(&[0x01, b'l'])?;
                pos.as_binary(wt)?;
                for &lit in self.db.retrieve_clause(*clause_addr) {
                    lit.binary(wt)?;
                }
                wt.write_all(&[0x00])?;
                self.stats.new_core();
            }
        }
        Ok(())
    }
}

pub struct TrimmedFragment<'a> {
    db: &'a mut CheckerDb,
    proof: &'a mut ForwardsProof,
}
impl<'a> TrimmedFragment<'a> {
    #[inline]
    pub fn dump<W: Write>(mut self, wt: &mut W, binary: bool) {
        if binary {
            self.proof.write_binary(&mut self.db, wt)
        } else {
            self.proof.write_text(&mut self.db, wt)
        }.unwrap_or_else(|e| panic!(format!("{}", e)));
    }
}
impl<'a> Drop for TrimmedFragment<'a> {
    fn drop(&mut self) {
        self.proof.clean(&mut self.db);
    }
}