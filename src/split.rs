use std::{
    time::{Instant},
    num::{NonZeroU64},
};

use crate::{
    database::{Database},
    clause::{ClauseDatabase, ClauseAddress},
    chain::{ChainDatabase, MultichainBuffer},
    formula::{Formula},
    mapping::{IndexMapping},
    proof::{ProofReversion, ProofReversionIterator},
    textparser::{TextAsrParser, AsrParsedInstructionKind, TextClauseParser},
    basic::{ClauseIndex, InstructionNumber, InstructionNumberKind},
};

pub struct SplittingData {
    pub database: Database,
    pub formula: Formula,
    pub stats: PreprocessingStats,
    pub qed: ClauseIndex,
}

pub struct PreprocessingStats {
    pub num_chunks: u64,
    pub num_cores: u64,
    pub num_intros: u64,
    pub num_dels: u64,
    pub num_instructions: u64,
    pub chunk_size: Option<NonZeroU64>,
    pub start_time: Instant,
    pub splitting_time: Instant,
    pub trimming_time: Instant,
}
impl PreprocessingStats {
    pub fn new(chunk_size: Option<NonZeroU64>) -> PreprocessingStats {
        let now = Instant::now();
        PreprocessingStats {
            num_chunks: 0u64,
            num_cores: 0u64,
            num_intros: 0u64,
            num_dels: 0u64,
            num_instructions: 0u64,
            chunk_size: chunk_size,
            start_time: now,
            splitting_time: now,
            trimming_time: now,
        }
    }
    pub fn record_splitting_time(&mut self) {
        let now = Instant::now();
        self.splitting_time = now;
        self.trimming_time = now;
    }
    pub fn record_trimming_time(&mut self) {
        let now = Instant::now();
        self.trimming_time = now;
    }
}

pub struct Splitter {
    database: Database,
    clausedb: ClauseDatabase,
    chaindb: ChainDatabase,
    formula: Formula,
    mapping: IndexMapping,
    proof: ProofReversion,
    buffer: MultichainBuffer,
    stats: PreprocessingStats,
    limit: InstructionNumber,
    count: InstructionNumber,
    qedid: Option<ClauseIndex>,
}
impl Splitter {
    pub fn new(qed: InstructionNumber, chunksize: Option<NonZeroU64>, ps: &mut TextAsrParser<'_>) -> Option<Splitter> {
        let mut spl = Splitter {
            database: Database::new(),
            clausedb: ClauseDatabase,
            chaindb: ChainDatabase,
            formula: Formula::new(),
            mapping: IndexMapping::new(),
            proof: ProofReversion::new(),
            buffer: MultichainBuffer::new(),
            stats: PreprocessingStats::new(chunksize),
            limit: qed,
            count: InstructionNumber::new(InstructionNumberKind::Core),
            qedid: None,
        };
        spl.process_core(ps);
        Some(spl)
    }
    pub fn split<'a, 'b: 'a, 'c: 'a, 'p>(&'b mut self, ps: &'c mut TextAsrParser<'p>) -> SplitterIterator<'a, 'p> {
        SplitterIterator::<'a, 'p> {
            split: self,
            parser: ps,
        }
    }
    pub fn extract(mut self) -> Option<SplittingData> {
        self.stats.record_splitting_time();
        Some(SplittingData {
            database: self.database,
            formula: self.formula,
            stats: self.stats,
            qed: self.qedid?,
        })
    }
    fn process_core(&mut self, ps: &mut TextAsrParser<'_>) {
        ps.parse_core_header();
        while let Some((oid, _)) = ps.parse_core() {
            self.count = self.count.succ().unwrap();
            let num = ps.parse_instruction_number().unwrap_or_else(|| self.count);
            let nid = self.mapping.allocate(oid).unwrap();
            let addr = self.process_clause(ps.parse_clause());
            self.formula.insert(nid, num, addr);
            if self.count >= self.limit {
                self.qedid = Some(nid);
                break;
            }
        }
        ps.parse_proof_header();
        self.count = InstructionNumber::new(InstructionNumberKind::Proof)
    }
    fn process_proof_fragment(&mut self, ps: &mut TextAsrParser<'_>) -> Option<()> {
        if self.count >= self.limit {
            None
        } else {
            self.stats.num_chunks += 1u64;
            loop {
                self.count = self.count.succ().unwrap();
                let (ins, _) = ps.parse_instruction().unwrap();
                let nid = match ins.kind {
                    AsrParsedInstructionKind::Rup => self.process_rup(ps, ins.id),
                    AsrParsedInstructionKind::Wsr => self.process_wsr(ps, ins.id),
                    AsrParsedInstructionKind::Del => self.process_del(ins.id),
                };
                if self.count >= self.limit {
                    self.qedid = Some(nid);
                }
                if self.count >= self.limit || self.stats.chunk_size.map_or(true, |s| self.count.is_break(s)) {
                    break Some(())
                }
            }
        }
    }
    fn process_rup(&mut self, ps: &mut TextAsrParser<'_>, oid: ClauseIndex) -> ClauseIndex {
        let num = ps.parse_instruction_number().unwrap_or_else(|| self.count);
        let nid = self.mapping.allocate(oid).unwrap();
        let clause = self.process_clause(ps.parse_clause());
        self.formula.insert(nid, num, clause);
        let mut ps_chain = ps.parse_chain();
        let mut wt = self.chaindb.open_chain(&mut self.database, Some(&self.mapping));
        while let Some(id) = ps_chain.next() {
            wt.write(id);
        }
        let chain = wt.close();
        self.proof.insert_rup(nid, chain);
        nid
    }
    fn process_wsr(&mut self, ps: &mut TextAsrParser<'_>, oid: ClauseIndex) -> ClauseIndex {
        let num = ps.parse_instruction_number().unwrap_or_else(|| self.count);
        let nid = self.mapping.allocate(oid).unwrap();
        let clause = self.process_clause(ps.parse_clause());
        self.formula.insert(nid, num, clause);
        let ptr: *mut IndexMapping = &mut self.mapping;
        let mut witness_wt = self.chaindb.open_multichain(&mut self.database, Some(&mut self.mapping), &mut self.buffer);
        {
            let mut witness_ps = ps.parse_witness();
            while let Some((var, lit)) = witness_ps.next() {
                witness_wt.write(var, lit);
            }
        }
        let mut multichain_wt = witness_wt.multichain();
        {
            let mut multichain_ps = ps.parse_multichain(oid);
            while let Some((id, subchain_ps)) = multichain_ps.next() {
                if let Some(mut chain_ps) = subchain_ps {
                    let mut chain_wt = multichain_wt.proper_chain(id);
                    while let Some(cid) = chain_ps.next() {
                        chain_wt.write(cid);
                    }
                    chain_wt.close();
                } else {
                    multichain_wt.deleted_chain(id, &self.formula);
                }
            }
        }
        let (mchain, buf) = multichain_wt.close();
        self.proof.insert_wsr(nid, mchain);
        let map = unsafe { &mut *ptr };
        for &oid in buf.deletions() {
            let nid = map.take(oid).unwrap();
            let (dnum, daddr) = self.formula.remove(nid).unwrap();
            self.proof.insert_del(nid, dnum, daddr)
        }
        nid
    }
    fn process_del(&mut self, oid: ClauseIndex) -> ClauseIndex {
        let nid = self.mapping.take(oid).unwrap();
        let (num, clause) = self.formula.remove(nid).unwrap();
        self.proof.insert_del(nid, num, clause);
        nid
    }
    fn process_clause(&mut self, mut ps: TextClauseParser<'_, '_>) -> ClauseAddress {
        let mut wt = self.clausedb.open(&mut self.database);
        while let Some(lit) = ps.next() {
            wt.write(lit);
        }
        wt.close()
    }
}

pub struct SplitterIterator<'a, 'p> {
    split: &'a mut Splitter,
    parser: &'a mut TextAsrParser<'p>,
}
impl<'a, 'p> SplitterIterator<'a, 'p> {
    pub fn next(&mut self) -> Option<ProofReversionIterator<'_>> {
        self.split.process_proof_fragment(self.parser)?;
        Some(self.split.proof.extract(&mut self.split.database))
    }
}

#[cfg(test)]
pub mod test {
    use std::{
        fs::{File},
        path::{Path},
        convert::{TryFrom},
    };
    use crate::{
        textparser::{TextAsrParser},
        io::{InputReader, MainOutput, MutedOutput},
        integrity::{IntegrityVerifier, IntegrityError, EqFilePosition, IntegritySection},
        basic::{Variable, Literal, MaybeVariable, ClauseIndex},
    };
}