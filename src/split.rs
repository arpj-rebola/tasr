use std::{
    time::{Instant},
    num::{NonZeroU64},
};

use crate::{
    database::{Database},
    clause::{ClauseDatabase, ClauseAddress},
    chain::{ChainDatabase, ChainAddress, MultichainAddress, MultichainBuffer},
    formula::{Formula},
    mapping::{IndexMapping},
    proof::{ProofReversion, ProofReversionIterator},
    textparser::{TextAsrParser, AsrParsedInstructionKind, TextClauseParser, TextChainParser},
    basic::{ClauseIndex, InstructionNumber, InstructionNumberKind},
};

pub struct SplittingData {
    pub database: Database,
    pub formula: Formula,
    pub stats: PreprocessingStats,
}

pub struct PreprocessingStats {
    pub qed: Option<ClauseIndex>,
    pub num_chunks: u64,
    pub num_cores: u64,
    pub num_intros: u64,
    pub num_dels: u64,
    pub chunk_size: u64,
    pub start_time: Instant,
    pub splitting_time: Instant,
    pub trimming_time: Instant,
}
impl PreprocessingStats {
    pub fn new(size: NonZeroU64) -> PreprocessingStats {
        let now = Instant::now();
        PreprocessingStats {
            qed: None,
            num_chunks: 0u64,
            num_cores: 0u64,
            num_intros: 0u64,
            num_dels: 0u64,
            chunk_size: size.get(),
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
    limit: u64,
    count: u64,
}
impl Splitter {
    pub fn new(qed: InstructionNumber, chunksize: Option<NonZeroU64>, ps: &mut TextAsrParser<'_>) -> Option<Splitter> {
        let (core_limit, proof_limit) = match qed.kind()? {
            InstructionNumberKind::Core => (qed.offset(), 0u64),
            InstructionNumberKind::Proof => (None, qed.offset().unwrap().get())
        };
        let chunksz = unsafe { chunksize.unwrap_or(NonZeroU64::new_unchecked(u64::max_value())) };
        let mut spl = Splitter {
            database: Database::new(),
            clausedb: ClauseDatabase,
            chaindb: ChainDatabase,
            formula: Formula::new(),
            mapping: IndexMapping::new(),
            proof: ProofReversion::new(),
            buffer: MultichainBuffer::new(),
            stats: PreprocessingStats::new(chunksz),
            limit: proof_limit,
            count: 0u64,
        };
        spl.process_core(core_limit, ps);
        Some(spl)
    }
    pub fn split<'a, 'b: 'a, 'c: 'a, 'p>(&'b mut self, ps: &'c mut TextAsrParser<'p>) -> SplitterIterator<'a, 'p> {
        SplitterIterator::<'a, 'p> {
            split: self,
            parser: ps,
        }
    }
    pub fn extract(mut self) -> SplittingData {
        self.stats.record_splitting_time();
        SplittingData {
            database: self.database,
            formula: self.formula,
            stats: self.stats,
        }
    }
    fn process_core(&mut self, limit: Option<NonZeroU64>, ps: &mut TextAsrParser<'_>) {
        ps.parse_core_header();
        let mut count = 0u64;
        while let Some((oid, mut num, _)) = ps.parse_core() {
            count += 1u64;
            num.insert(count);
            let nid = self.mapping.allocate(oid).unwrap();
            let addr = self.process_clause(ps.parse_clause());
            self.formula.insert(nid, num, addr);
            if let Some(lim) = limit {
                if lim.get() == count {
                    self.stats.qed = Some(nid);
                }
            }
        }
        ps.parse_proof_header();
    }
    fn process_proof_fragment(&mut self, ps: &mut TextAsrParser<'_>) -> Option<()> {
        if self.count >= self.limit {
            None
        } else {
            self.stats.num_chunks += 1u64;
            loop {
                self.count += 1u64;
                let (mut ins, _) = ps.parse_instruction().unwrap();
                ins.num.insert(self.count);
                let nid = match ins.kind {
                    AsrParsedInstructionKind::Rup => self.process_rup(ps, ins.id, ins.num),
                    AsrParsedInstructionKind::Wsr => self.process_wsr(ps, ins.id, ins.num),
                    AsrParsedInstructionKind::Del => self.process_del(ins.id, ins.num),
                };
                if self.count == self.limit {
                    self.stats.qed = Some(nid);
                    break Some(())
                } else if self.count % self.stats.chunk_size == 0u64 {
                    break Some(())
                }
            }
        }
    }
    fn process_rup(&mut self, ps: &mut TextAsrParser<'_>, oid: ClauseIndex, num: InstructionNumber) -> ClauseIndex {
        let nid = self.mapping.allocate(oid).unwrap();
        let clause = self.process_clause(ps.parse_clause());
        self.formula.insert(nid, num, clause);
        let chain = self.process_chain(ps.parse_chain());
        self.proof.insert_rup(nid, num, chain);
        nid
    }
    fn process_wsr(&mut self, ps: &mut TextAsrParser<'_>, oid: ClauseIndex, num: InstructionNumber) -> ClauseIndex {
        let nid = self.mapping.allocate(oid).unwrap();
        let clause = self.process_clause(ps.parse_clause());
        self.formula.insert(nid, num, clause);
        let mchain = self.process_multichain(ps, oid);
        self.proof.insert_wsr(nid, num, mchain);
        nid
    }
    fn process_del(&mut self, oid: ClauseIndex, num: InstructionNumber) -> ClauseIndex {
        let nid = self.mapping.take(oid).unwrap();
        let (_, clause) = self.formula.remove(nid).unwrap();
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
    fn process_chain(&mut self, mut ps: TextChainParser<'_, '_>) -> ChainAddress {
        let mut wt = self.chaindb.open_chain(&mut self.database, Some(&self.mapping));
        while let Some(id) = ps.next() {
            wt.write(id);
        }
        wt.close()
    }
    fn process_multichain(&mut self, ps: &mut TextAsrParser<'_>, oid: ClauseIndex) -> MultichainAddress {
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
        let (addr, buf) = multichain_wt.close();
        let map = unsafe { &mut *ptr };
        for &oid in buf.deletions() {
            let nid = map.take(oid).unwrap();
            self.formula.remove(nid);
        }
        addr
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