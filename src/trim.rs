use crate::{
    textparser::{TextAsrParser, AsrParsedInstructionKind},
    database::{Database},
    clause::{ClauseDatabase, ClauseAddress},
    chain::{ChainDatabase, MultichainBuffer},
    formula::{Formula},
    proof::{Proof, ProofIterator, CoreIterator},
    mapping::{IndexSet},
    basic::{InstructionNumber, ClauseIndex},
    split::{PreprocessingStats, SplittingData},
};

pub struct Trimmer {
    database: Database,
    clausedb: ClauseDatabase,
    chaindb: ChainDatabase,
    formula: Formula,
    marks: IndexSet,
    proof: Proof,
    buffer: MultichainBuffer,
    removals: Vec<ClauseAddress>,
    deletions: Vec<ClauseIndex>,
    stats: PreprocessingStats,
}
impl Trimmer {
    pub fn new(data: SplittingData) -> Option<Trimmer> {
        let mut marks = IndexSet::new();
        marks.set(data.stats.qed?);
        Some(Trimmer {
            database: data.database,
            clausedb: ClauseDatabase,
            chaindb: ChainDatabase,
            formula: data.formula,
            marks: marks,
            proof: Proof::new(),
            buffer: MultichainBuffer::new(),
            removals: Vec::new(),
            deletions: Vec::new(),
            stats: data.stats,
        })
    }
    pub fn trim_fragment(&mut self, mut ps: TextAsrParser<'_>) -> ProofIterator<'_> {
        while let Some((ins, _)) = ps.parse_instruction() {
            match ins.kind {
                AsrParsedInstructionKind::Rup => self.process_rup(&mut ps, ins.id, ins.num),
                AsrParsedInstructionKind::Wsr => self.process_wsr(&mut ps, ins.id, ins.num),
                AsrParsedInstructionKind::Del => self.process_del(&mut ps, ins.id, ins.num),
            }
        }
        self.proof.extract(&mut self.database, &mut self.removals)
    }
    pub fn core<'a, 'b: 'a>(&'b mut self) -> CoreIterator<'a> {
        let iter = self.formula.into_iter();
        CoreIterator::new(iter, &self.database, &self.marks, &mut self.stats.num_cores)
    }
    pub fn extract(mut self) -> PreprocessingStats {
        self.stats.record_trimming_time();
        self.stats
    }
    fn process_rup(&mut self, ps: &mut TextAsrParser<'_>, id: ClauseIndex, num: InstructionNumber) {
        let (_, clause) = self.formula.remove(id).unwrap();
        let mut chain_ps = ps.parse_chain();
        if self.marks.check_clear(id) {
            self.stats.num_intros += 1u64;
            let mut chain_wt = self.chaindb.open_chain(&mut self.database, None);
            while let Some(cid) = chain_ps.next() {
                chain_wt.write(cid);
                if !self.marks.check_set(cid) {
                    self.proof.insert_del(cid, num);
                    self.stats.num_dels += 1u64;
                }
            }
            let chain = chain_wt.close();
            self.proof.insert_rup(id, num, clause, chain);
        } else {
            self.removals.push(clause);
        }
    }
    fn process_wsr(&mut self, ps: &mut TextAsrParser<'_>, id: ClauseIndex, num: InstructionNumber) {
        let (_, clause) = self.formula.remove(id).unwrap();
        if self.marks.check_clear(id) {
            self.stats.num_intros += 1u64;
            let mut witness_wt = self.chaindb.open_multichain(&mut self.database, None, &mut self.buffer);
            {
                let mut witness_ps = ps.parse_witness();
                while let Some((var, lit)) = witness_ps.next() {
                    witness_wt.write(var, lit);
                }
            }
            let mut multichain_ps = ps.parse_multichain(id);
            {
                let mut multichain_wt = witness_wt.multichain();
                while let Some((lid, subchain_ps)) = multichain_ps.next() {
                    if self.marks.check(lid) || lid == id {
                        if let Some(mut chain_ps) = subchain_ps {
                            let mut chain_wt = multichain_wt.proper_chain(lid);
                            while let Some(cid) = chain_ps.next() {
                                chain_wt.write(cid);
                                if !self.marks.check_set(cid) {
                                    self.deletions.push(cid);
                                }
                            }
                            chain_wt.close();
                        }
                    }
                }
                self.deletions.sort();
                self.deletions.dedup();
                self.stats.num_dels += self.deletions.len() as u64;
                for &lid in &self.deletions {
                    multichain_wt.deleted_chain(lid, &self.formula);
                }
                self.deletions.clear();
                let (mchain, _) = multichain_wt.close();
                self.proof.insert_wsr(id, num, clause, mchain);
            }
        } else {
            ps.parse_witness();
            ps.parse_multichain(id);
            self.removals.push(clause);
        }
    }
    fn process_del(&mut self, ps: &mut TextAsrParser<'_>, id: ClauseIndex, num: InstructionNumber) {
        let mut clause_ps = ps.parse_clause();
        let mut clause_wt = self.clausedb.open(&mut self.database);
        while let Some(lit) = clause_ps.next() {
            clause_wt.write(lit);
        }
        let addr = clause_wt.close();
        self.formula.insert(id, num, addr);
    }
}