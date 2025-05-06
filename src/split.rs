use std::{
    io::{Write},
    time::{Instant, Duration},
};

use crate::{
    checkerdb::{CheckerDb, ClauseAddress, ChainAddress, WitnessAddress, MultichainAddress},
    io::{FilePosition, InputReader, FilePath},
    idmap::{IndexMapping},
    buffer::{UncheckedClauseBuffer, UncheckedChainBuffer, UncheckedWitnessBuffer, UncheckedMultichainBuffer},
    basic::{InstructionNumber, ClauseIndex},
    lexer::{AsrLexer, UnbufferedAsrBinaryLexer},
    parser::{AsrParser, AsrClauseParser, AsrChainParser, AsrMultichainParser, AsrWitnessParser, ParsedInstructionKind, ParsedInstruction},
    proof::{BackwardsProof, ProofBuffer},
};

pub struct SplitterConfig {
    pub chunk_size: u64,
    pub end: InstructionNumber,
}

pub struct PreprocessingStats {
    pub num_chunks: u64,
    pub num_cores: u64,
    pub num_instructions: u64,
    pub num_intros: u64,
    pub num_dels: u64,
    start_time: Instant,
    pub split_time: Option<Duration>,
    pub trim_time: Option<Duration>,
}
impl PreprocessingStats {
    fn new() -> PreprocessingStats {
        PreprocessingStats {
            num_chunks: 0u64,
            num_cores: 0u64,
            num_instructions: 0u64,
            num_intros: 0u64,
            num_dels: 0u64,
            start_time: Instant::now(),
            split_time: None,
            trim_time: None,
        }
    }
    pub fn record_split_time(&mut self) {
        self.split_time = Some(Instant::now().duration_since(self.start_time));
        self.start_time = Instant::now();
    }
    pub fn record_trim_time(&mut self) {
        self.trim_time = Some(Instant::now().duration_since(self.start_time));
        self.start_time = Instant::now();
    }
    pub fn new_core(&mut self) {
        self.num_cores += 1u64;
        self.num_intros += 1u64;
    }
    pub fn new_inference(&mut self) {
        self.num_instructions += 1u64;
        self.num_intros += 1u64;
    }
    pub fn new_implicit_deletion(&mut self) {
        self.num_dels += 1u64;
    }
    pub fn new_deletion_instruction(&mut self) {
        self.num_dels += 1u64;
        self.num_instructions += 1u64;
    }
    pub fn new_chunk(&mut self) {
        self.num_chunks += 1u64;
    }
    pub fn instruction_count(&self) -> u64 {
        self.num_instructions + self.num_cores
    }
}

pub struct SplitterData {
    pub db: CheckerDb,
    pub result: Vec<(ClauseIndex, ClauseAddress, FilePosition)>,
    pub qed: ClauseIndex,
    pub stats: PreprocessingStats,
}

struct SplitterBase {
    db: CheckerDb,
    idmap: IndexMapping,
    clause: UncheckedClauseBuffer,
    chain: UncheckedChainBuffer,
    witness: UncheckedWitnessBuffer,
    mchain: UncheckedMultichainBuffer,
    dels: Vec<ClauseIndex>,
    proof: BackwardsProof,
    current: InstructionNumber,
    counter: u64,
    qed: Option<ClauseIndex>,
    config: SplitterConfig,
    stats: PreprocessingStats,
}
impl SplitterBase {
    fn new(config: SplitterConfig) -> SplitterBase {
        SplitterBase {
            db: CheckerDb::new(),
            idmap: IndexMapping::new(),
            clause: UncheckedClauseBuffer::new(),
            chain: UncheckedChainBuffer::new(),
            witness: UncheckedWitnessBuffer::new(),
            mchain: UncheckedMultichainBuffer::new(),
            dels: Vec::new(),
            proof: BackwardsProof::new(),
            current: InstructionNumber::new_premise(),
            counter: config.chunk_size,
            qed: None,
            config: config,
            stats: PreprocessingStats::new(),
        }
    }
    fn process_core<L: AsrLexer>(&mut self, asr: &mut AsrParser<L>, buffer: &mut ProofBuffer) {
        asr.parse_core_header();
        self.current = InstructionNumber::new_premise();
        while let Some(ins) = asr.parse_instruction(true, true) {
            match ins.kind() {
                ParsedInstructionKind::Core => self.process_core_instruction(asr, &ins),
                _ => self.save_instruction(asr, &ins, buffer),
            }
            self.current.next();
            if self.finished() {
                self.qed = Some(self.idmap.id(*ins.index()).unwrap());
                break;
            }
        }
    }
    fn process_proof_chunk<L: AsrLexer>(&mut self, asr: &mut AsrParser<L>) -> Option<()> {
        while let Some(ins) = asr.parse_instruction(false, true) {
            match ins.kind() {
                ParsedInstructionKind::Rup => self.process_rup_instruction(asr, &ins),
                ParsedInstructionKind::Del => self.process_del_instruction(asr, &ins),
                ParsedInstructionKind::Wsr => self.process_wsr_instruction(asr, &ins),
                _ => (),
            }
            self.next_counter();
            if self.finished() {
                self.qed = Some(self.idmap.id(*ins.index()).unwrap());
                return Some(())
            }
            if self.chunk_done() {
                return Some(())
            }
        }
        None
    }
    fn process_core_instruction<L: AsrLexer>(&mut self, asr: &mut AsrParser<L>, ins: &ParsedInstruction) {
        let oid = *ins.index();
        let clause_addr = self.process_clause(asr.parse_clause());
        self.idmap.map(oid, clause_addr, ins.position().origin().clone()).unwrap();
    }
    fn process_rup_instruction<L: AsrLexer>(&mut self, asr: &mut AsrParser<L>, ins: &ParsedInstruction) {
        let oid = *ins.index();
        let clause_addr = self.process_clause(asr.parse_clause());
        let pos = ins.position().origin().clone();
        let nid = self.idmap.map(oid, clause_addr, pos.clone()).unwrap();
        let chain_addr = self.process_chain(asr.parse_chain());
        self.proof.insert_rup(nid, pos, chain_addr);
    }
    fn process_wsr_instruction<L: AsrLexer>(&mut self, asr: &mut AsrParser<L>, ins: &ParsedInstruction) {
        let oid = *ins.index();
        let clause_addr = self.process_clause(asr.parse_clause());
        let pos = ins.position().origin().clone();
        let nid = self.idmap.map(oid, clause_addr, pos.clone()).unwrap();
        let witness_addr = self.process_witness(asr.parse_witness());
        let mchain_addr = self.process_multichain(asr.parse_multichain(oid), witness_addr);
        self.proof.insert_wsr(nid, pos, mchain_addr);
        for &del_oid in &self.dels {
            let (del_nid, del_addr, del_pos) = self.idmap.unmap(del_oid).unwrap();
            self.proof.insert_del(del_nid, del_pos, del_addr);
        }
        self.dels.clear();
    }
    fn process_del_instruction<L: AsrLexer>(&mut self, _: &mut AsrParser<L>, ins: &ParsedInstruction) {
        let oid = *ins.index();
        let (nid, addr, pos) = self.idmap.unmap(oid).unwrap();
        self.proof.insert_del(nid, pos, addr);
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
        while let Some(oid) = ps.next() {
            let nid = self.idmap.id(oid).unwrap();
            self.chain.push(nid);
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
    fn process_multichain<L: AsrLexer>(&mut self, mut ps: AsrMultichainParser<'_, L>, witness: WitnessAddress) -> MultichainAddress {
        self.mchain.set_witness(witness);
        while let Some((olat, spec)) = ps.next() {
            if let Some(chain_ps) = spec {
                let nlat = self.idmap.id(olat).unwrap();
                let addr = self.process_chain(chain_ps);
                self.mchain.push_chain(nlat, addr);
            } else {
                self.dels.push(olat);
            }
        }
        let addr = self.db.allocate_multichain(self.mchain.get());
        self.mchain.clear();
        addr
    }
    fn save_instruction<L: AsrLexer>(&mut self, asr: &mut AsrParser<L>, ins: &ParsedInstruction, buffer: &mut ProofBuffer) {
        let res = match ins.kind() {
            ParsedInstructionKind::Core => buffer.core_instruction(*ins.index(), ins.position().origin().clone(), asr),
            ParsedInstructionKind::Rup => buffer.rup_instruction(*ins.index(), ins.position().origin().clone(), asr),
            ParsedInstructionKind::Del => buffer.del_instruction(*ins.index(), ins.position().origin().clone(), asr),
            ParsedInstructionKind::Wsr => buffer.wsr_instruction(*ins.index(), ins.position().origin().clone(), asr),
        };
        if let Err(e) = res {
            panic!("{}", e)
        }
    }
    fn init_buffer(&mut self) {
        self.current = InstructionNumber::new_buffer();
    }
    fn init_proof(&mut self) {
        self.current = InstructionNumber::new_proof();
    }
    fn next_counter(&mut self) {
        self.current.next();
        self.counter -= 1u64;
    }
    fn finished(&self) -> bool {
        self.current >= self.config.end
    }
    fn chunk_reset(&mut self) {
        self.counter = self.config.chunk_size;
    }
    fn chunk_done(&mut self) -> bool {
        self.counter == 0u64
    }
}

pub struct Splitter<'a, L: AsrLexer> {
    base: SplitterBase,
    asr: AsrParser<L>,
    buffer: Option<AsrParser<UnbufferedAsrBinaryLexer<&'a [u8]>>>,
}
impl<'a, L: AsrLexer> Splitter<'a, L> {
    pub fn new(config: SplitterConfig, mut asr: AsrParser<L>, buffer: &'a mut ProofBuffer) -> Splitter<'a, L> {
        let mut base = SplitterBase::new(config);
        base.process_core(&mut asr, buffer);
        base.init_buffer();
        let mut parser = AsrParser::new(UnbufferedAsrBinaryLexer::new(InputReader::new(buffer.reader(), FilePath::unknown(), false)));
        parser.parse_proof_header();
        Splitter {
            base: base,
            asr: asr,
            buffer: Some(parser),
        }
    }
    pub fn next<'c>(&'c mut self) -> Option<SplitFragment<'c>> {
        if self.base.finished() {
            None
        } else {
            self.base.chunk_reset();
            self.base.stats.new_chunk();
            let bf_drop = if let Some(bf) = &mut self.buffer {
                self.base.process_proof_chunk(bf).is_none()
            } else {
                false
            };
            if bf_drop {
                self.buffer = None;
                self.asr.parse_proof_header();
                self.base.init_proof();
            }
            if !self.base.finished() && !self.base.chunk_done() {
                self.base.process_proof_chunk(&mut self.asr);
            }
            Some(SplitFragment::<'c> {
                db: &mut self.base.db,
                proof: &mut self.base.proof,
            })
        }
    }
    pub fn extract(mut self) -> SplitterData {
        self.base.stats.record_split_time();
        SplitterData {
            db: self.base.db,
            result: self.base.idmap.extract(),
            qed: self.base.qed.unwrap(),
            stats: self.base.stats
        }
    }
}

pub struct SplitFragment<'a> {
    db: &'a mut CheckerDb,
    proof: &'a mut BackwardsProof,
}
impl<'a> SplitFragment<'a> {
    pub fn dump<W: Write>(mut self, wt: &mut W) {
        self.proof.write_text(&mut self.db, wt).unwrap_or_else(|e| panic!("{}", e));
    }
}
impl<'a> Drop for SplitFragment<'a> {
    fn drop(&mut self) {
        self.proof.clean(&mut self.db);
    }
}