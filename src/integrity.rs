use std::{
    time::{Instant},
};

use crate::{
    basic::{MaybeClauseIndex, MaybeVariable, Literal, ClauseIndex, Variable, InstructionNumber, InstructionNumberKind},
    mapping::{LiteralSet, IndexSet},
    substitution::{SubstitutionInsertion, Substitution},
    textparser::{TextAsrParser, TextClauseParser, TextWitnessParser, TextChainParser, TextMultichainParser, AsrParsedInstructionKind},
    io::{FilePositionRef},
    display::{CsvDisplay, ClauseDisplay, SubstitutionDisplay, MappingDisplay, ChainDisplay, CsvDisplayMap},
};

pub struct IntegrityStats {
    pub max_variable: MaybeVariable,
    pub max_clauseid: MaybeClauseIndex,
    pub num_premises: u64,
    pub num_cores: u64,
    pub num_instructions: u64,
    pub num_rup: u64,
    pub num_wsr: u64,
    pub num_del: u64,
    pub first_qed: InstructionNumber,
    pub errors: u64,
    pub start_time: Instant,
    pub cnf_time: Instant,
    pub asr_time: Instant,
}
impl IntegrityStats {
    fn new() -> IntegrityStats {
        let now = Instant::now();
        IntegrityStats {
            max_variable: MaybeVariable::new(None),
            max_clauseid: MaybeClauseIndex::new(None),
            num_premises: 0u64,
            num_cores: 0u64,
            num_instructions: 0u64,
            num_rup: 0u64,
            num_wsr: 0u64,
            num_del: 0u64,
            first_qed: InstructionNumber::new(InstructionNumberKind::Core),
            errors: 0u64,
            start_time: now,
            cnf_time: now,
            asr_time: now,
        }
    }
    fn count(&self) -> u64 {
        if self.num_instructions != 0u64 {
            self.num_instructions
        } else if self.num_cores != 0u64 {
            self.num_cores
        } else {
            self.num_premises
        }
    }
    fn kind(&self) -> &'static str {
        if self.num_instructions != 0u64 {
            "premise clause"
        } else if self.num_cores != 0u64 {
            "core instruction"
        } else {
            "proof instruction"
        }
    }
    fn record_cnf_time(&mut self) {
        let now = Instant::now();
        self.cnf_time = now;
        self.asr_time = now;
    }
    fn record_asr_time(&mut self) {
        let now = Instant::now();
        self.asr_time = now;
    }
    fn record_qed(&mut self) {
        if self.first_qed.offset().is_none() {
            if self.num_instructions == 0u64 {
                self.first_qed = InstructionNumber::new(InstructionNumberKind::Core);
                self.first_qed.insert(self.num_cores as u64);
            } else {
                self.first_qed = InstructionNumber::new(InstructionNumberKind::Proof);
                self.first_qed.insert(self.num_instructions as u64);
            }
        }
    }
}

pub struct IntegrityVerifier {
    clause: ClauseIssues,
    chain: ChainIssues,
    witness: WitnessIssues,
    lits: LiteralSet,
    slots: IndexSet,
    marks: IndexSet,
    subchains: Vec<(ClauseIndex, bool)>,
    subst: Substitution,
    stats: IntegrityStats,
}
impl IntegrityVerifier {
    pub fn new() -> IntegrityVerifier {
        IntegrityVerifier {
            clause: ClauseIssues::new(),
            chain: ChainIssues::new(),
            witness: WitnessIssues::new(),
            lits: LiteralSet::new(),
            slots: IndexSet::new(),
            marks: IndexSet::new(),
            subchains: Vec::new(),
            subst: Substitution::new(),
            stats: IntegrityStats::new(),
        }
    }
    pub fn check(mut self, mut cnf: TextAsrParser<'_>, mut asr: TextAsrParser<'_>) -> IntegrityStats {
        self.check_cnf(&mut cnf);
        self.check_asr(&mut asr);
        if self.stats.first_qed.offset().is_none() {
            self.missing_qed(&asr.position());
        }
        self.stats
    }
    fn check_cnf(&mut self, cnf: &mut TextAsrParser<'_>) {
        let (cnfpos, cnfvar, cnfcls) = cnf.parse_cnf_header();
        while let Some(pos) = cnf.parse_premise() {
            self.stats.num_premises += 1u64;
            self.check_clause(cnf.parse_clause(), &pos, None);
        }
        if cnfvar < self.stats.max_variable {
            self.invalid_num_variables(&cnfpos, cnfvar);
        }
        if cnfcls as u64 != self.stats.num_premises {
            self.invalid_num_clauses(&cnfpos, cnfcls);
        }
        self.stats.record_cnf_time();
    }
    fn check_asr(&mut self, asr: &mut TextAsrParser<'_>) {
        asr.parse_core_header();
        while let Some((id, _, pos)) = asr.parse_core() {
            self.stats.num_cores += 1u64;
            if self.slots.check_set(id) {
                self.conflict_id(&pos, id);
            }
            self.check_clause(asr.parse_clause(), &pos, Some(id));
        }
        asr.parse_proof_header();
        while let Some((ins, pos)) = asr.parse_instruction() {
            match ins.kind {
                AsrParsedInstructionKind::Rup => {
                    self.stats.num_instructions += 1u64;
                    self.stats.num_rup += 1u64;
                    self.check_clause(asr.parse_clause(), &pos, Some(ins.id));
                    self.check_chain(asr.parse_chain(), &pos, ins.id, None);
                    if self.slots.check_set(ins.id) {
                        self.conflict_id(&pos, ins.id);
                    }
                },
                AsrParsedInstructionKind::Wsr => {
                    self.stats.num_instructions += 1u64;
                    self.stats.num_wsr += 1u64;
                    self.check_clause(asr.parse_clause(), &pos, Some(ins.id));
                    self.check_substitution(asr.parse_witness(), &pos, ins.id);
                    self.check_multichain(asr.parse_multichain(ins.id), &pos, ins.id);
                    if self.slots.check_set(ins.id) {
                        self.conflict_id(&pos, ins.id);
                    }
                },
                AsrParsedInstructionKind::Del => {
                    self.stats.num_instructions += 1u64;
                    self.stats.num_del += 1u64;
                    self.stats.max_clauseid = self.stats.max_clauseid | ins.id;
                    if self.slots.check_clear(ins.id) {
                        self.missing_deletion_id(&pos, ins.id);
                    }
                },
            }
        }
        self.stats.record_asr_time();
    }
    fn check_clause(&mut self, mut ps: TextClauseParser<'_, '_>, pos: &FilePositionRef<'_>, id: Option<ClauseIndex>) {
        while let Some(lit) = ps.next() {
            self.stats.max_variable = self.stats.max_variable | lit;
            let repeated = self.lits.check_set(lit);
            self.clause.add(lit, repeated);
        }
        if !self.clause.is_ok() {
            self.clause.invalid_clause(pos, &mut self.stats, id);
        }
        if self.clause.is_qed() && id.is_some() {
            self.stats.record_qed();
        }
        self.clause.clear(&mut self.lits)
    }
    fn check_substitution(&mut self, mut ps: TextWitnessParser<'_, '_>, pos: &FilePositionRef<'_>, id: ClauseIndex) {
        while let Some((var, lit)) = ps.next() {
            self.stats.max_variable = (self.stats.max_variable | var) | lit;
            let ins = self.subst.insert(var, lit);
            self.witness.add(var, lit, ins);
        }
        if !self.witness.is_ok() {
            self.witness.invalid_witness(pos, &mut self.stats, id);
        }
        self.witness.clear(&mut self.subst);
    }
    fn check_chain(&mut self, mut ps: TextChainParser<'_, '_>, pos: &FilePositionRef<'_>, central: ClauseIndex, lateral: Option<ClauseIndex>) {
        while let Some(cid) = ps.next() {
            self.stats.max_clauseid = self.stats.max_clauseid | cid;
            let missing = !self.slots.check(cid) || cid == central;
            self.chain.add(cid, missing);
        }
        if !self.chain.is_ok() {
            self.chain.missing_id(pos, &mut self.stats, central, lateral)
        }
        self.chain.clear();
    }
    fn check_multichain(&mut self, mut ps: TextMultichainParser<'_, '_>, pos: &FilePositionRef<'_>, central: ClauseIndex) {
        while let Some((lat, subps)) = ps.next() {
            self.stats.max_clauseid = self.stats.max_clauseid | lat;
            if !self.slots.check(lat) && lat != central {
                self.missing_id_subchain(pos, central, lat, subps.is_some());
            } else if self.marks.check_set(lat) {
                self.repeated_id_subchain(pos, central, lat, subps.is_some());
            } else {
                self.subchains.push((lat, true));
            }
            if let Some(chps) = subps {
                self.check_chain(chps, pos, central, Some(lat));
            } else {
                self.stats.num_del += 1u64;
                self.subchains.push((lat, false));
            }
        }
        for &(lat, flag) in &self.subchains {
            self.marks.clear(lat);
            if !flag {
                self.slots.clear(lat);
            }
        }
        self.subchains.clear();
    }
    fn invalid_num_variables(&mut self, pos: &FilePositionRef<'_>, var: MaybeVariable) {
        self.stats.errors += 1u64;
        warning!("incorrect number of variables" @ pos, lock, {
            append!(lock, "The DIMACS header declares {} variables, ", var);
            append!(lock, "but the maximum variable throughout the premises is {}.", self.stats.max_variable);
        })
    }
    fn invalid_num_clauses(&mut self, pos: &FilePositionRef<'_>, cls: usize) {
        self.stats.errors += 1u64;
        warning!("incorrect number of clauses" @ pos, lock, {
            append!(lock, "The DIMACS header declares {} clauses, ", cls);
            append!(lock, "but the premises contain {} clauses.", self.stats.num_premises);
        })
    }
    fn conflict_id(&mut self, pos: &FilePositionRef<'_>, id: ClauseIndex) {
        self.stats.errors += 1u64;
        warning!("conflicting clause identifier" @ pos, lock, {
            append!(lock, "A clause is introduced with identifier {} in {} #{}, ", id, self.stats.kind(), self.stats.count());
            append!(lock, "but this identifier is already in use for another clause.");
        })
    }
    fn missing_id_subchain(&mut self, pos: &FilePositionRef<'_>, central: ClauseIndex, lateral: ClauseIndex, subchain: bool) {
        self.stats.errors += 1u64;
        warning!("missing subchain identifier" @ pos, lock, {
            append!(lock, "A clause is introduced as a WSR inference  ");
            append!(lock, "with identifier {} in {} #{}; ", central, self.stats.kind(), self.stats.count());
            append!(lock, "the WSR multichain specifies ");
            if subchain {
                append!(lock, "a RUP chain ");
            } else {
                append!(lock, "an implicit deletion ");
            }
            append!(lock, "for the lateral clause with identifier {}, but this clause is missing.", lateral);
        })
    }
    fn repeated_id_subchain(&mut self, pos: &FilePositionRef<'_>, central: ClauseIndex, lateral: ClauseIndex, subchain: bool) {
        self.stats.errors += 1u64;
        warning!("missing subchain identifier" @ pos, lock, {
            append!(lock, "A clause is introduced as a WSR inference  ");
            append!(lock, "with identifier {} in {} #{}; ", central, self.stats.kind(), self.stats.count());
            append!(lock, "the WSR multichain specifies ");
            if subchain {
                append!(lock, "a RUP chain ");
            } else {
                append!(lock, "an implicit deletion ");
            }
            append!(lock, "for the lateral clause with identifier {}, ", lateral);
            append!(lock, "but another RUP chain or implicit deletion is specified for this lateral clause in the same multichain.");
        })
    }
    fn missing_deletion_id(&mut self, pos: &FilePositionRef<'_>, id: ClauseIndex) {
        self.stats.errors += 1u64;
        warning!("missing clause deletion identifier" @ pos, lock, {
            append!(lock, "A clause with identifier {} is deleted in {} #{}, ", id, self.stats.kind(), self.stats.count());
            append!(lock, "but this clause is missing.");
        })
    }
    fn missing_qed(&mut self, pos: &FilePositionRef<'_>) {
        self.stats.errors += 1u64;
        warning!("missing contradiction" @ pos, lock, {
            append!(lock, "Neither the core nor the proof contain a contradiction clause.");
        })
    }
}


pub struct ClauseIssues {
    clause: Vec<Literal>,
    repetitions: Vec<Literal>,
    count: usize,
}
impl ClauseIssues {
    fn new() -> ClauseIssues {
        ClauseIssues {
            clause: Vec::new(),
            repetitions: Vec::new(),
            count: 0usize,
        }
    }
    fn add(&mut self, lit: Literal, repeated: bool) {
        self.clause.push(lit);
        if repeated {
            self.repetitions.push(lit)
        } else {
            self.count += 1usize;
        }
    }
    fn is_ok(&self) -> bool {
        self.repetitions.is_empty()
    }
    fn is_qed(&self) -> bool {
        self.count == 0usize || (self.count == 1usize && unsafe { self.clause.get_unchecked(0usize) } == &Literal::Bottom)
    }
    fn clear(&mut self, lits: &mut LiteralSet) {
        for &lit in &self.clause {
            lits.clear(lit);
        }
        self.clause.clear();
        self.repetitions.clear();
    }
    fn invalid_clause(&mut self, pos: &FilePositionRef<'_>, stats: &mut IntegrityStats, id: Option<ClauseIndex>) {
        stats.errors += 1u64;
        warning!("invalid clause" @ pos, lock, {
            append!(lock, "The clause {} is introduced ", ClauseDisplay(&self.clause));
            if let Some(cid) = id {
                append!(lock, "with identifier {} ", cid);
            }
            append!(lock, "in {} #{}; ", stats.kind(), stats.count());
            if self.repetitions.len() <= 1 {
                append!(lock, "the literal ");
            } else {
                append!(lock, "literals ");
            }
            append!(lock, "{} in the clause are repeated.", CsvDisplay(&self.repetitions));
        })
    }
}

pub struct ChainIssues {
    chain: Vec<ClauseIndex>,
    missing: Vec<ClauseIndex>,
}
impl ChainIssues {
    fn new() -> ChainIssues {
        ChainIssues {
            chain: Vec::new(),
            missing: Vec::new(),
        }
    }
    fn add(&mut self, id: ClauseIndex, missing: bool) {
        self.chain.push(id);
        if missing {
            self.missing.push(id)
        }
    }
    fn is_ok(&self) -> bool {
        self.missing.is_empty()
    }
    fn clear(&mut self) {
        self.chain.clear();
        self.missing.clear();
    }
    fn missing_id(&mut self, pos: &FilePositionRef<'_>, stats: &mut IntegrityStats, id: ClauseIndex, lateral: Option<ClauseIndex>) {
        stats.errors += 1u64;
        warning!("missing clause identifier" @ pos, lock, {
            append!(lock, "A clause is introduced as a ");
            match lateral {
                None => append!(lock, "RUP inference "),
                Some(_) => append!(lock, "WSR inference "),
            }
            append!(lock, "with identifier {} in {} #{} ", id, stats.kind(), stats.count());
            match lateral {
                None => append!(lock, "through the RUP chain {}; ", ChainDisplay(&self.chain)),
                Some(lat) => append!(lock, "through a WSR multichain which contains the RUP chain {} for the lateral clause with identifier {};", ChainDisplay(&self.chain), lat),
            }
            if self.missing.len() <= 1 {
                append!(lock, "the clause with identifier ");
            } else {
                append!(lock, "clauses with identifiers ");
            }
            append!(lock, "{} in the chain are missing.", CsvDisplay(&self.missing));
        })
    }
}

pub struct WitnessIssues {
    witness: Vec<(Variable, Literal)>,
    repeated: Vec<(Variable, Literal)>,
    conflict: Vec<(Variable, Vec<Literal>)>,
}
impl WitnessIssues {
    fn new() -> WitnessIssues {
        WitnessIssues {
            witness: Vec::new(),
            repeated: Vec::new(),
            conflict: Vec::new(),
        }
    }
    fn add(&mut self, var: Variable, lit: Literal, ins: SubstitutionInsertion) {
        self.witness.push((var, lit));
        match ins {
            SubstitutionInsertion::Repeated => self.insert_repeated(var, lit),
            SubstitutionInsertion::Inconsistent => self.insert_inconsistent(var, lit),
            _ => (),
        }
    }
    fn insert_repeated(&mut self, var: Variable, lit: Literal) {
        if self.repeated.iter().find(|&&(v, _)| v == var).is_none() {
            self.repeated.push((var, lit));
        }
    }
    fn insert_inconsistent(&mut self, var: Variable, lit: Literal) {
        match self.conflict.iter_mut().find(|&&mut (v, _)| v == var) {
            Some((_, vec)) => vec.push(lit),
            None => {
                let (_, l) = self.witness.iter().find(|&&(v, _)| v == var).unwrap();
                self.conflict.push((var, vec![*l, lit]));
            },
        }
    }
    fn is_ok(&self) -> bool {
        self.repeated.is_empty() && self.conflict.is_empty()
    }
    fn clear(&mut self, subst: &mut Substitution) {
        subst.clear();
        self.witness.clear();
        self.repeated.clear();
        self.conflict.clear();
    }
    fn invalid_witness(&mut self, pos: &FilePositionRef<'_>, stats: &mut IntegrityStats, id: ClauseIndex) {
        stats.errors += 1u64;
        warning!("invalid substitution witness" @ pos, lock, {
            append!(lock, "A clause is introduced as a WSR with identifier {} in {} #{} ", id, stats.kind(), stats.count());
            append!(lock, "with the witness {}; ", SubstitutionDisplay(&self.witness));
            if !self.repeated.is_empty() {
                if self.repeated.len() <= 1usize {
                    append!(lock, "the mapping {} in the witness is repeated", CsvDisplayMap(|x| MappingDisplay(x), &self.repeated));
                } else {
                    append!(lock, "mappings {} in the witness are repeated", CsvDisplayMap(|x| MappingDisplay(x), &self.repeated));
                }
            }
            if !self.repeated.is_empty() && !self.conflict.is_empty() {
                append!(lock, " and ");
            }
            if !self.conflict.is_empty() {
                let mut vec: Vec<(Variable, Literal)> = Vec::new();
                for (var, lits) in &self.conflict {
                    for lit in lits {
                        vec.push((*var, *lit));
                    }
                }
                if self.conflict.len() <= 1usize {
                    append!(lock, "the mapping {} in the witness are inconsistent", CsvDisplayMap(|x| MappingDisplay(x), &vec));
                } else {
                    append!(lock, "mappings {} in the witness are inconsistent", CsvDisplayMap(|x| MappingDisplay(x), &vec));
                }
            }
            append!(lock, ".");
        })
    }
}