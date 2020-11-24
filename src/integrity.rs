use std::{
    time::{Instant},
    path::{Path},
};

use crate::{
    basic::{MaybeClauseIndex, MaybeVariable, Literal, ClauseIndex, Variable, InstructionNumber, InstructionNumberKind},
    mapping::{LiteralSet, IndexSet, InstructionLoader},
    substitution::{SubstitutionInsertion, Substitution},
    textparser::{TextAsrParser, TextClauseParser, TextWitnessParser, TextChainParser, TextMultichainParser, AsrParsedInstructionKind},
    io::{FilePositionRef, FilePosition},
    display::{CsvDisplay, ClauseDisplay, SubstitutionDisplay, MappingDisplay, ChainDisplay, CsvDisplayMap},
};

pub struct EqFilePosition(pub FilePosition);
impl EqFilePosition {
    fn dummy() -> EqFilePosition {
        EqFilePosition(FilePosition::new(Path::new(""), false))
    }
}
impl PartialEq for EqFilePosition {
    fn eq(&self, _: &EqFilePosition) -> bool {
        true
    }
}

#[derive(PartialEq)]
pub enum IntegrityError {
    InvalidNumberOfVariables(EqFilePosition, MaybeVariable, MaybeVariable),
    InvalidNumberOfClauses(EqFilePosition, u64, u64),
    InvalidInstructionNumber(EqFilePosition, InstructionNumber, InstructionNumberKind),
    InvalidClause(EqFilePosition, Option<ClauseIndex>, InstructionNumber, Vec<Literal>, Vec<Literal>),
    InvalidWitness(EqFilePosition, ClauseIndex, InstructionNumber, Vec<(Variable, Literal)>, Vec<(Variable, Literal)>, Vec<(Variable, Vec<Literal>)>),
    ConflictingId(EqFilePosition, ClauseIndex, InstructionNumber),
    ChainMissingId(EqFilePosition, ClauseIndex, InstructionNumber, Option<ClauseIndex>, Vec<ClauseIndex>, Vec<ClauseIndex>),
    MissingIdSubchain(EqFilePosition, ClauseIndex, InstructionNumber, ClauseIndex, bool),
    RepeatedIdSubchain(EqFilePosition, ClauseIndex, InstructionNumber, ClauseIndex, bool),
    MissingDeletionId(EqFilePosition, ClauseIndex, InstructionNumber),
    MissingQed(EqFilePosition),
}

pub struct IntegrityStats {
    pub max_variable: MaybeVariable,
    pub max_clauseid: MaybeClauseIndex,
    pub num_premises: InstructionNumber,
    pub num_cores: InstructionNumber,
    pub num_instructions: InstructionNumber,
    pub num_rup: u64,
    pub num_wsr: u64,
    pub num_del: u64,
    pub first_qed: Option<InstructionNumber>,
    pub errors: Vec<IntegrityError>,
    pub start_time: Instant,
    pub cnf_time: Instant,
    pub asr_time: Instant,
}
impl IntegrityStats {
    pub fn new() -> IntegrityStats {
        let now = Instant::now();
        IntegrityStats {
            max_variable: MaybeVariable::new(None),
            max_clauseid: MaybeClauseIndex::new(None),
            num_premises: InstructionNumber::new(InstructionNumberKind::Premise),
            num_cores: InstructionNumber::new(InstructionNumberKind::Core),
            num_instructions: InstructionNumber::new(InstructionNumberKind::Proof),
            num_rup: 0u64,
            num_wsr: 0u64,
            num_del: 0u64,
            first_qed: None,
            errors: Vec::new(),
            start_time: now,
            cnf_time: now,
            asr_time: now,
        }
    }
    pub fn is_ok(&self) -> bool {
        self.errors.is_empty()
    }
    fn invalid_num_variables(&mut self, pos: &FilePositionRef<'_>, var: MaybeVariable) {
        error!("incorrect number of variables" @ pos, lock, {
            append!(lock, "The DIMACS header declares {} variables, ", var);
            append!(lock, "but the maximum variable throughout the premises is {}.", self.max_variable);
        });
        self.errors.push(IntegrityError::InvalidNumberOfVariables(
            EqFilePosition(pos.position()), var, self.max_variable));
    }
    fn invalid_num_clauses(&mut self, pos: &FilePositionRef<'_>, cls: u64) {
        error!("incorrect number of clauses" @ pos, lock, {
            append!(lock, "The DIMACS header declares {} clauses, ", cls);
            append!(lock, "but the premises contain {} clauses.", self.num_premises.number());
        });
        self.errors.push(IntegrityError::InvalidNumberOfClauses(
            EqFilePosition(pos.position()), cls, self.num_premises.number()));
    }
    fn invalid_instruction_number(&mut self, pos: &FilePositionRef<'_>, section: InstructionNumberKind, num: InstructionNumber) {
        error!("invalid instruction number" @ pos, lock, {
            append!(lock, "An instruction references '{}' as instruction number, ", num.text());
            append!(lock, "but it is located in the {} section of the ASR file.", match section {
                InstructionNumberKind::Premise => "premise",
                InstructionNumberKind::Core => "core",
                InstructionNumberKind::Proof => "proof",
            });
        });
        self.errors.push(IntegrityError::InvalidInstructionNumber(
            EqFilePosition(pos.position()), num, section))
    }
    fn conflict_id(&mut self, pos: &FilePositionRef<'_>, section: InstructionNumberKind, id: ClauseIndex) {
        let count = *self.count(section);
        error!("conflicting clause identifier" @ pos, lock, {
            append!(lock, "A clause is introduced with identifier {} in {}, ", id, count);
            append!(lock, "but this identifier is already in use for another clause.");
        });
        self.errors.push(IntegrityError::ConflictingId(
            EqFilePosition(pos.position()), id, count));
    }
    fn missing_id_subchain(&mut self, pos: &FilePositionRef<'_>, central: ClauseIndex, lateral: ClauseIndex, subchain: bool) {
        error!("missing lateral subchain" @ pos, lock, {
            append!(lock, "A clause is introduced as a WSR inference ");
            append!(lock, "with identifier {} in {}; ", central, self.num_instructions);
            append!(lock, "the WSR multichain specifies ");
            if subchain {
                append!(lock, "a RUP chain ");
            } else {
                append!(lock, "an implicit deletion ");
            }
            append!(lock, "for the lateral clause with identifier {}, but this clause is missing.", lateral);
        });
        self.errors.push(IntegrityError::MissingIdSubchain(
            EqFilePosition(pos.position()), central, self.num_instructions, lateral, subchain));
    }
    fn repeated_id_subchain(&mut self, pos: &FilePositionRef<'_>, central: ClauseIndex, lateral: ClauseIndex, subchain: bool) {
        error!("repeated lateral subchain" @ pos, lock, {
            append!(lock, "A clause is introduced as a WSR inference ");
            append!(lock, "with identifier {} in {}; ", central, self.num_instructions);
            append!(lock, "the WSR multichain specifies ");
            if subchain {
                append!(lock, "a RUP chain ");
            } else {
                append!(lock, "an implicit deletion ");
            }
            append!(lock, "for the lateral clause with identifier {}, ", lateral);
            append!(lock, "but another RUP chain or implicit deletion is specified for this lateral clause in the same multichain.");
        });
        self.errors.push(IntegrityError::RepeatedIdSubchain(
            EqFilePosition(pos.position()), central, self.num_instructions, lateral, subchain));
    }
    fn missing_deletion_id(&mut self, pos: &FilePositionRef<'_>, id: ClauseIndex) {
        error!("missing clause deletion identifier" @ pos, lock, {
            append!(lock, "A clause with identifier {} is deleted in {}, ", id, self.num_instructions);
            append!(lock, "but this clause is missing.");
        });
        self.errors.push(IntegrityError::MissingDeletionId(
            EqFilePosition(pos.position()), id, self.num_instructions))
    }
    fn missing_qed(&mut self, pos: &FilePositionRef<'_>) {
        error!("missing contradiction" @ pos, lock, {
            append!(lock, "Neither the core nor the proof contain a contradiction clause.");
        });
        self.errors.push(IntegrityError::MissingQed(
            EqFilePosition(pos.position())))
    }
    fn invalid_clause(&mut self, pos: &FilePositionRef<'_>, section: InstructionNumberKind, issues: &ClauseIssues, id: Option<ClauseIndex>) {
        let count = *self.count(section);
        error!("invalid clause" @ pos, lock, {
            append!(lock, "The clause {} is introduced ", ClauseDisplay(&issues.clause));
            if let Some(cid) = id {
                append!(lock, "with identifier {} ", cid);
            }
            append!(lock, "in {}; ", count);
            if issues.repetitions.len() <= 1 {
                append!(lock, "the literal {} in the clause is repeated.", CsvDisplay(&issues.repetitions));
            } else {
                append!(lock, "literals {} in the clause are repeated.", CsvDisplay(&issues.repetitions));
            }
        });
        self.errors.push(IntegrityError::InvalidClause(
            EqFilePosition(pos.position()), id, count, issues.clause.clone(), issues.repetitions.clone()));
    }
    fn missing_id(&mut self, pos: &FilePositionRef<'_>, issues: &ChainIssues, id: ClauseIndex, lateral: Option<ClauseIndex>) {
        error!("missing clause identifier" @ pos, lock, {
            append!(lock, "A clause is introduced as a ");
            match lateral {
                None => append!(lock, "RUP inference "),
                Some(_) => append!(lock, "WSR inference "),
            }
            append!(lock, "with identifier {} in {} ", id, self.num_instructions);
            match lateral {
                None => append!(lock, "through the RUP chain {}; ", ChainDisplay(&issues.chain)),
                Some(lat) => append!(lock, "through a WSR multichain which contains the RUP chain {} for the lateral clause with identifier {}; ", ChainDisplay(&issues.chain), lat),
            }
            if issues.missing.len() <= 1 {
                append!(lock, "the clause with identifier {} in the chain is missing", CsvDisplay(&issues.missing));
            } else {
                append!(lock, "clauses with identifiers {} in the chain are missing", CsvDisplay(&issues.missing));
            }
        });
        self.errors.push(IntegrityError::ChainMissingId(
            EqFilePosition(pos.position()), id, self.num_instructions, lateral, issues.chain.clone(), issues.missing.clone()))
    }
    fn invalid_witness(&mut self, pos: &FilePositionRef<'_>, issues: &WitnessIssues, id: ClauseIndex) {
        error!("invalid substitution witness" @ pos, lock, {
            append!(lock, "A clause is introduced as a WSR with identifier {} in {} ", id, self.num_instructions);
            append!(lock, "with the witness {}; ", SubstitutionDisplay(&issues.witness));
            if !issues.repeated.is_empty() {
                if issues.repeated.len() <= 1usize {
                    append!(lock, "the mapping {} in the witness is repeated", CsvDisplayMap(|x| MappingDisplay(x), &issues.repeated));
                } else {
                    append!(lock, "mappings {} in the witness are repeated", CsvDisplayMap(|x| MappingDisplay(x), &issues.repeated));
                }
            }
            if !issues.repeated.is_empty() && !issues.conflict.is_empty() {
                append!(lock, " and ");
            }
            if !issues.conflict.is_empty() {
                let mut vec: Vec<(Variable, Literal)> = Vec::new();
                for (var, lits) in &issues.conflict {
                    for lit in lits {
                        vec.push((*var, *lit));
                    }
                }
                if issues.conflict.len() <= 1usize {
                    append!(lock, "the mapping {} in the witness are inconsistent", CsvDisplayMap(|x| MappingDisplay(x), &vec));
                } else {
                    append!(lock, "mappings {} in the witness are inconsistent", CsvDisplayMap(|x| MappingDisplay(x), &vec));
                }
            }
            append!(lock, ".");
        });
        self.errors.push(IntegrityError::InvalidWitness(
            EqFilePosition(pos.position()), id, self.num_instructions, issues.witness.clone(), issues.repeated.clone(), issues.conflict.clone()))
    }
    fn error_instruction_number(&mut self, num: InstructionNumber) -> ! {
        panick!("checker capacity exceeded", lock, {
            append!(lock, "The input proof contains more than {} premise clauses, core clauses or proof instructions, which exceeds the checker capacity of {}.", num.number(), InstructionNumber::MaxValue);
        })
    }
    fn count(&self, section: InstructionNumberKind) -> &InstructionNumber {
        match section {
            InstructionNumberKind::Premise => &self.num_premises,
            InstructionNumberKind::Core => &self.num_cores,
            InstructionNumberKind::Proof => &self.num_instructions,
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
    fn increase_premises(&mut self) {
        self.num_premises = self.num_premises.succ().unwrap_or_else(|| self.error_instruction_number(self.num_premises))
    }
    fn increase_cores(&mut self) {
        self.num_cores = self.num_cores.succ().unwrap_or_else(|| self.error_instruction_number(self.num_cores))
    }
    fn increase_instructions(&mut self) {
        self.num_instructions = self.num_instructions.succ().unwrap_or_else(|| self.error_instruction_number(self.num_instructions))
    }
    fn record_qed(&mut self, section: InstructionNumberKind) {
        if let None = self.first_qed {
            let new = *self.count(section);
            self.first_qed = Some(new);
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
    loader: InstructionLoader,
    limit: InstructionNumber,
    stats: IntegrityStats,
}
impl IntegrityVerifier {
    pub fn new(limit: Option<InstructionNumber>) -> IntegrityVerifier {
        IntegrityVerifier {
            clause: ClauseIssues::new(),
            chain: ChainIssues::new(),
            witness: WitnessIssues::new(),
            lits: LiteralSet::new(),
            slots: IndexSet::new(),
            marks: IndexSet::new(),
            subchains: Vec::new(),
            subst: Substitution::new(),
            loader: InstructionLoader::new(),
            limit: limit.unwrap_or(InstructionNumber::new(InstructionNumberKind::Premise)),
            stats: IntegrityStats::new(),
        }
    }
    pub fn check(mut self, mut cnf: TextAsrParser<'_>, mut asr: TextAsrParser<'_>) -> (IntegrityStats, Vec<InstructionNumber>) {
        self.check_cnf(&mut cnf);
        self.check_asr(&mut asr);
        if self.stats.first_qed.is_none() {
            self.stats.missing_qed(&asr.position());
        }
        (self.stats, self.loader.extract())
    }
    fn check_cnf(&mut self, cnf: &mut TextAsrParser<'_>) {
        let (cnfpos, cnfvar, cnfcls) = cnf.parse_cnf_header();
        while let Some(pos) = cnf.parse_premise() {
            self.stats.increase_premises();
            self.check_clause(cnf.parse_clause(), &pos, InstructionNumberKind::Premise, None);
        }
        if cnfvar < self.stats.max_variable {
            self.stats.invalid_num_variables(&cnfpos, cnfvar);
        }
        if cnfcls as u64 != self.stats.num_premises.number() {
            self.stats.invalid_num_clauses(&cnfpos, cnfcls);
        }
        self.stats.record_cnf_time();
    }
    fn check_asr(&mut self, asr: &mut TextAsrParser<'_>) {
        asr.parse_core_header();
        while let Some((id, pos)) = asr.parse_core() {
            self.stats.increase_cores();
            self.check_instruction_number(asr, &pos, InstructionNumberKind::Core);
            self.check_clause(asr.parse_clause(), &pos, InstructionNumberKind::Core, Some(id));
            if self.slots.check_set(id) {
                self.stats.conflict_id(&pos, InstructionNumberKind::Core, id);
            }
            if self.stats.num_cores < self.limit {
                self.loader.insert(id, self.stats.num_cores);
            }
        }
        asr.parse_proof_header();
        while let Some((ins, pos)) = asr.parse_instruction() {
            self.stats.max_clauseid = self.stats.max_clauseid | ins.id;
            self.stats.increase_instructions();
            match ins.kind {
                AsrParsedInstructionKind::Rup => {
                    self.stats.num_rup += 1u64;
                    self.check_instruction_number(asr, &pos, InstructionNumberKind::Proof);
                    self.check_clause(asr.parse_clause(), &pos, InstructionNumberKind::Proof, Some(ins.id));
                    self.check_chain(asr.parse_chain(), &pos, ins.id, None);
                    if self.slots.check_set(ins.id) {
                        self.stats.conflict_id(&pos, InstructionNumberKind::Proof, ins.id);
                    }
                    if self.stats.num_instructions < self.limit {
                        self.loader.insert(ins.id, self.stats.num_instructions);
                    }
                },
                AsrParsedInstructionKind::Wsr => {
                    self.stats.num_wsr += 1u64;
                    self.check_instruction_number(asr, &pos, InstructionNumberKind::Proof);
                    self.check_clause(asr.parse_clause(), &pos, InstructionNumberKind::Proof, Some(ins.id));
                    self.check_substitution(asr.parse_witness(), &pos, ins.id);
                    self.check_multichain(asr.parse_multichain(ins.id), &pos, ins.id);
                    if self.slots.check_set(ins.id) {
                        self.stats.conflict_id(&pos, InstructionNumberKind::Proof, ins.id);
                    }
                    if self.stats.num_instructions < self.limit {
                        self.loader.insert(ins.id, self.stats.num_instructions);
                    }
                },
                AsrParsedInstructionKind::Del => {
                    self.stats.num_del += 1u64;
                    if !self.slots.check_clear(ins.id) {
                        self.stats.missing_deletion_id(&pos, ins.id);
                    }
                    if self.stats.num_instructions < self.limit {
                        self.loader.remove(ins.id)
                    }
                },
            }
        }
        self.stats.record_asr_time();
    }
    fn check_instruction_number(&mut self, asr: &mut TextAsrParser<'_>, pos: &FilePositionRef<'_>, section: InstructionNumberKind) {
        if let Some(num) = asr.parse_instruction_number() {
            if num.kind() != section {
                self.stats.invalid_instruction_number(pos, section, num);
            }
        }
    }
    fn check_clause(&mut self, mut ps: TextClauseParser<'_, '_>, pos: &FilePositionRef<'_>, section: InstructionNumberKind, optid: Option<ClauseIndex>) {
        while let Some(lit) = ps.next() {
            self.stats.max_variable |= lit;
            let repeated = self.lits.check_set(lit);
            self.clause.add(lit, repeated);
        }
        if !self.clause.is_ok() {
            self.stats.invalid_clause(pos, section, &self.clause, optid);
        }
        if optid.is_some() && self.clause.is_qed() {
            self.stats.record_qed(section);
        }
        self.clause.clear(&mut self.lits)
    }
    fn check_substitution(&mut self, mut ps: TextWitnessParser<'_, '_>, pos: &FilePositionRef<'_>, id: ClauseIndex) {
        while let Some((var, lit)) = ps.next() {
            self.stats.max_variable |= var;
            self.stats.max_variable |= lit;
            let ins = self.subst.insert(var, lit);
            self.witness.add(var, lit, ins);
        }
        if !self.witness.is_ok() {
            self.stats.invalid_witness(pos, &self.witness, id);
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
            self.stats.missing_id(pos, &self.chain, central, lateral)
        }
        self.chain.clear();
    }
    fn check_multichain(&mut self, mut ps: TextMultichainParser<'_, '_>, pos: &FilePositionRef<'_>, central: ClauseIndex) {
        while let Some((lat, subps)) = ps.next() {
            self.stats.max_clauseid = self.stats.max_clauseid | lat;
            if !self.slots.check(lat) && lat != central {
                self.stats.missing_id_subchain(pos, central, lat, subps.is_some());
            } else if self.marks.check_set(lat) {
                self.stats.repeated_id_subchain(pos, central, lat, subps.is_some());
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
        self.count = 0usize;
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


    fn test_integrity_file(cnf: &Path, asr: &Path, err: Vec<IntegrityError>) {
        let cnf_parser = TextAsrParser::new(InputReader::new(File::open(cnf).unwrap(), cnf, false));
        let asr_parser = TextAsrParser::new(InputReader::new(File::open(asr).unwrap(), asr, false));
        let intg = IntegrityVerifier::new();
        assert!(intg.check(cnf_parser, asr_parser).errors == err);
    }

    #[test]
    fn test_integrity() {
        MutedOutput.lock().unwrap().mute();
        test_integrity_file(Path::new("test/integrity/integrity.cnf"), Path::new("test/integrity/integrity.asr"),
            vec![]);
        test_integrity_file(Path::new("test/integrity/no_variables.cnf"), Path::new("test/integrity/integrity.asr"),
            vec![IntegrityError::InvalidNumberOfVariables(
                EqFilePosition::dummy(),
                MaybeVariable::try_from(19i64).unwrap(),
                MaybeVariable::try_from(20i64).unwrap())]);
        test_integrity_file(Path::new("test/integrity/no_clauses.cnf"), Path::new("test/integrity/integrity.asr"),
            vec![IntegrityError::InvalidNumberOfClauses(
                EqFilePosition::dummy(),
                46u64,
                45u64)]);
        test_integrity_file(Path::new("test/integrity/invalid.cnf"), Path::new("test/integrity/invalid.asr"),
            vec![IntegrityError::InvalidClause(
                EqFilePosition::dummy(),
                None,
                IntegritySection::Premise,
                46u64,
                vec![Literal::try_from(1i64).unwrap(), Literal::try_from(1i64).unwrap()],
                vec![Literal::try_from(1i64).unwrap()]),
            IntegrityError::InvalidClause(
                EqFilePosition::dummy(),
                Some(ClauseIndex::try_from(100i64).unwrap()),
                IntegritySection::Core,
                46u64,
                vec![Literal::try_from(1i64).unwrap(), Literal::try_from(2i64).unwrap(),
                    Literal::try_from(-3i64).unwrap(), Literal::try_from(-2i64).unwrap(),
                    Literal::try_from(-3i64).unwrap(), Literal::try_from(-4i64).unwrap(),
                    Literal::try_from(1i64).unwrap()],
                vec![Literal::try_from(-3i64).unwrap(), Literal::try_from(1i64).unwrap()]),
            IntegrityError::InvalidClause(
                EqFilePosition::dummy(),
                Some(ClauseIndex::try_from(101i64).unwrap()),
                IntegritySection::Proof,
                2u64,
                vec![Literal::try_from(1i64).unwrap(), Literal::try_from(2i64).unwrap(),
                    Literal::try_from(-3i64).unwrap(), Literal::try_from(-2i64).unwrap(),
                    Literal::try_from(-3i64).unwrap(), Literal::try_from(-4i64).unwrap()],
                vec![Literal::try_from(-3i64).unwrap()]),
            IntegrityError::InvalidWitness(
                EqFilePosition::dummy(),
                ClauseIndex::try_from(101i64).unwrap(),
                IntegritySection::Proof,
                2u64,
                vec![(Variable::try_from(4i64).unwrap(), Literal::try_from(5i64).unwrap()),
                    (Variable::try_from(4i64).unwrap(), Literal::try_from(5i64).unwrap()),
                    (Variable::try_from(9i64).unwrap(), Literal::try_from(10i64).unwrap()),
                    (Variable::try_from(10i64).unwrap(), Literal::try_from(-9i64).unwrap()),
                    (Variable::try_from(14i64).unwrap(), Literal::Top),
                    (Variable::try_from(14i64).unwrap(), Literal::Bottom),
                    (Variable::try_from(19i64).unwrap(), Literal::try_from(20i64).unwrap()),
                    (Variable::try_from(20i64).unwrap(), Literal::try_from(19i64).unwrap())],
                vec![(Variable::try_from(4i64).unwrap(), Literal::try_from(5i64).unwrap())],
                vec![(Variable::try_from(14i64).unwrap(), vec![Literal::Top, Literal::Bottom])])]);
        test_integrity_file(Path::new("test/integrity/integrity.cnf"), Path::new("test/integrity/id_conflict.asr"),
            vec![IntegrityError::ConflictingId(
                EqFilePosition::dummy(),
                ClauseIndex::try_from(45i64).unwrap(),
                IntegritySection::Core,
                46u64),
            IntegrityError::ConflictingId(
                EqFilePosition::dummy(),
                ClauseIndex::try_from(49i64).unwrap(),
                IntegritySection::Proof,
                5u64),
            IntegrityError::ConflictingId(
                EqFilePosition::dummy(),
                ClauseIndex::try_from(52i64).unwrap(),
                IntegritySection::Proof,
                9u64)]);
        test_integrity_file(Path::new("test/integrity/integrity.cnf"), Path::new("test/integrity/missing.asr"),
            vec![IntegrityError::ChainMissingId(
                EqFilePosition::dummy(),
                ClauseIndex::try_from(46i64).unwrap(),
                IntegritySection::Proof,
                1u64,
                Some(ClauseIndex::try_from(30i64).unwrap()),
                vec![ClauseIndex::try_from(6i64).unwrap(), ClauseIndex::try_from(100i64).unwrap()],
                vec![ClauseIndex::try_from(100i64).unwrap()]),
            IntegrityError::ChainMissingId(
                EqFilePosition::dummy(),
                ClauseIndex::try_from(50i64).unwrap(),
                IntegritySection::Proof,
                5u64,
                None,
                vec![ClauseIndex::try_from(46i64).unwrap(), ClauseIndex::try_from(100i64).unwrap(),
                    ClauseIndex::try_from(1i64).unwrap(), ClauseIndex::try_from(200i64).unwrap()],
                vec![ClauseIndex::try_from(100i64).unwrap(), ClauseIndex::try_from(200i64).unwrap()]),
            IntegrityError::MissingIdSubchain(
                EqFilePosition::dummy(),
                ClauseIndex::try_from(53i64).unwrap(),
                IntegritySection::Proof,
                12u64,
                ClauseIndex::try_from(100i64).unwrap(),
                true),
            IntegrityError::MissingIdSubchain(
                EqFilePosition::dummy(),
                ClauseIndex::try_from(53i64).unwrap(),
                IntegritySection::Proof,
                12u64,
                ClauseIndex::try_from(200i64).unwrap(),
                false),
            IntegrityError::RepeatedIdSubchain(
                EqFilePosition::dummy(),
                ClauseIndex::try_from(53i64).unwrap(),
                IntegritySection::Proof,
                12u64,
                ClauseIndex::try_from(10i64).unwrap(),
                true),
            IntegrityError::RepeatedIdSubchain(
                EqFilePosition::dummy(),
                ClauseIndex::try_from(55i64).unwrap(),
                IntegritySection::Proof,
                14u64,
                ClauseIndex::try_from(35i64).unwrap(),
                false),
            IntegrityError::MissingDeletionId(
                EqFilePosition::dummy(),
                ClauseIndex::try_from(49i64).unwrap(),
                IntegritySection::Proof,
                17u64),
            IntegrityError::MissingQed(EqFilePosition::dummy())]);
    }
}