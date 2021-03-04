use std::{
    mem::{self},
    time::{Instant, Duration},
    path::{PathBuf},
    io::{Write, Read},
};

use either::{
    Either, Left, Right,
};

use crate::{
    io::{FilePosition, PathFilePosition, DeferredPosition, InputReader},
    basic::{Literal, Variable, ClauseIndex, InstructionNumber},
    model::{Model},
    substitution::{Substitution},
    idflags::{ClauseIndexFlags},
    display::{DisplayClause, DisplayLiteralCsv, DisplayLiteralPairCsv, DisplaySubstitution, DisplaySubstitutionMappingCsv, DisplaySubstitutionMultimappingCsv, DisplayChain, DisplayClauseIndexCsv},
    textparser::{PrepParsedInstruction, PrepParsedInstructionKind, TextAsrParser, TextClauseParser, TextChainParser, TextWitnessParser, TextMultichainParser},
    buffer::{ClauseBuffer, WitnessBuffer, ChainBuffer, MultichainBuffer},
};

pub struct VariableCounter {
    val: u32
}
impl VariableCounter {
    pub fn new() -> VariableCounter {
        VariableCounter { val: 0u32 }
    }
    pub fn update_variable(&mut self, var: Variable) {
        self.val = unsafe { self.val.max(mem::transmute::<Variable, u32>(var) >> 1) }
    }
    pub fn update_literal(&mut self, lit: Literal) {
        self.val = unsafe { self.val.max(mem::transmute::<Literal, u32>(lit) >> 1) }
    }
    pub fn get(&self) -> u32 {
        self.val
    }
}

pub struct ClauseIndexCounter {
    val: u64
}
impl ClauseIndexCounter {
    pub fn new() -> ClauseIndexCounter {
        ClauseIndexCounter { val: 0u64 }
    }
    pub fn update(&mut self, id: ClauseIndex) {
        self.val = unsafe { self.val.max(mem::transmute::<ClauseIndex, u64>(id)) }
    }
    pub fn get(&self) -> u64 {
        self.val
    }
}

pub enum IntegrityError {
    InvalidNumberOfVariables(PathFilePosition, u32, u32),
    InvalidNumberOfClauses(PathFilePosition, u64, u64),
    InvalidNumberOfInstructions(PathFilePosition, u64, u64),
    InvalidClause(DeferredPosition, Option<ClauseIndex>, InstructionNumber, Vec<Literal>, Vec<Literal>, Vec<(Literal, Literal)>),
    InvalidWitness(DeferredPosition, ClauseIndex, InstructionNumber, Vec<(Variable, Literal)>, Vec<(Variable, Literal)>, Vec<(Variable, Vec<Literal>)>),
    ConflictingId(DeferredPosition, ClauseIndex, InstructionNumber),
    ChainMissingId(DeferredPosition, ClauseIndex, InstructionNumber, Option<ClauseIndex>, Vec<ClauseIndex>, Vec<ClauseIndex>),
    InvalidLateralId(DeferredPosition, ClauseIndex, InstructionNumber, Vec<ClauseIndex>, Vec<ClauseIndex>, Vec<ClauseIndex>, Vec<ClauseIndex>),
    MissingDeletionId(DeferredPosition, ClauseIndex, InstructionNumber),
    MissingQed(PathFilePosition),
}
impl IntegrityError {
    pub fn show(&self) {
        match self {
            IntegrityError::InvalidNumberOfVariables(pos, declared, found) => {
                error!("incorrect number of variables" @ pos, lock, {
                    append!(lock, "The DIMACS header declares {} variables, ", declared);
                    append!(lock, "but the maximum variable throughout the premises is {}.", found);
                });
            },
            IntegrityError::InvalidNumberOfClauses(pos, declared, found) => {
                error!("incorrect number of clauses" @ pos, lock, {
                    append!(lock, "The DIMACS header declares {} clauses, ", declared);
                    append!(lock, "but the premises contain {} clauses.", found);
                });
            },
            IntegrityError::InvalidNumberOfInstructions(pos, declared, found) => {
                error!("incorrect number of instructions" @ pos, lock, {
                    append!(lock, "The ASR proof 'p count' header declares {} instructions, ", declared);
                    append!(lock, "but the ASR file contains {} core and proof instructions.", found);
                });
            },
            IntegrityError::InvalidClause(pos, opt_id, num, clause, reps, confs) => {
                error!("invalid clause" @ pos, lock, {
                    append!(lock, "The invalid clause {} is introduced ", DisplayClause(&clause));
                    if let Some(id) = opt_id {
                        append!(lock, "with identifier {} ", id);
                    }
                    append!(lock, "in {}.", num);
                    breakline!(lock);
                    if reps.len() == 1usize {
                        append!(lock, "The literal {} in the clause is repeated.", DisplayLiteralCsv(&reps));
                    } else if reps.len() > 1usize {
                        append!(lock, "Literals {} in the clause are repeated.", DisplayLiteralCsv(&reps));
                    }
                    if reps.len() != 0usize && confs.len() != 0usize {
                        breakline!(lock);
                    }
                    if reps.len() == 1usize {
                        append!(lock, "The clause contains the complementary pair of literals {}.", DisplayLiteralPairCsv(&confs));
                    } else if reps.len() > 1usize {
                        append!(lock, "The clause contains the complementary pairs of literals {}.", DisplayLiteralPairCsv(&confs));
                    }
                });
            },
            IntegrityError::InvalidWitness(pos, id, num, witness, reps, confs) => {
                error!("invalid substitution" @ pos, lock, {
                    append!(lock, "A clause is intrudiced as a WSR with identifier {} in {}", id, num);
                    append!(lock, "with the invalid witness substitution {}.", DisplaySubstitution(&witness));
                    breakline!(lock);
                    if reps.len() == 1usize {
                        append!(lock, "The mapping {} in the substitution is repeated.", DisplaySubstitutionMappingCsv(&reps));
                    } else if reps.len() > 1usize {
                        append!(lock, "Mappings {} in the substitution are repeated.", DisplaySubstitutionMappingCsv(&reps));
                    }
                    if reps.len() != 0usize && confs.len() != 0usize {
                        breakline!(lock);
                    }
                    if reps.len() == 1usize {
                        append!(lock, "The substitution contains the conflicting mapping {}.", DisplaySubstitutionMultimappingCsv(&confs));
                    } else if reps.len() > 1usize {
                        append!(lock, "The substitution contains the conflicting mappings {}.", DisplaySubstitutionMultimappingCsv(&confs));
                    }
                });
            },
            IntegrityError::ConflictingId(pos, id, num) => {
                error!("conflicting clause identifier" @ pos, lock, {
                    append!(lock, "A clause is introduced with identifier {} in {}, ", id, num);
                    append!(lock, "but this identifier is already in use for another clause.");
                });
            },
            IntegrityError::ChainMissingId(pos, id, num, latid, chain, missing) => {
                error!("missing clause identifier" @ pos, lock, {
                    append!(lock, "A clause is introduced as a ");
                    match latid {
                        None => append!(lock, "RUP inference "),
                        Some(_) => append!(lock, "WSR inference "),
                    }
                    append!(lock, "with identifier {} in {} ", id, num);
                    match latid {
                        None => append!(lock, "through the invalid RUP chain {}.", DisplayChain(&chain)),
                        Some(lat) => append!(lock, "through a WSR multichain which contains the invalid RUP chain {} for the lateral clause with identifier {}.", DisplayChain(&chain), lat),
                    }
                    breakline!(lock);
                    if missing.len() == 1usize {
                        append!(lock, "The RUP chain contains the missing clause identifier {}.", DisplayClauseIndexCsv(&missing));
                    } else if missing.len() > 1usize {
                        append!(lock, "The RUP chain contains the missing clause identifiers {}.", DisplayClauseIndexCsv(&missing));
                    }
                });
            },
            IntegrityError::InvalidLateralId(pos, id, num, laterals, chain_missing, dels_missing, repeated) => {
                error!("invalid lateral clause identifier" @ pos, lock, {
                    append!(lock, "A clause is introduced as a WSR inference with identifier {} in {} with an invalid WSR multichain ", id, num);
                    append!(lock, "containing RUP chains or implicit deletions for the lateral clause");
                    if laterals.len() <= 1usize {
                        append!(lock, " with identifier {}.", DisplayClauseIndexCsv(&laterals));
                    } else {
                        append!(lock, "s with identifiers {}.", DisplayClauseIndexCsv(&laterals));
                    }
                    if !chain_missing.is_empty() {
                        breakline!(lock);
                    }
                    if chain_missing.len() == 1usize {
                        append!(lock, "A RUP chain is specified for the lateral clause with identifier {}, but this clause is missing.", DisplayClauseIndexCsv(&chain_missing));
                    } else if chain_missing.len() > 1usize {
                        append!(lock, "RUP chains are specified for the lateral clauses with identifiers {}, but these clauses are missing.", DisplayClauseIndexCsv(&chain_missing));
                    }
                    if !dels_missing.is_empty() {
                        breakline!(lock);
                    }
                    if dels_missing.len() == 1usize {
                        append!(lock, "An implicit deletion is specified for the lateral clause with identifier {}, but this clause is missing.", DisplayClauseIndexCsv(&dels_missing));
                    } else if dels_missing.len() > 1usize {
                        append!(lock, "Implicit deletions are specified for the lateral clauses with identifiers {}, but these clauses are missing.", DisplayClauseIndexCsv(&dels_missing));
                    }
                    if !repeated.is_empty() {
                        breakline!(lock);
                    }
                    if repeated.len() == 1usize {
                        append!(lock, "Multiple RUP chains or implicit deletions are specified for the lateral clause with identifier {}.", DisplayClauseIndexCsv(&dels_missing));
                    } else if repeated.len() > 1usize {
                        append!(lock, "Multiple RUP chains or implicit deletions are specified for the lateral clauses with identifiers {}.", DisplayClauseIndexCsv(&dels_missing));
                    }
                });
            },
            IntegrityError::MissingDeletionId(pos, id, num) => {
                error!("missing clause deletion identifier" @ pos, lock, {
                    append!(lock, "A clause with identifier {} is deleted in {}, but this clause is missing.", id, num);
                });
            },
            IntegrityError::MissingQed(pos) => {
                error!("missing contradiction" @ pos, lock, {
                    append!(lock, "Neither the core nor the proof contain a contradiction clause.");
                });
            },
        }
    }
}

pub struct IntegrityStats {
    pub max_var: VariableCounter,
    pub max_id: ClauseIndexCounter,
    pub num_premises: u64,
    pub num_cores: u64,
    pub num_instructions: u64,
    pub num_rup: u64,
    pub num_wsr: u64,
    pub num_del: u64,
    pub first_qed: Option<(InstructionNumber, ClauseIndex)>,
    pub errors: Vec<IntegrityError>,
    start_time: Instant,
    pub cnf_time: Option<Duration>,
    pub asr_time: Option<Duration>,
}
impl IntegrityStats {
    fn new() -> IntegrityStats {
        IntegrityStats {
            max_var: VariableCounter::new(),
            max_id: ClauseIndexCounter::new(),
            num_premises: 0u64,
            num_cores: 0u64,
            num_instructions: 0u64,
            num_rup: 0u64,
            num_wsr: 0u64,
            num_del: 0u64,
            first_qed: None,
            errors: Vec::new(),
            start_time: Instant::now(),
            cnf_time: None,
            asr_time: None,
        }
    }
    fn record_cnf_time(&mut self) {
        self.cnf_time = Some(Instant::now().duration_since(self.start_time));
        self.start_time = Instant::now();
    }
    fn record_asr_time(&mut self) {
        self.asr_time = Some(Instant::now().duration_since(self.start_time));
        self.start_time = Instant::now();
    }
    fn new_premise(&mut self) {
        self.num_premises += 1u64;
    }
    fn new_core(&mut self) {
        self.num_cores += 1u64;
    }
    fn new_instruction(&mut self) {
        self.num_instructions += 1u64;
    }
    fn new_rup(&mut self) {
        self.num_rup += 1u64;
    }
    fn new_wsr(&mut self) {
        self.num_wsr += 1u64;
    }
    fn new_del(&mut self) {
        self.num_del += 1u64;
    }
    fn update_literal(&mut self, lit: Literal) {
        self.max_var.update_literal(lit)
    }
    fn update_variable(&mut self, var: Variable) {
        self.max_var.update_variable(var)
    }
    fn update_index(&mut self, id: ClauseIndex) {
        self.max_id.update(id)
    }
    fn check_variables(&self, vars: u32) -> bool {
        self.max_var.get() <= vars
    }
    fn check_clauses(&self, cls: u64) -> bool {
        self.num_premises == cls
    }
    fn log_error(&mut self, err: IntegrityError) {
        err.show();
        self.errors.push(err)
    }
}

pub struct IntegrityConfig {
    pub preprocessing: bool,
    pub select: u64,
    pub parts: u64,
}
impl IntegrityConfig {
    fn lower(&self, total: u64) -> u64 {
        (((total as u128) * (self.select as u128)) / (self.parts as u128)) as u64
    }
    fn upper(&self, total: u64) -> u64 {
        u64::max((((total as u128) * (self.select as u128 + 1u128)) / (self.parts as u128)) as u64, total)
    }
}

pub struct IntegrityData {
    pub insertions: Vec<u64>,
    pub end: InstructionNumber,
    pub qed: ClauseIndex,
}

pub struct IntegrityVerifier {
    clause: ClauseBuffer,
    chain: ChainBuffer,
    witness: WitnessBuffer,
    mchain: MultichainBuffer,
    model: Model,
    idflags: ClauseIndexFlags<u64>,
    subst: Substitution,
    insertions: Option<Vec<u64>>,
    stats: IntegrityStats,
    config: IntegrityConfig,
    current: InstructionNumber,
    countdown: u64,
    original: PathBuf,
    deferred: PathBuf,
}
impl IntegrityVerifier {
    pub fn new(config: IntegrityConfig) -> IntegrityVerifier {
        IntegrityVerifier {
            clause: ClauseBuffer::new(),
            chain: ChainBuffer::new(),
            witness: WitnessBuffer::new(),
            mchain: MultichainBuffer::new(),
            model: Model::new(),
            idflags: ClauseIndexFlags::new(),
            subst: Substitution::new(),
            insertions: None,
            stats: IntegrityStats::new(),
            config: config,
            countdown: u64::max_value(),
            current: InstructionNumber::new_premise(),
            original: PathBuf::from("(unknown)"),
            deferred: PathBuf::from("(unknown)"),
        }
    }
    pub fn check<R, S>(&mut self, mut cnf: TextAsrParser<R>, mut asr: TextAsrParser<S>) where
        R: Read,
        S: Read,
    {
        // Set the path to the CNF formula as the reference for error-reporting.
        self.original = cnf.path().to_path_buf();
        // Check the CNF formula.
        self.check_cnf(&mut cnf);
        // Make a new buffer.
        let mut buffer = Vec::<u8>::new();
        write!(&mut buffer, "p proof\n").unwrap_or_else(|e| panic!(format!("{}", e)));
        // Set the path to the ASR proof as the reference for error-reporting...
        self.original = asr.path().to_path_buf();
        // ... and possibly use the 'p source' header as a deferred reference.
        self.deferred = if let Some((_, src)) = asr.parse_source_header() {
            src
        } else {
            PathBuf::from("(unknown)")
        };
        let count_header = asr.parse_count_header(!self.config.preprocessing);
        if let Some((_, total)) = count_header {
            self.countdown = self.config.lower(total);
        }
        // Check the ASR core, storing any delayed instructions in the buffer.
        self.check_asr_core(&mut asr, &mut buffer);
        // Temporarily set alternative paths for error-reporting.
        let mut temp_path = PathBuf::from("(buffer)");
        mem::swap(&mut self.original, &mut temp_path);
        mem::swap(&mut self.deferred, &mut temp_path);
        // Check the ASR proof fragment corresponding to the buffer.
        let buffer_input = InputReader::new(&buffer[..], &self.original, false);
        let mut buffer_ps = TextAsrParser::new(buffer_input);
        self.check_asr_proof(&mut buffer_ps, true);
        // Restore the alternative paths for error-reporting, and drop all structures related to buffering.
        mem::swap(&mut self.deferred, &mut temp_path);
        mem::swap(&mut self.original, &mut temp_path);
        mem::drop(temp_path);
        mem::drop(buffer_ps);
        mem::drop(buffer);
        // Check the rest of the ASR proof.
        self.check_asr_proof(&mut asr, false);
        if let None = self.stats.first_qed {
            self.missing_qed(asr.position())
        }
        if let Some((pos, total)) = count_header {
            if total != self.stats.num_cores + self.stats.num_instructions {
                self.invalid_num_instructions(pos, total);
            }
        }
    }
    pub fn data(self) -> (IntegrityStats, Option<IntegrityData>) {
        let data = if self.stats.first_qed.is_none() || (!self.config.preprocessing && self.insertions.is_none()) {
            None
        } else {
            let (end, qed) = self.stats.first_qed.unwrap();
            Some(IntegrityData {
                insertions: self.insertions.unwrap_or_else(|| Vec::new()),
                end: end,
                qed: qed,
            })
        };
        (self.stats, data)
    }
    fn check_cnf<R>(&mut self, cnf: &mut TextAsrParser<R>) where
        R: Read
    {
        self.current = InstructionNumber::new_premise();
        let (cnfpos, cnfvar, cnfcls) = cnf.parse_cnf_header();
        while let Some(pos) = cnf.parse_premise() {
            self.current.next();
            self.check_clause(cnf.parse_clause(), Left(&pos));
            self.stats.new_premise();
        }
        if !self.stats.check_variables(cnfvar) {
            self.invalid_num_variables(cnfpos, cnfvar);
        }
        if !self.stats.check_clauses(cnfcls) {
            self.invalid_num_clauses(cnfpos, cnfcls);
        }
        self.stats.record_cnf_time();
    }
    fn check_asr_core<R>(&mut self, asr: &mut TextAsrParser<R>, buffer: &mut Vec<u8>) where
        R: Read
    {
        self.current = InstructionNumber::new_core();
        asr.parse_core_header();
        while let Some(ins) = asr.parse_instruction(true, self.config.preprocessing) {
            match ins.kind() {
                PrepParsedInstructionKind::Core => self.check_asr_core_instruction(asr, &ins),
                _ => self.save_instruction(asr, &ins, buffer),
            }
        }
    }
    fn check_asr_proof<R>(&mut self, asr: &mut TextAsrParser<R>, buffered: bool) where
        R: Read
    {
        self.current = if buffered { InstructionNumber::new_buffer() } else { InstructionNumber::new_proof() };
        asr.parse_proof_header();
        while let Some(ins) = asr.parse_instruction(false, true) {
            match ins.kind() {
                PrepParsedInstructionKind::Rup => self.check_asr_rup_instruction(asr, &ins),
                PrepParsedInstructionKind::Del => self.check_asr_del_instruction(asr, &ins),
                PrepParsedInstructionKind::Wsr => self.check_asr_wsr_instruction(asr, &ins),
                _ => (),
            }
        }
    }
    fn check_asr_core_instruction<R>(&mut self, asr: &mut TextAsrParser<R>, ins: &PrepParsedInstruction) where
        R: Read
    {
        self.next_instruction();
        let id = *ins.index();
        self.stats.update_index(id);
        if let Err(_) = self.idflags.insert(id, self.stats.num_cores + self.stats.num_instructions) {
            self.conflicting_id(ins);
        }
        self.check_clause(asr.parse_clause(), Right(ins));
        self.stats.new_core();
    }
    fn check_asr_rup_instruction<R>(&mut self, asr: &mut TextAsrParser<R>, ins: &PrepParsedInstruction) where
        R: Read
    {
        self.next_instruction();
        let id = *ins.index();
        self.stats.update_index(id);
        if let Err(_) = self.idflags.insert(id, self.stats.num_cores + self.stats.num_instructions) {
            self.conflicting_id(ins);
        }
        self.check_clause(asr.parse_clause(), Right(ins));
        self.check_chain(asr.parse_chain(), ins, None);
        self.stats.new_instruction();
        self.stats.new_rup();
    }
    fn check_asr_wsr_instruction<R>(&mut self, asr: &mut TextAsrParser<R>, ins: &PrepParsedInstruction) where
        R: Read
    {
        self.next_instruction();
        let id = *ins.index();
        self.stats.update_index(id);
        if let Err(_) = self.idflags.insert(id, self.stats.num_cores + self.stats.num_instructions) {
            self.conflicting_id(ins);
        }
        self.check_clause(asr.parse_clause(), Right(ins));
        self.check_witness(asr.parse_witness(), ins);
        self.check_multichain(asr.parse_multichain(id), ins);
        self.stats.new_instruction();
        self.stats.new_wsr();
    }
    fn check_asr_del_instruction<R>(&mut self, _: &mut TextAsrParser<R>, ins: &PrepParsedInstruction) where
        R: Read
    {
        self.next_instruction();
        let id = *ins.index();
        self.stats.update_index(id);
        if let Err(_) = self.idflags.remove(id) {
            self.missing_deletion_id(ins);
        }
        self.stats.new_instruction();
        self.stats.new_del();
    }
    fn check_clause<R>(&mut self, mut ps: TextClauseParser<'_, R>, data: Either<&FilePosition, &PrepParsedInstruction>) where
        R: Read
    {
        while let Some(lit) = ps.next() {
            self.stats.update_literal(lit);
            self.clause.push(&mut self.model, lit);
        }
        if let (Right(ins), None) = (data, self.stats.first_qed) {
            if self.clause.is_absurd() {
                self.stats.first_qed = Some((self.current, *ins.index()));
            }
        }
        if !self.clause.is_ok() {
            self.invalid_clause(data);
        }
        self.clause.clear(&mut self.model);
    }
    fn check_witness<R>(&mut self, mut ps: TextWitnessParser<'_, R>, ins: &PrepParsedInstruction) where
        R: Read
    {
        while let Some((var, lit)) = ps.next() {
            self.stats.update_variable(var);
            self.stats.update_literal(lit);
            self.witness.push(&mut self.subst, var, lit);
        }
        if !self.witness.is_ok() {
            self.invalid_witness(ins);
        }
        self.witness.clear(&mut self.subst);
    }
    fn check_chain<R>(&mut self, mut ps: TextChainParser<'_, R>, ins: &PrepParsedInstruction, lat: Option<ClauseIndex>) where
        R: Read
    {
        self.chain.exclude(*ins.index());
        while let Some(id) = ps.next() {
            self.stats.update_index(id);
            self.chain.push(&self.idflags, id);
        }
        if !self.chain.is_ok() {
            self.missing_chain_id(lat, ins);
        }
        self.chain.clear();
    }
    fn check_multichain<R>(&mut self, mut ps: TextMultichainParser<'_, R>, ins: &PrepParsedInstruction) where
        R: Read
    {
        while let Some((lat, sub)) = ps.next() {
            self.stats.update_index(lat);
            if let Some(chain_ps) = sub {
                self.mchain.push_dummy_chain(&mut self.idflags, lat);
                self.check_chain(chain_ps, ins, Some(lat));
            } else {
                self.mchain.push_deletion(&mut self.idflags, lat);
            }
        }
        if !self.mchain.is_ok() {
            self.invalid_lateral_id(ins);
        }
        for &id in self.mchain.deletions() {
            self.idflags.remove(id).unwrap();
        }
        self.mchain.clear(&mut self.idflags);
    }
    fn next_instruction(&mut self) {
        self.current.next();
        if self.countdown == 0u64 {
            self.extract_instructions();
            self.countdown = u64::max_value();
        } else {
            self.countdown -= 1u64;
        }
    }
    fn extract_instructions(&mut self) {
        let mut vec = self.idflags.instructions();
        vec.sort();
        vec.reverse();
        self.insertions = Some(vec)
    }
    fn save_instruction<R>(&mut self, asr: &mut TextAsrParser<R>, ins: &PrepParsedInstruction, buffer: &mut Vec<u8>) where
        R: Read
    {
        let res = match ins.kind() {
            PrepParsedInstructionKind::Core => asr.save_core_instruction(*ins.index(), ins.default_position(), buffer),
            PrepParsedInstructionKind::Rup => asr.save_rup_instruction(*ins.index(), ins.default_position(), buffer),
            PrepParsedInstructionKind::Del => asr.save_del_instruction(*ins.index(), ins.default_position(), buffer),
            PrepParsedInstructionKind::Wsr => asr.save_wsr_instruction(*ins.index(), ins.default_position(), buffer),
        };
        if let Err(e) = res {
            panic!(format!("{}", e))
        }
    }
    fn invalid_num_variables(&mut self, pos: FilePosition, vars: u32) {
        self.stats.log_error(IntegrityError::InvalidNumberOfVariables(pos.with_path(&self.original), vars, self.stats.max_var.get()));
    }
    fn invalid_num_clauses(&mut self, pos: FilePosition, cls: u64) {
        self.stats.log_error(IntegrityError::InvalidNumberOfClauses(pos.with_path(&self.original), cls, self.stats.num_premises));
    }
    fn invalid_num_instructions(&mut self, pos: FilePosition, num: u64) {
        self.stats.log_error(IntegrityError::InvalidNumberOfInstructions(pos.with_path(&self.original), num, self.stats.num_cores + self.stats.num_instructions));
    }
    fn invalid_clause(&mut self, data: Either<&FilePosition, &PrepParsedInstruction>) {
        let (clause, reps, confs) = self.clause.extract();
        let pos = data.either(|pos| pos.with_path(&self.original).deferred(), |ins| ins.position(&self.original, &self.deferred));
        let id = data.either(|_| None, |ins| Some(*ins.index()));
        self.stats.log_error(IntegrityError::InvalidClause(pos, id, self.current, clause, reps, confs));
    }
    fn invalid_witness(&mut self, ins: &PrepParsedInstruction) {
        let (witness, reps, confs) = self.witness.extract();
        let pos = ins.position(&self.original, &self.deferred);
        self.stats.log_error(IntegrityError::InvalidWitness(pos, *ins.index(), self.current, witness, reps, confs));
    }
    fn conflicting_id(&mut self, ins: &PrepParsedInstruction) {
        let pos = ins.position(&self.original, &self.deferred);
        self.stats.log_error(IntegrityError::ConflictingId(pos, *ins.index(), self.current));
    }
    fn missing_chain_id(&mut self, lat_id: Option<ClauseIndex>, ins: &PrepParsedInstruction) {
        let (chain, missing) = self.chain.extract();
        let pos = ins.position(&self.original, &self.deferred);
        self.stats.log_error(IntegrityError::ChainMissingId(pos, *ins.index(), self.current, lat_id, chain, missing));
    }
    fn invalid_lateral_id(&mut self, ins: &PrepParsedInstruction) {
        let (laterals, miss_chain, miss_del, reps) = self.mchain.extract();
        let pos = ins.position(&self.original, &self.deferred);
        self.stats.log_error(IntegrityError::InvalidLateralId(pos, *ins.index(), self.current, laterals, miss_chain, miss_del, reps));
    }
    fn missing_deletion_id(&mut self, ins: &PrepParsedInstruction) {
        let pos = ins.position(&self.original, &self.deferred);
        self.stats.log_error(IntegrityError::MissingDeletionId(pos, *ins.index(), self.current));
    }
    fn missing_qed(&mut self, pos: FilePosition) {
        self.stats.log_error(IntegrityError::MissingQed(pos.with_path(&self.original)));
    }
}

// #[cfg(test)]
// mod test {
//     fn check_instance(cnf: &Path, asr: &Path) {
//         let cnf_file = File::open(cnf);
//         let cnf_input = InputReader::new(cnf_file, cnf, false);
//         let cnf_parser = TextAsrParser::new(cnf_input);
//         let asr_file = File::open(asr);
//         let asr_input = InputReader::new(asr_file, asr, false);
//         let asr_parser = TextAsrParser::new(asr_input);
//         let config = IntegrityConfig {
//             preprocessing: false,
//             select: 1u64,
//             parts: 1u64,
//         };
//         let checker = IntegrityVerifier::new(config);
//         checker.check(&mut cnf_parser, &mut asr_parser);

//     }
//     #[test]
//     fn test_integrity() {

//     }
// }