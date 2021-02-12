use std::{
    path::{PathBuf},
    mem::{self},
    time::{Instant, Duration},
};

use crate::{
    io::{DeferredPosition},
    clauseset::{ClauseSet},
    basic::{InstructionNumber, ClauseIndex, Literal, Variable},
    textparser::{PrepParsedInstructionKind, PrepParsedInstruction, TextAsrParser, TextClauseParser},
    substitution::{Substitution},
    formula::{Formula},
    propagation::{ExplicitPropagationResult, PropagationError, PropagationChecker},
    display::{DisplayClause, DisplayLiteralCsv, DisplaySubstitution},
    checkerdb::{CheckerDb, ClauseAddress},
    buffer::{UncheckedClauseBuffer},
    chain::{ChainHolder},
};

pub enum CorrectnessError {
    IncorrectCore(DeferredPosition, ClauseIndex, InstructionNumber, Vec<Literal>),
    IncorrectRupChain(DeferredPosition, ClauseIndex, InstructionNumber, Vec<Literal>, PropagationError),
    IncorrectWsrChain(DeferredPosition, ClauseIndex, InstructionNumber, Vec<Literal>, Vec<(Variable, Literal)>, ClauseIndex, Vec<Literal>, PropagationError),
}
impl CorrectnessError {
    pub fn show(&self) {
        let serious = self.is_serious();
        match self {
            CorrectnessError::IncorrectCore(pos, id, num, clause) => {
                error!("incorrect core clause" @ pos, lock, {
                    append!(lock, "The core clause {} with clause identifier {} is introduced in {}", DisplayClause(&clause), id, num);
                    append!(lock, ", but this clause does not occur in the premise CNF formula.");
                });
            },
            CorrectnessError::IncorrectRupChain(pos, id, num, clause, pe) if serious => {
                error!("incorrect RUP inference" @ pos, lock, {
                    append!(lock, "The RUP inference {} with clause identifier {} is introduced in {}", DisplayClause(&clause), id, num);
                    append!(lock, " through the following RUP chain:");
                    breakline!(lock);
                    append!(lock, "  assumptions: {}", DisplayLiteralCsv(pe.assumptions()));
                    for (cid, cls, res) in pe.propagations() {
                        breakline!(lock);
                        append!(lock, "  {:<.15}: {}   ", cid, DisplayClause(cls));
                        match res {
                            ExplicitPropagationResult::Idle(first, second) => append!(lock, "idle (literals {} and {} are unassigned)", first, second),
                            ExplicitPropagationResult::Satisfied(lit) => append!(lock, "satisfied (literal {} is satisfied)", lit),
                            ExplicitPropagationResult::Triggered(lit) => append!(lock, "triggered (propagates literal {})", lit),
                            ExplicitPropagationResult::Falsified => append!(lock, "falsified (all literals are falsified)"),
                            ExplicitPropagationResult::Conflict => append!(lock, "conflict was already reached"),
                        }
                    }
                    breakline!(lock);
                    append!(lock, "  no conflict is reached");
                });
            },
            CorrectnessError::IncorrectRupChain(pos, id, num, clause, pe) => {
                warning!("incorrect RUP inference" @ pos, lock, {
                    append!(lock, "The RUP inference {} with clause identifier {} is introduced in {}", DisplayClause(&clause), id, num);
                    append!(lock, " through the following RUP chain:");
                    breakline!(lock);
                    append!(lock, "  assumptions: {}", DisplayLiteralCsv(pe.assumptions()));
                    for (cid, cls, res) in pe.propagations() {
                        breakline!(lock);
                        append!(lock, "  {:<.15}: {}   ", cid, DisplayClause(cls));
                        match res {
                            ExplicitPropagationResult::Idle(first, second) => append!(lock, "idle (literals {} and {} are unassigned)", first, second),
                            ExplicitPropagationResult::Satisfied(lit) => append!(lock, "satisfied (literal {} is satisfied)", lit),
                            ExplicitPropagationResult::Triggered(lit) => append!(lock, "triggered (propagates literal {})", lit),
                            ExplicitPropagationResult::Falsified => append!(lock, "falsified (all literals are falsified)"),
                            ExplicitPropagationResult::Conflict => append!(lock, "conflict was already reached"),
                        }
                    }
                });
            },
            CorrectnessError::IncorrectWsrChain(pos, id, num, clause, witness, lat, lclause, pe) if serious => {
                error!("incorrect WSR inference" @ pos, lock, {
                    append!(lock, "The WSR inference {} with clause identifier {} is introduced in {}", DisplayClause(&clause), id, num);
                    append!(lock, " through the witness {}. ", DisplaySubstitution(&witness));
                    append!(lock, "The following RUP chain is given for the lateral clause {} with clause identifier {}:", DisplayClause(&lclause), lat);
                    breakline!(lock);
                    append!(lock, "  assumptions: {}", DisplayLiteralCsv(pe.assumptions()));
                    for (cid, cls, res) in pe.propagations() {
                        breakline!(lock);
                        append!(lock, "  {:<.15}: {}   ", cid, DisplayClause(cls));
                        match res {
                            ExplicitPropagationResult::Idle(first, second) => append!(lock, "idle (literals {} and {} are unassigned)", first, second),
                            ExplicitPropagationResult::Satisfied(lit) => append!(lock, "satisfied (literal {} is satisfied)", lit),
                            ExplicitPropagationResult::Triggered(lit) => append!(lock, "triggered (propagates literal {})", lit),
                            ExplicitPropagationResult::Falsified => append!(lock, "falsified (all literals are falsified)"),
                            ExplicitPropagationResult::Conflict => append!(lock, "conflict was already reached"),
                        }
                    }
                });
            },
            CorrectnessError::IncorrectWsrChain(pos, id, num, clause, witness, lat, lclause, pe) => {
                warning!("incorrect WSR inference" @ pos, lock, {
                    append!(lock, "The WSR inference {} with clause identifier {} is introduced in {}", DisplayClause(&clause), id, num);
                    append!(lock, " through the witness {}. ", DisplaySubstitution(&witness));
                    append!(lock, "The following RUP chain is given for the lateral clause {} with clause identifier {}:", DisplayClause(&lclause), lat);
                    breakline!(lock);
                    append!(lock, "  assumptions: {}", DisplayLiteralCsv(pe.assumptions()));
                    for (cid, cls, res) in pe.propagations() {
                        breakline!(lock);
                        append!(lock, "  {:<.15}: {}   ", cid, DisplayClause(cls));
                        match res {
                            ExplicitPropagationResult::Idle(first, second) => append!(lock, "idle (literals {} and {} are unassigned)", first, second),
                            ExplicitPropagationResult::Satisfied(lit) => append!(lock, "satisfied (literal {} is satisfied)", lit),
                            ExplicitPropagationResult::Triggered(lit) => append!(lock, "triggered (propagates literal {})", lit),
                            ExplicitPropagationResult::Falsified => append!(lock, "falsified (all literals are falsified)"),
                            ExplicitPropagationResult::Conflict => append!(lock, "conflict was already reached"),
                        }
                    }
                });
            },
        }
    }
    pub fn is_serious(&self) -> bool {
        match self {
            CorrectnessError::IncorrectCore(_, _, _, _) => true,
            CorrectnessError::IncorrectRupChain(_, _, _, _, pe) => pe.is_serious(),
            CorrectnessError::IncorrectWsrChain(_, _, _, _, _, _, _, pe) => pe.is_serious(),
        }
    }
}

pub struct CorrectnessConfig {
    pub select: u64,
    pub parts: u64,
    pub insertions: Vec<u64>,
    pub permissive: bool,
}
impl CorrectnessConfig {
    fn lower(&self, total: u64) -> u64 {
        (((total as u128) * (self.select as u128)) / (self.parts as u128)) as u64
    }
    fn upper(&self, total: u64) -> u64 {
        u64::max((((total as u128) * (self.select as u128 + 1u128)) / (self.parts as u128)) as u64, total)
    }
}

pub struct CorrectnessStats {
    pub errors: Vec<CorrectnessError>,
    pub warnings: Vec<CorrectnessError>,
    pub part: (u64, u64),
    pub processed: u64,
    start_time: Instant,
    pub rewind_time: Option<Duration>,
    pub check_time: Option<Duration>,
}
impl CorrectnessStats {
    fn new(config: &CorrectnessConfig) -> CorrectnessStats {
        CorrectnessStats {
            errors: Vec::new(),
            warnings: Vec::new(),
            part: (config.select, config.parts),
            processed: 0u64,
            start_time: Instant::now(),
            rewind_time: None,
            check_time: None,
        }
    }
    pub fn record_rewind_time(&mut self) {
        self.rewind_time = Some(Instant::now().duration_since(self.start_time));
        self.start_time = Instant::now();
    }
    pub fn record_check_time(&mut self) {
        self.check_time = Some(Instant::now().duration_since(self.start_time));
        self.start_time = Instant::now();
    }
    fn new_processed(&mut self) {
        self.processed += 1u64;
    }
    fn log_error(&mut self, err: CorrectnessError) {
        err.show();
        if err.is_serious() {
            self.errors.push(err);
        } else {
            self.warnings.push(err);
        }
    }
}

pub struct CorrectnessChecker {
    db: CheckerDb,
    prop: PropagationChecker,
    clause: UncheckedClauseBuffer,
    chain: ChainHolder,
    subst: Substitution,
    formula: Formula,
    stats: CorrectnessStats,
    config: CorrectnessConfig,
    laterals: Vec<ClauseIndex>,
    next: Option<u64>,
    counter: u64,
    begin: u64,
    end: u64,
    current: InstructionNumber,
    original: PathBuf,
    deferred: PathBuf,
}
impl CorrectnessChecker {
    pub fn new(config: CorrectnessConfig) -> CorrectnessChecker {
        CorrectnessChecker {
            db: CheckerDb::new(),
            prop: PropagationChecker::new(),
            clause: UncheckedClauseBuffer::new(),
            chain: ChainHolder::new(),
            subst: Substitution::new(),
            formula: Formula::new(),
            stats: CorrectnessStats::new(&config),
            config: config,
            laterals: Vec::new(),
            next: None,
            counter: 0u64,
            begin: 0u64,
            end: 0u64,
            current: InstructionNumber::new_premise(),
            original: PathBuf::from("(unknown)"),
            deferred: PathBuf::from("(unknown)"),
        }
    }
    pub fn check(mut self, mut cnf: TextAsrParser<'_>, mut asr: TextAsrParser<'_>) -> CorrectnessStats {
        self.original = asr.path().to_path_buf();
        self.deferred = if let Some((_, src)) = asr.parse_source_header() {
            src
        } else {
            PathBuf::from("(unknown)")
        };
        let (_, total) = asr.parse_count_header(true).unwrap();
        self.begin = self.config.lower(total);
        self.end = self.config.upper(total);
        let mut set = ClauseSet::new();
        self.load_cnf_formula(&mut set, &mut cnf);
        self.next = self.config.insertions.pop();
        self.check_asr_core(&mut set, &mut asr);
        set.deallocate_clauses(&mut self.db);
        mem::drop(set);
        self.check_asr_proof(&mut asr);
        self.stats.record_check_time();
        self.stats
    }
    fn load_cnf_formula(&mut self, set: &mut ClauseSet, cnf: &mut TextAsrParser<'_>) {
        cnf.parse_cnf_header();
        while let Some(_) = cnf.parse_premise() {
            let addr = self.parse_clause(cnf.parse_clause());
            set.insert(&self.db, addr);
        }
    }
    fn check_asr_core(&mut self, set: &mut ClauseSet, asr: &mut TextAsrParser<'_>) {
        self.current = InstructionNumber::new_core();
        asr.parse_core_header();
        if !self.must_finish() {
            while let Some(ins) = asr.parse_instruction(true, false) {
                match ins.kind() {
                    PrepParsedInstructionKind::Core => self.process_asr_core_instruction(asr, set, &ins),
                    _ => (),
                }
                if self.must_finish() {
                    break;
                }
            }
        }
    }
    fn check_asr_proof(&mut self, asr: &mut TextAsrParser<'_>) {
        self.current = InstructionNumber::new_proof();
        asr.parse_proof_header();
        if !self.must_finish() {
            while let Some(ins) = asr.parse_instruction(false, true) {
                match ins.kind() {
                    PrepParsedInstructionKind::Rup => self.process_rup_instruction(asr, &ins),
                    PrepParsedInstructionKind::Wsr => self.process_wsr_instruction(asr, &ins),
                    PrepParsedInstructionKind::Del => self.process_del_instruction(asr, &ins),
                    _ => (),
                }
                if self.must_finish() {
                    break;
                }
            }
        }
    }
    fn process_asr_core_instruction(&mut self, asr: &mut TextAsrParser<'_>, set: &mut ClauseSet, ins: &PrepParsedInstruction) {
        self.next_instruction();
        let insert = self.must_insert();
        let check = self.must_check();
        let clause_ps = asr.parse_clause();
        if insert || check {
            let addr = if check {
                self.check_core_instruction(set, clause_ps, ins)
            } else {
                self.parse_clause(clause_ps)
            };
            self.formula.insert(*ins.index(), addr);
        } else {
            mem::drop(clause_ps);
        }
    }
    fn process_rup_instruction(&mut self, asr: &mut TextAsrParser<'_>, ins: &PrepParsedInstruction) {
        self.next_instruction();
        let insert = self.must_insert();
        let check = self.must_check();
        let id = *ins.index();
        let clause_ps = asr.parse_clause();
        let addr = if insert || check {
            let addr = self.parse_clause(clause_ps);
            self.formula.insert(id, addr);
            Some(addr)
        } else {
            mem::drop(clause_ps);
            None
        };
        let mut chain_ps = asr.parse_chain();
        if check {
            self.stats.new_processed();
            while let Some(cid) = chain_ps.next() {
                self.chain.push_id(cid);
            }
            let res = {
                let central = self.db.retrieve_clause(addr.unwrap());
                let mut checker = self.prop.rup_check(central);
                checker.check_chain(Some(self.chain.chain()), &self.db, &self.formula, self.config.permissive)
            };
            if !res {
                self.incorrect_rup(ins, addr.unwrap());
            }
            self.chain.clear();
        } else {
            mem::drop(chain_ps);
        }
    }
    fn process_wsr_instruction(&mut self, asr: &mut TextAsrParser<'_>, ins: &PrepParsedInstruction) {
        self.next_instruction();
        let insert = self.must_insert();
        let check = self.must_check();
        let id = *ins.index();
        let clause_ps = asr.parse_clause();
        let addr = if insert || check {
            let addr = self.parse_clause(clause_ps);
            self.formula.insert(id, addr);
            Some(addr)
        } else {
            mem::drop(clause_ps);
            None
        };
        if check {
            self.stats.new_processed();
            let mut witness_ps = asr.parse_witness();
            while let Some((var, lit)) = witness_ps.next() {
                self.subst.insert(var, lit);
            }
            mem::drop(witness_ps);
            let mut mchain_ps = asr.parse_multichain(id);
            while let Some((lid, spec)) = mchain_ps.next() {
                if let Some(mut chain_ps) = spec {
                    self.chain.open_chain();
                    while let Some(cid) = chain_ps.next() {
                        self.chain.push_id(cid);
                    }
                    self.chain.push_chain(lid);
                } else {
                    self.chain.push_deletion(lid);
                }
            }
            let central = self.db.retrieve_clause(addr.unwrap());
            let mut checker = self.prop.wsr_check(central);
            for lid in self.formula.clauses() {
                if let Some(opt_chain) = self.chain.spec(lid) {
                    let laddr = self.formula.get(lid).unwrap();
                    let lateral = self.db.retrieve_clause(laddr);
                    let mut subchecker = checker.rup_check(lateral, &self.subst);
                    if !subchecker.check_chain(opt_chain, &self.db, &self.formula, self.config.permissive) {
                        self.laterals.push(lid);
                    }
                }
            }
            mem::drop(checker);
            if !self.laterals.is_empty() {
                self.incorrect_wsr(ins, addr.unwrap());
            }
            for &id in self.chain.deletions() {
                let clause_addr = self.formula.take(id).unwrap();
                self.db.deallocate_clause(clause_addr);
            }
            self.laterals.clear();
            self.chain.clear();
            self.subst.clear();
        } else {
            mem::drop(asr.parse_witness());
            mem::drop(asr.parse_chain());
        }
    }
    fn process_del_instruction(&mut self, _: &mut TextAsrParser<'_>, ins: &PrepParsedInstruction) {
        self.next_instruction();
        if self.must_check() {
            self.stats.new_processed();
            let addr = self.formula.take(*ins.index()).unwrap();
            self.db.deallocate_clause(addr);
        }
    }
    fn check_core_instruction(&mut self, set: &mut ClauseSet, mut ps: TextClauseParser<'_, '_>, ins: &PrepParsedInstruction) -> ClauseAddress {
        self.stats.new_processed();
        while let Some(lit) = ps.next() {
            self.clause.push(lit);
        }
        let opt_addr = set.take(&self.db, self.clause.get());
        let addr = match opt_addr {
            Some(addr) => addr,
            None => {
                self.incorrect_core(ins);
                self.db.allocate_clause(self.clause.get())
            },
        };
        self.clause.clear();
        addr
    }
    fn parse_clause(&mut self, mut ps: TextClauseParser<'_, '_>) -> ClauseAddress {
        while let Some(lit) = ps.next() {
            self.clause.push(lit);
        }
        let addr = self.db.allocate_clause(self.clause.get());
        self.clause.clear();
        addr 
    }
    fn next_instruction(&mut self) {
        self.current.next();
        self.counter += 1u64;
    }
    fn must_insert(&mut self) -> bool {
        let must_insert = self.next.map(|num| num == self.counter).unwrap_or(false);
        if must_insert {
            self.next = self.config.insertions.pop();
        }
        must_insert
    }
    fn must_check(&mut self) -> bool {
        if self.begin == self.counter {
            self.stats.record_rewind_time();
        }
        self.begin <= self.counter
    }
    fn must_finish(&self) -> bool {
        self.counter >= self.end
    }
    fn incorrect_core(&mut self, ins: &PrepParsedInstruction) {
        let clause = self.clause.extract();
        let pos = ins.position(&self.original, &self.deferred);
        self.stats.log_error(CorrectnessError::IncorrectCore(pos, *ins.index(), self.current, clause));
    }
    fn incorrect_rup(&mut self, ins: &PrepParsedInstruction, central: ClauseAddress) {
        let clause = self.db.retrieve_clause(central);
        let mut checker = self.prop.explicit_rup_check(clause);
        checker.check_chain(Some(self.chain.chain()), &self.db, &self.formula);
        let pe = checker.extract_propagations();
        let pos = ins.position(&self.original, &self.deferred);
        self.stats.log_error(CorrectnessError::IncorrectRupChain(pos, *ins.index(), self.current, clause.into_iter().copied().collect(), pe));
    }
    fn incorrect_wsr(&mut self, ins: &PrepParsedInstruction, central: ClauseAddress) {
        for &lid in &self.laterals {
            let clause = self.db.retrieve_clause(central);
            let witness = self.subst.into_iter().map(|(var, lit)| (*var, *lit)).collect();
            let laddr = self.formula.get(lid).unwrap();
            let lateral = self.db.retrieve_clause(laddr);
            let mut checker = self.prop.explicit_wsr_check(clause, lateral, &self.subst);
            checker.check_chain(self.chain.spec(lid).unwrap(), &self.db, &self.formula);
            let pe = checker.extract_propagations();
            let pos = ins.position(&self.original, &self.deferred);
            let clausevec = clause.into_iter().copied().collect();
            let lateralvec = lateral.into_iter().copied().collect();
            self.stats.log_error(CorrectnessError::IncorrectWsrChain(pos, *ins.index(), self.current, clausevec, witness, lid, lateralvec, pe));
        }
    }
}