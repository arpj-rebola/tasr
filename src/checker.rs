use std::{
    collections::{BTreeMap},
    time::{Instant},
};

use crate::{
    formula::{Formula},
    database::{Database},
    clause::{ClauseAddress, ClauseDatabase},
    basic::{InstructionNumberKind, InstructionNumber, Literal, ClauseIndex, Variable},
    clauseset::{ClauseSet},
    textparser::{TextAsrParser, TextClauseParser, AsrParsedInstructionKind, TextChainParser, TextMultichainParser},
    integrity::{EqFilePosition},
    io::{FilePositionRef},
    display::{ClauseDisplay, SubstitutionDisplay},
    propagation::{WsrChecker, PropagationResult, WsrCheck},
};

pub enum CheckingError {
    IncorrectCore(EqFilePosition, InstructionNumber, Option<InstructionNumber>, ClauseIndex, Vec<Literal>),
    IncorrectRup(EqFilePosition, InstructionNumber, Option<InstructionNumber>, ClauseIndex, Vec<Literal>, Vec<(ClauseIndex, PropagationResult, Vec<Literal>)>),
    IncorrectWsr(EqFilePosition, InstructionNumber, Option<InstructionNumber>, ClauseIndex, Vec<Literal>, Vec<(Variable, Literal)>, ClauseIndex, Vec<Literal>, Vec<(ClauseIndex, PropagationResult, Vec<Literal>)>),
}

pub enum CheckingWarning {
    UncompliantRup(EqFilePosition, InstructionNumber, Option<InstructionNumber>, ClauseIndex, Vec<Literal>, Vec<(ClauseIndex, PropagationResult, Vec<Literal>)>),
    UncompliantWsr(EqFilePosition, InstructionNumber, Option<InstructionNumber>, ClauseIndex, Vec<Literal>, Vec<(Variable, Literal)>, ClauseIndex, Vec<Literal>, Vec<(ClauseIndex, PropagationResult, Vec<Literal>)>),
}

pub struct CheckingStats {
    pub errors: Vec<CheckingError>,
    pub warnings: Vec<CheckingWarning>,
    pub start_time: Instant,
    pub rewind_time: Instant,
    pub check_time: Instant,
    pub cores: u64,
    pub first: u64,
    pub last: u64,
}
impl CheckingStats {
    pub fn new() -> CheckingStats {
        let now = Instant::now();
        CheckingStats {
            errors: Vec::new(),
            warnings: Vec::new(),
            start_time: now,
            rewind_time: now,
            check_time: now,
            cores: 0u64,
            first: 0u64,
            last: 0u64,
        }
    }
    fn record_rewind_time(&mut self) {
        let now = Instant::now();
        self.rewind_time = now;
        self.check_time = now;
    }
    fn record_check_time(&mut self) {
        let now = Instant::now();
        self.check_time = now;
    }
    fn incorrect_core(&mut self, pos: &FilePositionRef<'_>, id: ClauseIndex, num: InstructionNumber, pnum: Option<InstructionNumber>, clause: &Vec<Literal>) {
        error!("incorrect core clause" @ pos, lock, {
            append!(lock, "The core clause {} with clause identifier {} is introduced in {}", ClauseDisplay(clause), id, num);
            if let Some(pn) = pnum {
                append!(lock, " (corresponding to {} in the preprocessed proof)", pn);
            }
            append!(lock, ", but this clause does not occur in the premise CNF formula.");
        });
        self.errors.push(CheckingError::IncorrectCore(
            EqFilePosition(pos.position()), num, pnum, id, clause.clone()));
    }
    fn incorrect_rup(&mut self, pos: &FilePositionRef<'_>, id: ClauseIndex, num: InstructionNumber, pnum: Option<InstructionNumber>, clause: Vec<Literal>, issues: Vec<(ClauseIndex, PropagationResult, Vec<Literal>)>, conflict: bool) {
        if !conflict {
            error!("incorrect RUP inference" @ pos, lock, {
                append!(lock, "The RUP inference {} with clause identifier {} is introduced in {}", ClauseDisplay(&clause), id, num);
                if let Some(pn) = pnum {
                    append!(lock, " (corresponding to {} in the preprocessed proof)", pn);
                }
                append!(lock, " through the following RUP chain:");
                for (prop, id, cls) in &issues {
                    breakline!(lock);
                    append!(lock, "  {:<.15} {}: {}", id, prop, ClauseDisplay(cls));
                }
                breakline!(lock);
                append!(lock, "which does not reach a conflict.");
            });
            self.errors.push(CheckingError::IncorrectRup(
                EqFilePosition(pos.position()), num, pnum, id, clause.clone(), issues));
        } else {
            warning!("uncompliant RUP inference" @ pos, lock, {
                append!(lock, "The RUP inference {} with clause identifier {} is introduced in {}", ClauseDisplay(&clause), id, num);
                if let Some(pn) = pnum {
                    append!(lock, " (corresponding to {} in the preprocessed proof)", pn);
                }
                append!(lock, " through the following RUP chain:");
                for (prop, cid, ccls) in &issues {
                    breakline!(lock);
                    append!(lock, "  {:<.15} {}: {}", cid, prop, ClauseDisplay(ccls));
                }
                breakline!(lock);
                append!(lock, "which reaches a conflict but does not comply with the ASR specification.");
            });
            self.warnings.push(CheckingWarning::UncompliantRup(
                EqFilePosition(pos.position()), num, pnum, id, clause, issues));
        }
    }
    fn incorrect_wsr(&mut self, pos: &FilePositionRef<'_>, id: ClauseIndex, num: InstructionNumber, pnum: Option<InstructionNumber>, cls: Vec<Literal>, subst: Vec<(Variable, Literal)>, lid: ClauseIndex, lcls: Vec<Literal>, issues: Option<(Vec<(ClauseIndex, PropagationResult, Vec<Literal>)>, bool)>) {
        let serious = issues.as_ref().map_or(true, |&(_, conflict)| !conflict);
        if serious {
            error!("incorrect WSR inference" @ pos, lock, {
                append!(lock, "The WSR inference {} with clause identifier {} is introduced in {}", ClauseDisplay(&cls), id, num);
                if let Some(pn) = pnum {
                    append!(lock, " (corresponding to {} in the preprocessed proof)", pn);
                }
                append!(lock, " with substitution witness {}. ", SubstitutionDisplay(&subst));
                append!(lock, "For the lateral clause {} with clause identifier {}, ", ClauseDisplay(&lcls), lid);
                if let Some((seq, conflict)) = &issues {
                    append!(lock, "the following RUP chain is given:");
                    for (prop, cid, ccls) in seq {
                        breakline!(lock);
                        append!(lock, "  {:<.15} {}: {}", cid, prop, ClauseDisplay(ccls));
                    }
                    breakline!(lock);
                    if *conflict {
                        append!(lock, "which reaches a conflict but does not comply with the ASR specification.");
                    } else {
                        append!(lock, "which does not reach a conflict.");
                    }
                } else {
                    append!(lock, "no RUP chain is given, but the lateral clause is touched by the substitution witness and its combined clause is not a tautology.")
                }
            });
            let seq = match issues {
                Some((seq, _)) => seq,
                None => Vec::new(),
            };
            self.errors.push(CheckingError::IncorrectWsr(
                EqFilePosition(pos.position()), num, pnum, id, cls, subst, lid, lcls, seq));
        } else {
            warning!("uncompliant WSR inference" @ pos, lock, {
                append!(lock, "The WSR inference {} with clause identifier {} is introduced in {}", ClauseDisplay(&cls), id, num);
                if let Some(pn) = pnum {
                    append!(lock, " (corresponding to {} in the preprocessed proof)", pn);
                }
                append!(lock, " with substitution witness {}. ", SubstitutionDisplay(&subst));
                append!(lock, "For the lateral clause {} with clause identifier {}, ", ClauseDisplay(&lcls), lid);
                if let Some((seq, conflict)) = &issues {
                    append!(lock, "the following RUP chain is given:");
                    for (prop, cid, ccls) in seq {
                        breakline!(lock);
                        append!(lock, "  {:<.15} {}: {}", cid, prop, ClauseDisplay(ccls));
                    }
                    breakline!(lock);
                    if *conflict {
                        append!(lock, "which reaches a conflict but does not comply with the ASR specification.");
                    } else {
                        append!(lock, "which does not reach a conflict.");
                    }
                } else {
                    append!(lock, "no RUP chain is given, but the lateral clause is touched by the substitution witness and its combined clause is not a tautology.")
                }
            });
            let seq = match issues {
                Some((seq, _)) => seq,
                None => Vec::new(),
            };
            self.warnings.push(CheckingWarning::UncompliantWsr(
                EqFilePosition(pos.position()), num, pnum, id, cls, subst, lid, lcls, seq));
        }
    }
}

struct RupIssues {
    chain: Vec<ClauseIndex>,
}
impl RupIssues {
    fn new() -> RupIssues {
        RupIssues { chain: Vec::new() }
    }
    fn parse(&mut self, mut chain_ps: TextChainParser<'_, '_>) {
        while let Some(cid) = chain_ps.next() {
            self.chain.push(cid);
        }
    }
    fn check(&self, formula: &Formula, db: &Database, checker: &mut WsrChecker, addr: ClauseAddress) -> bool {
        let cls = unsafe { ClauseDatabase.retrieve(db, addr) };
        let mut rup = checker.rup(cls);
        for &id in &self.chain {
            let (_, addr) = formula.get(id).unwrap();
            let clause = unsafe { ClauseDatabase.retrieve(db, *addr) };
            if !rup.propagate(clause).is_ok() {
                return false;
            }
        }
        rup.conflict()
    }
    fn extract(&mut self, formula: &Formula, db: &Database, checker: &mut WsrChecker, addr: ClauseAddress) -> (Vec<Literal>, Vec<(ClauseIndex, PropagationResult, Vec<Literal>)>, bool) {
        let clause = unsafe { ClauseDatabase.retrieve(db, addr) };
        let cls: Vec<Literal> = clause.into_iter().copied().collect();
        let mut issues = Vec::new();
        let mut rup = checker.rup(clause);
        for &cid in &self.chain {
            let (_, addr) = formula.get(cid).unwrap();
            let cclause = unsafe { ClauseDatabase.retrieve(db, *addr) };
            let lits = cclause.into_iter().copied().collect();
            let prop = rup.propagate(cclause);
            issues.push((cid, prop, lits));
        }
        (cls, issues, rup.conflict())
    }
    #[inline]
    fn clear(&mut self) {
        self.chain.clear();
    }
}

struct WsrIssues {
    chain: Vec<ClauseIndex>,
    index: BTreeMap<ClauseIndex, Option<(usize, usize)>>,
    dels: Vec<ClauseIndex>,
}
impl WsrIssues {
    fn new() -> WsrIssues {
        WsrIssues {
            chain: Vec::new(),
            index: BTreeMap::new(),
            dels: Vec::new(),
        }
    }
    fn parse(&mut self, mut mchain_ps: TextMultichainParser<'_, '_>) {
        while let Some((id, subc)) = mchain_ps.next() {
            if let Some(mut chain_ps) = subc {
                let offset = self.chain.len();
                while let Some(cid) = chain_ps.next() {
                    self.chain.push(cid);
                }
                let end = self.chain.len();
                self.index.insert(id, Some((offset, end)));
            } else {
                self.index.insert(id, None);
                self.dels.push(id);
            }
        }
    }
    fn check_lateral(&self, formula: &Formula, db: &Database, checker: &mut WsrCheck<'_>, lid: ClauseIndex, laddr: ClauseAddress) -> bool {
        let lclause = unsafe { ClauseDatabase.retrieve(db, laddr) };
        let mut rup = checker.check(lclause);
        match self.index.get(&lid) {
            None => !rup.touched().unwrap() || rup.conflict(),
            Some(&Some((offset, end))) => {
                let chain = &self.chain[offset .. end];
                for &cid in chain {
                    let (_, caddr) = formula.get(cid).unwrap();
                    let cclause = unsafe { ClauseDatabase.retrieve(db, *caddr) };
                    if !rup.propagate(cclause).is_ok() {
                        return false;
                    }
                }
                rup.conflict()
            },
            Some(&None) => true,
        }
    }
    fn extract_lateral(&self, formula: &Formula, db: &Database, checker: &mut WsrCheck<'_>, id: ClauseIndex, lid: ClauseIndex, laddr: ClauseAddress) -> (Vec<(Variable, Literal)>, Vec<Literal>, Vec<Literal>, Option<(Vec<(ClauseIndex, PropagationResult, Vec<Literal>)>, bool)>) {
        let subst = checker.extract_substitution();
        let (_, addr) = formula.get(id).unwrap();
        let clause = unsafe { ClauseDatabase.retrieve(db, *addr) };
        let lits = clause.into_iter().copied().collect();
        let lclause = unsafe { ClauseDatabase.retrieve(db, laddr) };
        let llits = lclause.into_iter().copied().collect();
        let issues = if let Some(&Some((offset, end))) = self.index.get(&lid) {
            let mut rup = checker.check(lclause);
            let mut issues = Vec::new();
            let chain = &self.chain[offset .. end];
            for &cid in chain {
                let (_, caddr) = formula.get(cid).unwrap();
                let cclause = unsafe { ClauseDatabase.retrieve(db, *caddr) };
                let clits = cclause.into_iter().copied().collect();
                let cprop = rup.propagate(cclause);
                issues.push((cid, cprop, clits));
            }
            Some((issues, rup.conflict()))
        } else {
            None
        };
        (subst, lits, llits, issues)
    }
    fn deletions(&self) -> &[ClauseIndex] {
        &self.dels
    }
    #[inline]
    fn clear(&mut self) {
        self.chain.clear();
        self.index.clear();
        self.dels.clear();
    }
}

pub struct Checker {
    formula: Formula,
    database: Database,
    clausedb: ClauseDatabase,
    checker: WsrChecker,
    count: InstructionNumber,
    from: InstructionNumber,
    until: Option<InstructionNumber>,
    load: Vec<InstructionNumber>,
    rupbuf: RupIssues,
    wsrbuf: WsrIssues,
    stats: CheckingStats,
}
impl Checker {
    pub fn new(cnf: &mut TextAsrParser<'_>, asr: &mut TextAsrParser<'_>, from: InstructionNumber, until: Option<InstructionNumber>, load: Vec<InstructionNumber>) -> Checker {
        let mut chk = Checker {
            formula: Formula::new(),
            database: Database::new(),
            clausedb: ClauseDatabase,
            checker: WsrChecker::new(),
            count: InstructionNumber::new(InstructionNumberKind::Premise),
            from: if from <= InstructionNumber::new(InstructionNumberKind::Proof) {
                InstructionNumber::new(InstructionNumberKind::Core)
            } else {
                from
            },
            until: until,
            load: load,
            rupbuf: RupIssues::new(),
            wsrbuf: WsrIssues::new(),
            stats: CheckingStats::new(),
        };
        chk.load.reverse();
        chk.check_core(cnf, asr);
        chk
    }
    pub fn check(mut self, asr: &mut TextAsrParser<'_>) -> CheckingStats {
        let mut next = self.load.pop();
        let mut check = false;
        while let Some((ins, pos)) = asr.parse_instruction() {
            self.count = self.count.succ().unwrap();
            if self.until.map_or(false, |inum| self.count >= inum) {
                break;
            }
            if !check && self.from <= self.count {
                check = true;
                self.stats.first = self.count.number();
                self.stats.record_rewind_time();
            }
            match ins.kind {
                AsrParsedInstructionKind::Rup => {
                    let num = asr.parse_instruction_number();
                    if check {
                        self.check_rup(&pos, asr, ins.id, num);
                    } else {
                        self.rewind(asr, ins.id, &mut next, false);
                    }
                },
                AsrParsedInstructionKind::Wsr => {
                    let num = asr.parse_instruction_number();
                    if check {
                        self.check_wsr(&pos, asr, ins.id, num);
                    } else {
                        self.rewind(asr, ins.id, &mut next, true);
                    }
                },
                AsrParsedInstructionKind::Del => if check {
                    let (_, addr) = self.formula.remove(ins.id).unwrap();
                    unsafe { self.clausedb.remove(&mut self.database, addr) };
                },
            }
        }
        self.stats.last = {
            let number = self.count.number();
            if number == 0u64 {
                0u64
            } else {
                number - 1u64
            }
        };
        self.stats.record_check_time();
        self.stats
    }
    fn check_core(&mut self, cnf: &mut TextAsrParser<'_>, asr: &mut TextAsrParser<'_>) {
        let mut set = ClauseSet::new();
        let check = self.from <= InstructionNumber::new(InstructionNumberKind::Proof);
        if check {
            cnf.parse_cnf_header();
            while let Some(_) = cnf.parse_premise() {
                let addr = self.insert_clause(cnf.parse_clause());
                set.insert(&mut self.database, addr);
            }
        }
        asr.parse_core_header();
        let mut clause = Vec::<Literal>::new();
        let mut next = self.load.pop();
        self.count = InstructionNumber::new(InstructionNumberKind::Core);
        while let Some((id, pos)) = asr.parse_core() {
            self.count = self.count.succ().unwrap();
            let num = asr.parse_instruction_number();
            let mut clause_ps = asr.parse_clause();
            if next.map_or(false, |inum| inum == self.count) {
                let addr = if check {
                    while let Some(lit) = clause_ps.next() {
                        clause.push(lit);
                    }
                    match set.remove(&self.database, &clause) {
                        Some(addr) => addr,
                        None => {
                            self.stats.incorrect_core(&pos, id, self.count, num, &clause);
                            let mut wt = self.clausedb.open(&mut self.database);
                            for &lit in &clause {
                                wt.write(lit);
                            }
                            clause.clear();
                            wt.close()
                        }
                    }
                } else {
                    self.insert_clause(clause_ps)
                };
                self.formula.insert(id, self.count, addr);
                next = self.load.pop();
            }
        }
        if check {
            set.delete_all(&mut self.database);
        }
        self.stats.cores = self.count.number();
        self.count = InstructionNumber::new(InstructionNumberKind::Proof);
        if let Some(inum) = next {
            self.load.push(inum);
        }
    }
    fn rewind(&mut self, asr: &mut TextAsrParser<'_>, id: ClauseIndex, next: &mut Option<InstructionNumber>, wsr: bool) {
        {
            let mut clause_ps = asr.parse_clause();
            if next.map_or(false, |inum| inum == self.count) {
                let mut wt = self.clausedb.open(&mut self.database);
                while let Some(lit) = clause_ps.next() {
                    wt.write(lit);
                }
                let addr = wt.close();
                self.formula.insert(id, self.count, addr);
            }
        }
        if wsr {
            asr.parse_witness();
            asr.parse_multichain(id);
        } else {
            asr.parse_chain();
        }
    }
    fn check_rup(&mut self, pos: &FilePositionRef<'_>, asr: &mut TextAsrParser<'_>, id: ClauseIndex, num: Option<InstructionNumber>) {
        let addr = self.insert_clause(asr.parse_clause());
        self.formula.insert(id, self.count, addr);
        self.rupbuf.parse(asr.parse_chain());
        if !self.rupbuf.check(&self.formula, &self.database, &mut self.checker, addr) {
            let (cls, issues, conflict) = self.rupbuf.extract(&self.formula, &self.database, &mut self.checker, addr);
            self.stats.incorrect_rup(pos, id, self.count, num, cls, issues, conflict);
        }
        self.rupbuf.clear();
    }
    fn check_wsr(&mut self, pos: &FilePositionRef<'_>, asr: &mut TextAsrParser<'_>, id: ClauseIndex, num: Option<InstructionNumber>) {
        let addr = self.insert_clause(asr.parse_clause());
        self.formula.insert(id, self.count, addr);
        let mut witness = self.checker.wsr();
        {
            let mut witness_ps = asr.parse_witness();
            while let Some((var, lit)) = witness_ps.next() {
                witness.map(var, lit);
            }
        }
        let clause = unsafe { self.clausedb.retrieve(&self.database, addr) };
        let mut wsr = witness.close(clause);
        self.wsrbuf.parse(asr.parse_multichain(id));
        for (lid, _, laddr) in &self.formula {
            if !self.wsrbuf.check_lateral(&self.formula, &self.database, &mut wsr, lid, laddr) {
                let (subst, cls, lcls, issues) = self.wsrbuf.extract_lateral(&self.formula, &self.database, &mut wsr, id, lid, laddr);
                self.stats.incorrect_wsr(pos, id, self.count, num, cls, subst, lid, lcls, issues);
            }
        }
        for &did in self.wsrbuf.deletions() {
            let (_, addr) = self.formula.remove(did).unwrap();
            unsafe { self.clausedb.remove(&mut self.database, addr) };
        }
        self.wsrbuf.clear();
    }
    fn insert_clause(&mut self, mut ps: TextClauseParser<'_, '_>) -> ClauseAddress {
        let mut wt = self.clausedb.open(&mut self.database);
        while let Some(lit) = ps.next() {
            wt.write(lit);
        }
        wt.close()
    }
}