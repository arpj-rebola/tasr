use std::{
    mem::{self},
};

use crate::{
    model::{Model, ModelValue},
    checkerdb::{Clause, Chain, CheckerDb},
    substitution::{Substitution},
    basic::{Literal, ClauseIndex},
    formula::{Formula},
};

pub struct PropagationChecker {
    model: Model,
    prop: Vec<Literal>,
}
impl PropagationChecker {
    pub fn new() -> PropagationChecker {
        let mut model = Model::new();
        model.set(Literal::Top);
        let prop = vec![Literal::Top];
        PropagationChecker {
            model: model,
            prop: prop,
        }
    }
    pub fn rup_check(&mut self, clause: Clause) -> RupChecker<'_> {
        let conflict = self.assign_complements(clause);
        RupChecker::<'_> {
            checker: self,
            undo: 1usize,
            conflict: conflict,
        }
    }
    pub fn wsr_check(&mut self, clause: Clause) -> WsrChecker<'_> {
        let conflict = self.assign_complements(clause);
        WsrChecker::<'_> {
            checker: self,
            conflict: conflict,
        }
    }
    pub fn explicit_rup_check(&mut self, clause: Clause) -> ExplicitChecker<'_> {
        let conflict = self.assign_complements(clause);
        let mut vec = Vec::new();
        for &lit in clause {
            vec.push(lit.complement());
        }
        ExplicitChecker::<'_> {
            checker: self,
            assumed: vec,
            conflict: conflict,
            propagations: Vec::new(),
        }
    }
    pub fn explicit_wsr_check(&mut self, clause: Clause, lateral: Clause, subst: &Substitution) -> ExplicitChecker<'_> {
        let mut conflict = self.assign_complements(clause);
        if !conflict {
            conflict = self.assign_mapped_complements(lateral, subst);
        }
        let mut vec = Vec::new();
        for &lit in clause {
            vec.push(lit.complement());
        }
        for &lit in lateral {
            vec.push(subst.map(lit.complement()));
        }
        ExplicitChecker::<'_> {
            checker: self,
            assumed: vec,
            conflict: conflict,
            propagations: Vec::new(),
        }
    }
    fn assign_complements(&mut self, clause: Clause) -> bool {
        for lit in clause {
            let comp = lit.complement();
            match self.model.check_set(comp) {
                ModelValue::Unassigned => self.prop.push(comp),
                ModelValue::True => (),
                ModelValue::False => return true,
            }
        }
        false
    }
    fn assign_mapped_complements(&mut self, clause: Clause, subst: &Substitution) -> bool {
        for lit in clause {
            let comp = subst.map(lit.complement());
            match self.model.check_set(comp) {
                ModelValue::Unassigned => self.prop.push(comp),
                ModelValue::True => (),
                ModelValue::False => return true,
            }
        }
        false
    }
}

pub struct RupChecker<'a> {
    checker: &'a mut PropagationChecker,
    undo: usize,
    conflict: bool,
}
impl<'a> RupChecker<'a> {
    pub fn check_chain(&mut self, opt_chain: Option<Chain<'_>>, db: &CheckerDb, f: &Formula) -> bool {
        if let Some(chain) = opt_chain {
            for &cid in chain {
                let cls = db.retrieve_clause(f.get(cid).unwrap());
                if !self.check_clause(cls) {
                    return false
                }
            }
        }
        return self.conflict()
    }
    pub fn check_clause(&mut self, clause: Clause<'_>) -> bool {
        if self.conflict {
            false
        } else {
            let mut prop = Literal::Bottom;
            let mut it = clause.into_iter();
            while let Some(&lit) = it.next() {
                match unsafe { self.checker.model.value(lit) } {
                    ModelValue::False => (),
                    ModelValue::Unassigned if prop == Literal::Bottom => prop = lit,
                    _ => return false,
                }
            }
            if prop == Literal::Bottom {
                self.conflict = true;
            } else {
                self.checker.model.set(prop);
                self.checker.prop.push(prop);
            }
            true
        }
    }
    pub fn conflict(&self) -> bool {
        self.conflict
    }
}
impl<'a> Drop for RupChecker<'a> {
    fn drop(&mut self) {
        for &lit in &self.checker.prop[self.undo ..] {
            self.checker.model.clear(lit);
        }
        self.checker.prop.truncate(self.undo);
    }
}

pub struct WsrChecker<'a> {
    checker: &'a mut PropagationChecker,
    conflict: bool,
}
impl<'a> WsrChecker<'a> {
    pub fn rup_check(&mut self, lateral: Clause, subst: &Substitution) -> RupChecker<'_> {
        let undo = self.checker.prop.len();
        let conflict = if self.conflict { true } else { self.checker.assign_mapped_complements(lateral, subst) };
        RupChecker::<'_> {
            checker: &mut *self.checker,
            undo: undo,
            conflict: conflict,
        }
    }
}
impl<'a> Drop for WsrChecker<'a> {
    fn drop(&mut self) {
        for &lit in &self.checker.prop {
            self.checker.model.clear(lit);
        }
        self.checker.prop.truncate(1usize);
    }
}

pub struct ExplicitChecker<'a> {
    checker: &'a mut PropagationChecker,
    assumed: Vec<Literal>,
    conflict: bool,
    propagations: Vec<(ClauseIndex, Vec<Literal>, ExplicitPropagationResult)>,
}
impl<'a> ExplicitChecker<'a> {
    pub fn check_chain(&mut self, opt_chain: Option<Chain<'_>>, db: &CheckerDb, f: &Formula) {
        if let Some(chain) = opt_chain {
            for &cid in chain {
                let cls = db.retrieve_clause(f.get(cid).unwrap());
                self.check_clause(cid, cls);
            }   
        }
    }
    pub fn check_clause(&mut self, id: ClauseIndex, clause: Clause) {
        let res = if self.conflict {
            ExplicitPropagationResult::Conflict
        } else {
            let mut prop = Literal::Bottom;
            let mut it = clause.into_iter();
            loop { match it.next() {
                Some(&lit) => match unsafe { self.checker.model.value(lit) } {
                    ModelValue::False => (),
                    ModelValue::Unassigned => if prop == Literal::Bottom {
                        prop = lit;
                    } else {
                        break ExplicitPropagationResult::Idle(prop, lit);
                    },
                    ModelValue::True => break ExplicitPropagationResult::Satisfied(lit),
                },
                None => if prop == Literal::Bottom {
                    self.conflict = true;
                    break ExplicitPropagationResult::Falsified;
                } else {
                    self.checker.model.set(prop);
                    self.checker.prop.push(prop);
                    break ExplicitPropagationResult::Triggered(prop);
                }
            } }
        };
        self.propagations.push((id, clause.into_iter().copied().collect(), res));
    }
    pub fn extract_propagations(mut self) -> PropagationError {
        let mut assumed = Vec::new();
        let mut propagations = Vec::new();
        mem::swap(&mut self.assumed, &mut assumed);
        mem::swap(&mut self.propagations, &mut propagations);
        PropagationError {
            assumed: assumed,
            propagations: propagations,
        }
    }
}
impl<'a> Drop for ExplicitChecker<'a> {
    fn drop(&mut self) {
        for &lit in &self.checker.prop[1usize ..] {
            self.checker.model.clear(lit);
        }
        self.checker.prop.truncate(1usize);
    }
}

#[repr(u8)]
pub enum PropagationResult {
    Idle,
    Satisfied,
    Triggered,
    Conflict,
}

pub enum ExplicitPropagationResult {
    Idle(Literal, Literal),
    Satisfied(Literal),
    Triggered(Literal),
    Falsified,
    Conflict,
}

pub struct PropagationError {
    assumed: Vec<Literal>,
    propagations: Vec<(ClauseIndex, Vec<Literal>, ExplicitPropagationResult)>,
}
impl PropagationError {
    pub fn is_serious(&self) -> bool {
        !self.propagations.iter().any(|(_, _, x)| match x {
            ExplicitPropagationResult::Falsified => true,
            _ => false,
        })
    }
    pub fn assumptions(&self) -> &[Literal] {
        &self.assumed[..]
    }
    pub fn propagations(&self) -> &[(ClauseIndex, Vec<Literal>, ExplicitPropagationResult)] {
        &self.propagations[..]
    }
}
