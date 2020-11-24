use std::{
    mem::{self, ManuallyDrop},
    fmt::{Display, Formatter, Result as FmtResult},
};

use crate::{
    basic::{Literal, Variable},
    substitution::{Substitution},
    mapping::{LiteralSet},
    clause::{Clause},
};

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum PropagationResult {
    Triggered = 0,
    Falsified = 1,
    Satisfied = 2,
    Idle = 3,
    Inconsistent = 4,
}
impl PropagationResult {
    pub fn is_ok(&self) -> bool {
        unsafe { mem::transmute::<&PropagationResult, &u32>(self) <= &1 }
    }
    pub fn is_conflict(&self) -> bool {
        if let PropagationResult::Falsified = self {
            true
        } else {
            false
        }
    }
}
impl Display for PropagationResult {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		match self {
            PropagationResult::Triggered => write!(f, "ok"),
            PropagationResult::Falsified => write!(f, "ok, conflict!"),
            PropagationResult::Satisfied => write!(f, "satisfied clause"),
            PropagationResult::Idle => write!(f, "non-propagating clause"),
            PropagationResult::Inconsistent => write!(f, "conflict already reached"),
        }
	}
}

pub struct RupCheck<'a> {
    stack: &'a mut Vec<Literal>,
    lits: &'a mut LiteralSet,
    conflict: bool,
    touched: Option<bool>,
}
impl<'a> RupCheck<'a> {
    pub fn propagate(&mut self, clause: Clause<'a>) -> PropagationResult {
        if self.conflict {
            return PropagationResult::Inconsistent;
        }
        let mut prop: Option<Literal> = None;
        for &lit in clause {
            if self.lits.check(lit) {
                return PropagationResult::Satisfied;
            } else if !self.lits.check(lit.complement()) {
                if let None = prop {
                    prop = Some(lit)
                } else {
                    return PropagationResult::Idle;
                }
            }
        }
        if let Some(lit) = prop {
            self.stack.push(lit);
            self.lits.set(lit);
            PropagationResult::Triggered
        } else {
            self.conflict = true;
            PropagationResult::Falsified
        }
    }
    pub fn conflict(&self) -> bool {
        self.conflict
    }
    pub fn touched(&self) -> Option<bool> {
        self.touched
    }
}
impl<'a> Drop for RupCheck<'a> {
    fn drop(&mut self) {
        for lit in &*self.stack {
            self.lits.clear(*lit);
        }
        self.stack.clear();
    }
}

pub struct WsrCheck<'a> {
    central: &'a mut Vec<Literal>,
    stack: &'a mut Vec<Literal>,
    lits: &'a mut LiteralSet,
    subst: &'a mut Substitution,
    conflict: bool,
}
impl<'a> WsrCheck<'a> {
    pub fn check<'b, 'c>(&'b mut self, clause: Clause<'_>) -> RupCheck<'c> where 'a: 'b, 'b: 'c {
        let mut touched = false;
        let mut new_conflict = self.conflict;
        self.lits.set(Literal::Top);
        for &lit in clause {
            let mapped_lit = self.subst.map(lit);
            if mapped_lit != lit {
                touched = true;
            }
            self.lits.set(mapped_lit.complement());
            self.stack.push(mapped_lit.complement());
            if self.lits.check(mapped_lit) {
                new_conflict = true;
                break;
            }
        }
        RupCheck {
            stack: &mut self.stack,
            lits: &mut self.lits,
            conflict: new_conflict,
            touched: Some(touched),
        }
    }
    pub fn extract_substitution(&self) -> Vec<(Variable, Literal)> {
        self.subst.into_iter().map(|(var, lit)| (*var, *lit)).collect()
    }
}
impl<'a> Drop for WsrCheck<'a> {
    fn drop(&mut self) {
        for lit in &*self.central {
            self.lits.clear(*lit);
        }
        self.stack.clear();
        self.subst.clear();
    }
}

pub struct WsrWitness<'a> {
    chk: &'a mut WsrChecker,
}
impl<'a> WsrWitness<'a> {
    pub fn map(&mut self, var: Variable, lit: Literal) {
        self.chk.subst.insert(var, lit);
    }
    pub fn close(self, clause: Clause<'_>) -> WsrCheck<'a> {
        let conflict = WsrChecker::load_clause(&mut self.chk.central, &mut self.chk.lits, clause);
        let ptr: *mut WsrChecker = self.chk;
        ManuallyDrop::new(self);
        unsafe { WsrCheck {
            central: &mut (*ptr).central,
            stack: &mut (*ptr).stack,
            lits: &mut (*ptr).lits,
            subst: &mut (*ptr).subst,
            conflict: conflict,
        } }
    }
}
impl<'a> Drop for WsrWitness<'a> {
    fn drop(&mut self) {
        self.chk.subst.clear();
    }
}

pub struct WsrChecker {
    central: Vec<Literal>,
    stack: Vec<Literal>,
    lits: LiteralSet,
    subst: Substitution,
}
impl WsrChecker {
    pub fn new() -> WsrChecker {
        WsrChecker {
            central: Vec::new(),
            stack: Vec::new(),
            lits: LiteralSet::new(),
            subst: Substitution::new(),
        }
    }
    pub fn rup<'a, 'b: 'a>(&'b mut self, clause: Clause<'_>) -> RupCheck<'a> {
        let conflict = WsrChecker::load_clause(&mut self.stack, &mut self.lits, clause);
        RupCheck {
            stack: &mut self.stack,
            lits: &mut self.lits,
            conflict: conflict,
            touched: None,
        }
    }
    pub fn wsr<'a, 'b: 'a>(&'b mut self) -> WsrWitness<'a> {
        WsrWitness::<'a> { chk: self }
    }
    fn load_clause(vec: &mut Vec<Literal>, lits: &mut LiteralSet, clause: Clause<'_>) -> bool {
        let mut conflict = false;
        lits.set(Literal::Top);
        for &lit in clause {
            lits.set(lit.complement());
            vec.push(lit.complement());
            if lits.check(lit) {
                conflict = true;
                break;
            }
        }
        conflict
    }
    fn load_central(&mut self, clause: Clause<'_>) -> WsrCheck {
        let conflict = WsrChecker::load_clause(&mut self.central, &mut self.lits, clause);
        WsrCheck {
            central: &mut self.central,
            stack: &mut self.stack,
            lits: &mut self.lits,
            subst: &mut self.subst,
            conflict: conflict,
        }
    }
}