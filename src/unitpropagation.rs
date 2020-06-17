use std::{
    mem::{ManuallyDrop},
};
use crate::{
    assignment::{InsertionTest, BacktrackBlock, SubstitutionStack},
    chaindb::{ChainDb},
    clausedb::{ClauseDb, ClauseIndex, ClauseReference},
    variable::{Literal},
};

#[derive(Copy, Clone, Debug)]
#[repr(transparent)]
pub struct Propagation {
    lit: Literal,
}
impl Propagation {
    pub fn proper(self) -> Option<Literal> {
        if self.lit.proper() {
            Some(self.lit)
        } else {
            None
        }
    }
    pub fn conflict(&self) -> bool {
        self.lit == Literal::Bottom
    }
    pub fn null(&self) -> bool {
        self.lit == Literal::Top
    }
    pub fn propagate<'a>(rf: &ClauseReference<'a>, block: &BacktrackBlock) -> Propagation {
        let mut prop = Propagation { lit: Literal::Bottom };
        for &lit in rf {
			match (block.test(lit), prop.lit) {
                (InsertionTest::Conflict, _) => (),
                (InsertionTest::Alright, Literal::Bottom) => prop.lit = lit,
                _ => prop.lit = Literal::Top,
			}
        }
        prop
    }
}

pub struct PropagationResult {
    state: u64,
}
impl PropagationResult {
    const ConflictMask: u64 = 0b001u64;
    const NullMask: u64 = 0b010u64;
    const MissingMask: u64 = 0b100u64;
    const StepShift: u32 = 3u32;
    const Step: u64 = 1 << PropagationResult::StepShift;
    pub const Stable: PropagationResult = PropagationResult { state: 0u64 };
    #[inline]
    pub fn error(&self) -> bool {
        self.state & (PropagationResult::NullMask | PropagationResult::MissingMask) != 0u64
    }
    #[inline]
    pub fn conflict(&self) -> bool {
        self.state & PropagationResult::ConflictMask != 0u64
    }
    #[inline]
    pub fn null(&self) -> bool {
        self.state & PropagationResult::NullMask != 0u64
    }
    #[inline]
    pub fn missing(&self) -> bool {
        self.state & PropagationResult::MissingMask != 0u64
    }
    #[inline]
    pub fn step_up(&mut self) {
        if !self.error() {
            self.state += PropagationResult::Step;
        }
    }
    #[inline]
    pub fn make_conflict(&mut self) {
        self.state |= PropagationResult::ConflictMask;
    }
    #[inline]
    pub fn make_null(&mut self, permissive: bool) {
        if !permissive {
            self.state |= PropagationResult::NullMask;
        }
    }
    #[inline]
    pub fn make_missing(&mut self) {
        self.state |= PropagationResult::MissingMask;
    }
    #[inline]
    pub fn cutoff(&self) -> usize {
        (self.state >> PropagationResult::StepShift) as usize
    }
}

pub struct UnitPropagator<'a> {
    block: &'a mut BacktrackBlock,
    chain: &'a mut ChainDb,
    db: &'a ClauseDb,
    subst: &'a mut SubstitutionStack,
}
impl<'a> UnitPropagator<'a> {
    pub fn new<'b: 'a, 'c: 'a, 'd: 'a, 's: 'a>(
        block: &'b mut BacktrackBlock,
        chain: &'c mut ChainDb,
        db: &'d ClauseDb,
        subst: &'s mut SubstitutionStack,
    ) -> UnitPropagator<'a> {
        UnitPropagator::<'a> {
            block: block,
            chain: chain,
            db: db,
            subst: subst,
        }
    }
    pub fn propagate_rup(&mut self, id: ClauseIndex, permissive: bool) -> Option<PropagationResult> {
        let mut state = PropagationResult::Stable;
        for lit in &self.db.retrieve(id)? {
            match self.block.set(lit.complement()) {
                InsertionTest::Alright | InsertionTest::Repeated => (),
                InsertionTest::Conflict => state.make_conflict(),
            }
        }
        state = self.propagate_chain(None, permissive, state);
        self.block.clear();
        Some(state)
    }
    pub fn propagate_sr(&mut self, id: ClauseIndex, lat: ClauseIndex, permissive: bool) -> Option<PropagationResult> {
        let mut state = PropagationResult::Stable;
        let idclause = self.db.retrieve(id)?;
        let latclause = self.db.retrieve(lat)?;
        for lit in &idclause {
            match self.block.set(lit.complement()) {
                InsertionTest::Alright | InsertionTest::Repeated => (),
                InsertionTest::Conflict => state.make_conflict(),
            }
        }
        let mut touched = false;
        for lit in &latclause {
            let map = self.subst.map(*lit);
            if map != *lit {
                touched = true;
            }
            match self.block.set(map.complement()) {
                InsertionTest::Alright | InsertionTest::Repeated => (),
                InsertionTest::Conflict => state.make_conflict(),
            }
        }
        if touched && !self.chain.exists(lat) {
            state.make_conflict();
        } else {
            state = self.propagate_chain(None, permissive, state);
            state = self.propagate_chain(Some(lat), permissive, state);
        }
        self.block.clear();
        Some(state)
    }
    fn propagate_chain(&mut self, id: Option<ClauseIndex>, permissive: bool, mut state: PropagationResult) -> PropagationResult {
        match self.chain.retrieve(id) {
            Some(chn) => {
                for cid in chn {
                    let rf = match self.db.retrieve(*cid) {
                        Some(rf) => rf,
                        None => {
                            state.make_missing();
                            continue
                        },
                    };
                    if state.conflict() {
                        state.make_null(permissive);
                        continue
                    }
                    let prop = Propagation::propagate(&rf, &self.block);
                    match prop.proper() {
                        Some(lit) => {
                            self.block.set(lit);
                            state.step_up();
                        },
                        None => {
                            if prop.conflict() {
                                state.make_conflict();
                            } else {
                                state.make_null(permissive);
                            }
                            state.step_up();
                        }
                    }
                }
            },
            None => (),
        }
        state
    }
    pub fn freeze(self) {
        ManuallyDrop::<UnitPropagator<'a>>::new(self);
    }
}
impl<'a> Drop for UnitPropagator<'a> {
    fn drop(&mut self) {
        self.subst.clear();
        self.chain.clear();
    }
}
