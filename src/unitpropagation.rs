use crate::{
    assignment::{InsertionTest, BacktrackBlock, Substitution},
    chaindb::{ChainDb},
    clausedb::{ClauseDb},
    basic::{ClauseIndex, WitnessContainer},
    variable::{Literal},
    results::{PropagationIssuesBuilder, PropagationIssues},
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
    pub fn propagate<'a, R: IntoIterator<Item = &'a Literal>>(rf: R, block: &BacktrackBlock) -> Propagation {
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

#[derive(Debug)]
pub enum PropagationResult {
    Stable,
    Null(usize),
    Missing(usize),
    Done(usize),
}
impl PropagationResult {
    pub fn index(&self) -> Option<usize> {
        match self {
            PropagationResult::Stable => None,
            PropagationResult::Null(index) => Some(*index),
            PropagationResult::Missing(index) => Some(*index),
            PropagationResult::Done(index) => Some(*index),
        }
    }
}

pub struct UnitPropagator<'a> {
    block: &'a mut BacktrackBlock,
    db: &'a ClauseDb,
    subst: &'a mut Substitution,
    chain: &'a mut ChainDb,
    issues: PropagationIssuesBuilder,
}
impl<'a> UnitPropagator<'a> {
    pub fn new<'b: 'a, 'c: 'a, 'd: 'a, 's: 'a>(
        block: &'b mut BacktrackBlock,
        chain: &'c mut ChainDb,
        db: &'d ClauseDb,
        subst: &'s mut Substitution,
    ) -> UnitPropagator<'a> {
        UnitPropagator::<'a> {
            block: block,
            db: db,
            subst: subst,
            chain: chain,
            issues: PropagationIssuesBuilder::new(),
        }
    }
    pub fn propagate_inference(&mut self, id: ClauseIndex, wsr: bool) -> Option<()> {
        let result = if wsr {
            self.propagate_wsr(id)
        } else {
            self.propagate_rup(id)
        };
        self.block.clear();
        result
    }
    fn propagate_rup(&mut self, id: ClauseIndex) -> Option<()> {
        let conflict = self.load_central(id)?;
        let prop = self.propagate_chain(id, id, conflict);
        if prop.is_none() && !conflict {
            self.issues.push_issue(PropagationResult::Stable);
        }
        self.issues.push_chain(id);
        Some(())
    }
    pub fn propagate_wsr(&mut self, id: ClauseIndex) -> Option<()> {
        let conflict = self.load_central(id)?;
        let level = self.block.level();
        for lat in self.db.iter() {
            let (latconflict, touched) = self.load_lateral(lat, conflict)?;
            let prop = self.propagate_chain(id, lat, latconflict);
            if prop.is_none() && touched && !latconflict {
                self.issues.push_issue(PropagationResult::Stable);
            }
            self.issues.push_chain(lat);
            self.block.backtrack(level);
        }
        Some(())
    }
    pub fn extract(mut self) -> Option<Box<Vec<(ClauseIndex, PropagationIssues, WitnessContainer)>>> {
        self.issues.extract(&self.db, &self.chain, &self.subst)
    }
    fn load_central(&mut self, id: ClauseIndex) -> Option<bool> {
        let rf = self.db.retrieve(id)?;
        let mut conflict = false;
        for lit in &rf {
            match self.block.set(lit.complement()) {
                InsertionTest::Alright | InsertionTest::Repeated => (),
                InsertionTest::Conflict => conflict = true,
            }
        }
        Some(conflict)
    }
    fn load_lateral(&mut self, lateral: ClauseIndex, mut conflict: bool) -> Option<(bool, bool)> {
        let rf = self.db.retrieve(lateral)?;
        let mut touched = false;
        if !conflict {
            for lit in &rf {
                let map = self.subst.map(*lit);
                touched |= &map != lit;
                match self.block.set(map.complement()) {
                    InsertionTest::Alright | InsertionTest::Repeated => (),
                    InsertionTest::Conflict => {
                        conflict = true;
                        break;
                    }
                }
            }
        }
        Some((conflict, touched))
    }
    fn propagate_chain(&mut self, central: ClauseIndex, lat: ClauseIndex, mut conflict: bool) -> Option<()> {
        let chain = self.chain.retrieve(lat)?;
        if chain.len() == 0usize {
            return None;
        }
        for (n, cid) in chain.iter().enumerate() {
            if cid == &central {
                self.issues.push_issue(PropagationResult::Missing(n));
            } else {
                match self.db.retrieve(*cid) {
                    Some(rf) => {
                        if conflict {
                            self.issues.push_issue(PropagationResult::Done(n));
                        } else {
                            let prop = Propagation::propagate(&rf, &self.block);
                            match prop.proper() {
                                Some(lit) => {
                                    self.block.set(lit);
                                },
                                None if prop.conflict() => conflict = true,
                                None => self.issues.push_issue(PropagationResult::Null(n)),
                            }
                        }
                    },
                    None => self.issues.push_issue(PropagationResult::Missing(n)),
                }
            }
        }
        if !conflict {
            self.issues.push_issue(PropagationResult::Stable)
        }
        Some(())
    }
}
impl<'a> Drop for UnitPropagator<'a> {
    fn drop(&mut self) {
        self.subst.clear();
        self.chain.clear();
    }
}

#[cfg(test)]
mod test {
    use std::{
        convert::{TryFrom},
    };
	use rand::{self, Rng};
	use crate::{
        assignment::{BacktrackBlock, InsertionTest},
        unitpropagation::{Propagation},
        variable::{Variable, Literal},
    };

    #[test]
    fn test_propagation() {
        let mut rng = rand::thread_rng();
        let minvar = Variable::try_from(1i64).unwrap();
        let maxvar = Variable::try_from(100i64).unwrap();
        let mut block = BacktrackBlock::new();
        for _ in 0usize..100usize {
            let length = rng.gen_range(0usize, 12usize);
            let mut clause = Vec::<Literal>::new();
            for _ in 0usize..length {
                let lit = Literal::random(&mut rng, Some(minvar), maxvar);
                if let InsertionTest::Alright = block.set(lit) {
                    clause.push(lit);
                }
            }
            block.clear();
            for _ in 0usize..100usize {
                for _ in 0usize..50usize {
                    let lit = Literal::random(&mut rng, Some(minvar), maxvar);
                    block.set(lit);
                }
                let prop = Propagation::propagate(&clause, &block);
                let mut count = 0usize;
                let mut taut = false;
                let mut last = Literal::Bottom;
                for lit in &clause {
                    if block.check(*lit) {
                        taut = true;
                    } else if !block.check(lit.complement()) {
                        last = *lit;
                        count += 1usize;
                    }
                }
                if taut || count > 1usize {
                    assert!(prop.null());
                } else if count == 1usize {
                    assert!(last == prop.proper().unwrap())
                } else {
                    assert!(prop.conflict())
                }
                block.clear();
            }
        }
    }
}
