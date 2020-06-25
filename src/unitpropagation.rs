use crate::{
    assignment::{InsertionTest, BacktrackBlock, Substitution},
    chaindb::{ChainDb},
    clausedb::{ClauseDb, ClauseIndex, ClauseReference, WitnessContainer},
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

pub struct PropagationError {
    pub result: PropagationResult,
    pub chain: Vec<ClauseIndex>,
    pub subst: WitnessContainer,
    pub lateral: ClauseIndex,
    pub length: usize,
}
impl PropagationError {
    fn new(result: PropagationResult, chain: &ChainDb, subst: &Substitution, lateral: ClauseIndex, length: usize) -> Option<PropagationError> {
        Some(PropagationError {
            result: result,
            chain: chain.extract(lateral)?,
            subst: subst.extract(),
            lateral: lateral,
            length: length,
        })
    }
}

pub enum PropagationResult {
    Conflict,
    Stable,
    Null,
    Missing,
}
impl PropagationResult {
    #[inline]
    fn is_conflict(&self) -> bool {
        match self {
            PropagationResult::Conflict => true,
            _ => false,
        }
    }
}

pub struct UnitPropagator<'a> {
    block: &'a mut BacktrackBlock,
    chain: &'a mut ChainDb,
    db: &'a ClauseDb,
    subst: &'a mut Substitution,
    central: ClauseIndex,
    conflict: bool,
}
impl<'a> UnitPropagator<'a> {
    pub fn new<'b: 'a, 'c: 'a, 'd: 'a, 's: 'a>(
        block: &'b mut BacktrackBlock,
        chain: &'c mut ChainDb,
        db: &'d ClauseDb,
        subst: &'s mut Substitution,
        id: ClauseIndex,
    ) -> Option<UnitPropagator<'a>> {
        let mut up = UnitPropagator::<'a> {
            block: block,
            chain: chain,
            db: db,
            subst: subst,
            central: id,
            conflict: false,
        };
        up.load_central()?;
        Some(up)
    }
    pub fn propagate_rup(mut self, permissive: bool) -> Result<(), Box<PropagationError>> {
        let (result, length) = self.propagate_chain(self.central, permissive);
        if result.is_conflict() {
            Ok(())
        } else {
            Err(Box::new(PropagationError::new(result, &self.chain, &self.subst, self.central, length).unwrap()))
        }
    }
    pub fn propagate_wsr(mut self, permissive: bool) -> Result<(), Box<PropagationError>> {
        let level = self.block.level();
        let conflict = self.conflict;
        for cid in self.db {
            let touched = self.load_lateral(cid).unwrap();
            let (result, length) = self.propagate_chain(cid, permissive);
            let postresult = if !touched && length == 0usize {
                PropagationResult::Conflict
            } else {
                result
            };
            if !postresult.is_conflict() {
                return Err(Box::new(PropagationError::new(postresult, &self.chain, &self.subst, cid, length).unwrap()))
            }
            self.block.backtrack(level);
            self.conflict = conflict;
        }
        Ok(())
    }
    fn load_central(&mut self) -> Option<()> {
        let rf = self.db.retrieve(self.central)?;
        for lit in &rf {
            match self.block.set(lit.complement()) {
                InsertionTest::Alright | InsertionTest::Repeated => (),
                InsertionTest::Conflict => self.conflict = true,
            }
        }
        Some(())
    }
    fn load_lateral(&mut self, lateral: ClauseIndex) -> Option<bool> {
        let rf = self.db.retrieve(lateral)?;
        let mut touched = false;
        for lit in &rf {
            let map = self.subst.map(*lit);
            touched |= &map != lit;
            match self.block.set(map.complement()) {
                InsertionTest::Alright | InsertionTest::Repeated => (),
                InsertionTest::Conflict => self.conflict = true,
            }
        }
        Some(touched)
    }
    fn propagate_chain(&mut self, chain: ClauseIndex, permissive: bool) -> (PropagationResult, usize) {
        let mut n: usize = 0usize;
        let mut conflict = self.conflict;
        match self.chain.retrieve(chain) {
            Some(chn) => for cid in chn {
                let rf = self.db.retrieve(*cid);
                if cid == &self.central || rf.is_none() {
                    return (PropagationResult::Missing, n)
                }
                if conflict && !permissive {
                    return (PropagationResult::Null, n)
                }
                let prop = Propagation::propagate(&rf.unwrap(), &self.block);
                match prop.proper() {
                    Some(lit) => {
                        self.block.set(lit);
                    },
                    None => if prop.conflict() {
                        conflict = true;
                    } else {
                        return (PropagationResult::Null, n)
                    },
                }
                n += 1usize;
            },
            None => (),
        }
        if conflict {
            (PropagationResult::Conflict, n)
        } else {
            (PropagationResult::Stable, n)
        }
    }
}
impl<'a> Drop for UnitPropagator<'a> {
    fn drop(&mut self) {
        self.block.clear();
        self.subst.clear();
        self.chain.clear();
    }
}
