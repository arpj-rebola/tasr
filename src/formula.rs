use crate::{
    basic::{ClauseIndex, MaybeClauseIndex, InstructionNumber},
    clause::{ClauseAddress},
};

pub struct Formula {
    vec: Vec<Option<(InstructionNumber, ClauseAddress)>>,
    count: usize,
}
impl Formula {
    pub fn new() -> Formula {
        Formula {
            vec: Vec::new(),
            count: 0usize,
        }
    }
    pub fn insert(&mut self, nid: ClauseIndex, num: InstructionNumber, addr: ClauseAddress) -> Option<()> {
        let index = nid.index();
        if index >= self.vec.len() {
            self.vec.resize((index << 1usize) + 1usize, None);
        }
        let slot = unsafe { self.vec.get_unchecked_mut(index) };
        if slot.is_none() {
            *slot = Some((num, addr));
            self.count += 1usize;
            Some(())
        } else {
            None
        }
    }
    pub fn remove(&mut self, nid: ClauseIndex) -> Option<(InstructionNumber, ClauseAddress)> {
        let slot = self.vec.get_mut(nid.index())?;
        if slot.is_none() {
            None
        } else {
            let pair = slot.take();
            self.count -= 1usize;
            pair
        }
    }
    pub fn get(&self, nid: ClauseIndex) -> Option<&(InstructionNumber, ClauseAddress)> {
        self.vec.get(nid.index())?.as_ref()
    }
    pub fn size(&mut self) -> usize {
        self.count
    }
}
impl<'a> IntoIterator for &'a Formula {
    type Item = (ClauseIndex, InstructionNumber, ClauseAddress);
    type IntoIter = FormulaIterator<'a>;
    fn into_iter(self) -> FormulaIterator<'a> {
        let count = self.count;
        FormulaIterator::<'a> {
            formula: self,
            nid: MaybeClauseIndex::new(None),
            count: count,
        }
    }
}

pub struct FormulaIterator<'a> {
    formula: &'a Formula,
    nid: MaybeClauseIndex,
    count: usize,
}
impl<'a> Iterator for FormulaIterator<'a> {
    type Item = (ClauseIndex, InstructionNumber, ClauseAddress);
    fn next(&mut self) -> Option<(ClauseIndex, InstructionNumber, ClauseAddress)> {
        if self.count == 0usize {
            None
        } else { loop {
            self.nid = unsafe { self.nid.succ() };
            let id = self.nid.get().unwrap();
            if let Some(&(num, addr)) = unsafe { self.formula.vec.get_unchecked(id.index()).as_ref() } {
                self.count -= 1usize;
                break Some((id, num, addr))
            }
        } }
    }
}