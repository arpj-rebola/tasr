use std::{
    mem::{self},
};

use crate::{
    basic::{Literal, ClauseIndex, Variable},
    checkerdb::{Clause, Chain, Witness, Multichain, ChainAddress, WitnessAddress},
    model::{Model},
    substitution::{Substitution, SubstitutionInsertion},
    idflags::{ClauseIndexFlags},
};

pub struct ClauseBuffer {
    vec: Vec<Literal>,
    repetitions: Vec<usize>,
    conflicts: Vec<usize>,
    truth: u8,
}
impl ClauseBuffer {
    pub fn new() -> ClauseBuffer {
        ClauseBuffer {
            vec: Vec::new(),
            repetitions: Vec::new(),
            conflicts: Vec::new(),
            truth: 0u8,
        }
    }
    pub fn push(&mut self, model: &mut Model, lit: Literal) {
        if lit != Literal::Bottom {
            self.truth |= 0b01u8;
        }
        if lit == Literal::Top {
            self.truth |= 0b10u8;
        }
        self.vec.push(lit);
        let mm = model.check_force(lit);
        if !mm.is_unassigned() {
            let pos = self.vec.len() - 1usize;
            if mm.is_true() {
                self.repetitions.push(pos);
            }
            if mm.is_false() {
                self.conflicts.push(pos);
                self.truth |= 0b10u8;
            }
        }
    }
    pub fn is_ok(&self) -> bool {
        self.conflicts.is_empty() && self.repetitions.is_empty()
    }
    pub fn is_absurd(&self) -> bool {
        self.truth == 0u8
    }
    pub fn is_tautology(&self) -> bool {
        self.truth > 1u8
    }
    pub fn extract(&self) -> (Vec<Literal>, Vec<Literal>, Vec<(Literal, Literal)>) {
        (self.vec.clone(), self.extract_repetitions(), self.extract_conflicts())
    }
    pub fn get(&mut self, model: &mut Model) -> Clause<'_> {
        if !self.is_ok() {
            self.reduce_issues(model);
        }
        Clause::from(&self.vec[..])
    }
    pub fn clear(&mut self, model: &mut Model) {
        for &lit in &self.vec {
            model.clear(lit);
        }
        self.vec.clear();
        self.repetitions.clear();
        self.conflicts.clear();
        self.truth = 0u8;
    }
    fn extract_repetitions(&self) -> Vec<Literal> {
        let mut vec: Vec<Literal> = self.repetitions.iter().map(
            |&pos| *unsafe { self.vec.get_unchecked(pos) }).collect();
        vec.sort_by(|x, y| unsafe {
            mem::transmute::<&Literal, &u32>(x).cmp(mem::transmute::<&Literal, &u32>(y)) });
        vec.dedup();
        vec
    }
    fn extract_conflicts(&self) -> Vec<(Literal, Literal)> {
        let mut vec: Vec<(Literal, Literal)> = self.conflicts.iter().map(|&pos| {
            let lit = *unsafe { self.vec.get_unchecked(pos) };
            let cmp = lit.complement();
            if lit.positive() {
                (lit, cmp)
            } else {
                (cmp, lit)
            }
        }).collect();
        vec.sort_by(|(x, _), (y, _)| unsafe {
            mem::transmute::<&Literal, &u32>(x).cmp(mem::transmute::<&Literal, &u32>(y)) });
        vec.dedup();
        vec
    }
    fn reduce_issues(&mut self, model: &mut Model) {
        if !self.conflicts.is_empty() {
            for &lit in &self.vec {
                model.clear(lit);
            }
            self.vec.clear();
            self.vec.push(Literal::Top);
            model.force(Literal::Top);
        } else {
            let mut reps = self.repetitions.iter();
            let mut i = self.vec.as_ptr();
            let mut j = self.vec.as_mut_ptr();
            let end = unsafe { i.add(self.vec.len()) };
            let mut skip = unsafe { self.vec.as_ptr().add(*reps.next().unwrap()) };
            while i != end {
                if i == skip {
                    skip = reps.next().map_or(end, |&n| unsafe { self.vec.as_ptr().add(n) });
                } else {
                    unsafe { *j = *i };
                    j = unsafe { j.add(1usize) };
                }
                i = unsafe { i.add(1usize) };
            }
            unsafe { self.vec.set_len(j.offset_from(self.vec.as_ptr()) as usize); }
        }
        self.repetitions.clear();
        self.conflicts.clear();
    }
}

pub struct UncheckedClauseBuffer {
    vec: Vec<Literal>
}
impl UncheckedClauseBuffer {
    pub fn new() -> UncheckedClauseBuffer {
        UncheckedClauseBuffer { vec: Vec::new() }
    }
    pub fn push(&mut self, lit: Literal) {
        self.vec.push(lit);
    }
    pub fn get(&self) -> Clause<'_> {
        Clause::from(&self.vec[..])
    }
    pub fn extract(&self) -> Vec<Literal> {
        self.vec.clone()
    }
    pub fn clear(&mut self) {
        self.vec.clear()
    }
}

pub struct ChainBuffer {
    vec: Vec<ClauseIndex>,
    missing: Vec<usize>,
    exclude: Option<ClauseIndex>,
}
impl ChainBuffer {
    pub fn new() -> ChainBuffer {
        ChainBuffer {
            vec: Vec::new(),
            missing: Vec::new(),
            exclude: None,
        }
    }
    pub fn exclude(&mut self, id: ClauseIndex) {
        self.exclude = Some(id);
    }
    pub fn push<T>(&mut self, flags: &ClauseIndexFlags<T>, id: ClauseIndex) {
        self.vec.push(id);
        if !flags.contains(id) || self.exclude.map_or(false, |lid| lid == id) {
            self.missing.push(self.vec.len() - 1usize);
        }
    }
    pub fn is_ok(&self) -> bool {
        self.missing.is_empty()
    }
    pub fn extract(&self) -> (Vec<ClauseIndex>, Vec<ClauseIndex>) {
        (self.vec.clone(), self.extract_missing())
    }
    pub fn get(&mut self) -> Chain<'_> {
        if !self.is_ok() {
            self.reduce_issues()
        }
        Chain::from(&self.vec[..])
    }
    pub fn clear(&mut self) {
        self.vec.clear();
        self.missing.clear();
        self.exclude = None;
    }
    fn extract_missing(&self) -> Vec<ClauseIndex> {
        let mut vec: Vec<ClauseIndex> = self.missing.iter().map(
            |&pos| *unsafe { self.vec.get_unchecked(pos) }).collect();
        vec.sort_by(|x, y| unsafe {
            mem::transmute::<&ClauseIndex, &u64>(x).cmp(mem::transmute::<&ClauseIndex, &u64>(y))});
        vec.dedup();
        vec
    }
    fn reduce_issues(&mut self) {
        let mut miss = self.missing.iter();
        let mut i = self.vec.as_ptr();
        let mut j = self.vec.as_mut_ptr();
        let end = unsafe { i.add(self.vec.len()) };
        let mut skip = unsafe { self.vec.as_ptr().add(*miss.next().unwrap()) };
        while i != end {
            if i == skip {
                skip = miss.next().map_or(end, |&n| unsafe { self.vec.as_ptr().add(n) });
            } else {
                unsafe { *j = *i };
                j = unsafe { j.add(1usize) };
            }
            i = unsafe { i.add(1usize) };
        }
        unsafe { self.vec.set_len(j.offset_from(self.vec.as_ptr()) as usize); }
        self.missing.clear();
    }
}

pub struct UncheckedChainBuffer {
    vec: Vec<ClauseIndex>
}
impl UncheckedChainBuffer {
    pub fn new() -> UncheckedChainBuffer {
        UncheckedChainBuffer { vec: Vec::new() }
    }
    pub fn push(&mut self, id: ClauseIndex) {
        self.vec.push(id);
    }
    pub fn get(&self) -> Chain<'_> {
        Chain::from(&self.vec[..])
    }
    pub fn extract(&self) -> Vec<ClauseIndex> {
        self.vec.clone()
    }
    pub fn clear(&mut self) {
        self.vec.clear()
    }
}

pub struct WitnessBuffer {
    vec: Vec<(Variable, Literal)>,
    repetitions: Vec<usize>,
    conflicts: Vec<usize>,
}
impl WitnessBuffer {
    pub fn new() -> WitnessBuffer {
        WitnessBuffer {
            vec: Vec::new(),
            repetitions: Vec::new(),
            conflicts: Vec::new(),
        }
    }
    pub fn push(&mut self, subst: &mut Substitution, var: Variable, lit: Literal) {
        self.vec.push((var, lit));
        match subst.insert(var, lit) {
            SubstitutionInsertion::Inserted => (),
            SubstitutionInsertion::Repeated => self.repetitions.push(self.vec.len() - 1usize),
            SubstitutionInsertion::Inconsistent => self.conflicts.push(self.vec.len() - 1usize),
        }
    }
    pub fn is_ok(&self) -> bool {
        self.conflicts.is_empty() && self.repetitions.is_empty()
    }
    pub fn extract(&self) -> (Vec<(Variable, Literal)>, Vec<(Variable, Literal)>, Vec<(Variable, Vec<Literal>)>) {
        (self.vec.clone(), self.extract_repetitions(), self.extract_conflicts())
    }
    pub fn get(&mut self) -> Witness<'_> {
        if !self.is_ok() {
            self.reduce_issues();
        }
        Witness::from(&self.vec[..])
    }
    pub fn clear(&mut self, subst: &mut Substitution) {
        subst.clear();
        self.vec.clear();
        self.repetitions.clear();
        self.conflicts.clear();
    }
    fn extract_repetitions(&self) -> Vec<(Variable, Literal)> {
        let mut vec: Vec<(Variable, Literal)> = self.repetitions.iter().map(
            |&pos| *unsafe { self.vec.get_unchecked(pos) }).collect();
        vec.sort_by(|(x, _), (y, _)| unsafe {
            mem::transmute::<&Variable, &u32>(x).cmp(mem::transmute::<&Variable, &u32>(y)) });
        vec.dedup();
        vec
    }
    fn extract_conflicts(&self) -> Vec<(Variable, Vec<Literal>)> {
        let mut vec: Vec<(Variable, Vec<Literal>)> = self.conflicts.iter().map(
            |&pos| (unsafe { self.vec.get_unchecked(pos).0 }, Vec::new())).collect();
        vec.sort_by(|(x, _), (y, _)| unsafe {
            mem::transmute::<&Variable, &u32>(x).cmp(mem::transmute::<&Variable, &u32>(y)) });
        vec.dedup_by(|(x, _), (y, _)| x == y);
        for (var, lits) in &mut vec {
            for (v, l) in &self.vec {
                if v == var {
                    lits.push(*l);
                }
            }
        }
        vec
    }
    fn reduce_issues(&mut self) {
        for &pos in &self.conflicts {
            self.repetitions.push(pos);
        }
        let mut reps = self.repetitions.iter();
        let mut i = self.vec.as_ptr();
        let mut j = self.vec.as_mut_ptr();
        let end = unsafe { i.add(self.vec.len()) };
        let mut skip = unsafe { self.vec.as_ptr().add(*reps.next().unwrap()) };
        while i != end {
            if i == skip {
                skip = reps.next().map_or(end, |&n| unsafe { self.vec.as_ptr().add(n) });
            } else {
                unsafe { *j = *i };
                j = unsafe { j.add(1usize) };
            }
            i = unsafe { i.add(1usize) };
        }
        unsafe { self.vec.set_len(j.offset_from(self.vec.as_ptr()) as usize); }
        self.repetitions.clear();
        self.conflicts.clear();
    }
}

pub struct UncheckedWitnessBuffer {
    vec: Vec<(Variable, Literal)>
}
impl UncheckedWitnessBuffer {
    pub fn new() -> UncheckedWitnessBuffer {
        UncheckedWitnessBuffer { vec: Vec::new() }
    }
    pub fn push(&mut self, var: Variable, lit: Literal) {
        self.vec.push((var, lit));
    }
    pub fn extract(&self) -> Vec<(Variable, Literal)> {
        self.vec.clone()
    }
    pub fn get(&self) -> Witness<'_> {
        Witness::from(&self.vec[..])
    }
    pub fn clear(&mut self) {
        self.vec.clear();
    }
}

pub struct MultichainBuffer {
    vec: Vec<(Option<ClauseIndex>, Option<ChainAddress>)>,
    missing: Vec<usize>,
    repetitions: Vec<usize>,
    deletions: Vec<ClauseIndex>,
    discard: Vec<ChainAddress>,
}
impl MultichainBuffer {
    pub fn new() -> MultichainBuffer {
        MultichainBuffer {
            vec: vec![(None, None)],
            missing: Vec::new(),
            repetitions: Vec::new(),
            deletions: Vec::new(),
            discard: Vec::new(),
        }
    }
    pub fn set_witness(&mut self, addr: WitnessAddress) {
        unsafe { *self.vec.get_unchecked_mut(0usize) = (None, Some(mem::transmute::<WitnessAddress, ChainAddress>(addr))); }
    }
    #[inline(always)]
    pub fn push_chain<T>(&mut self, flags: &mut ClauseIndexFlags<T>, lat: ClauseIndex, addr: ChainAddress) {
        self.push_item(flags, lat, Some(addr))
    }
    #[inline(always)]
    pub fn push_dummy_chain<T>(&mut self, flags: &mut ClauseIndexFlags<T>, lat: ClauseIndex) {
        self.push_item(flags, lat, unsafe { Some(mem::transmute::<usize, ChainAddress>(1usize)) });
    }
    #[inline(always)]
    pub fn push_deletion<T>(&mut self, flags: &mut ClauseIndexFlags<T>, lat: ClauseIndex) {
        self.push_item(flags, lat, None)
    }
    pub fn is_ok(&self) -> bool {
        self.missing.is_empty() && self.repetitions.is_empty()
    }
    pub fn extract(&self) -> (Vec<ClauseIndex>, Vec<ClauseIndex>, Vec<ClauseIndex>, Vec<ClauseIndex>) {
        (self.extract_vec(), self.extract_missing(true), self.extract_missing(false), self.extract_repeated())
    }
    pub fn get(&mut self) -> Multichain<'_> {
        if self.is_ok() {
            self.reduce_issues()
        }
        Multichain::from(&self.vec[..])
    }
    pub fn discards(&self) -> &[ChainAddress] {
        &self.discard
    }
    pub fn deletions(&self) -> &[ClauseIndex] {
        &self.deletions
    }
    pub fn clear<T>(&mut self, flags: &mut ClauseIndexFlags<T>) {
        for &(opt_id, _) in &self.vec {
            if let Some(lat) = opt_id {
                if let Some(f) = flags.flags_mut(lat) {
                    f.clear_mchain_spec();
                }
            }
        }
        self.vec.clear();
        self.missing.clear();
        self.repetitions.clear();
        self.deletions.clear();
        self.discard.clear();
        self.vec.push((None, None));
    }
    fn push_item<T>(&mut self, flags: &mut ClauseIndexFlags<T>, lat: ClauseIndex, addr: Option<ChainAddress>) {
        let chain = addr.is_some();
        self.vec.push((Some(lat), addr));
        if let Some(f) = flags.flags_mut(lat) {
            if !f.has_mchain_spec() {
                f.set_mchain_spec();
                if !chain {
                    self.deletions.push(lat);
                }
            } else {
                self.repetitions.push(self.vec.len() - 1usize);
            }
        } else {
            self.missing.push(self.vec.len() - 1usize);
        }
    }
    fn extract_vec(&self) -> Vec<ClauseIndex> {
        self.vec.iter().filter_map(|(id, _)| *id).collect()
    }
    fn extract_missing(&self, chain: bool) -> Vec<ClauseIndex> {
        let mut vec: Vec<ClauseIndex> = self.missing.iter().filter_map(|&pos| {
            let pair = unsafe { self.vec.get_unchecked(pos) };
            if pair.1.is_some() == chain {
                pair.0
            } else {
                None
            }
        }).collect();
        vec.sort_by(|x, y| unsafe {
            mem::transmute::<&ClauseIndex, &u64>(x).cmp(mem::transmute::<&ClauseIndex, &u64>(y))});
        vec.dedup();
        vec
    }
    fn extract_repeated(&self) -> Vec<ClauseIndex> {
        let mut vec: Vec<ClauseIndex> = self.repetitions.iter().map(
            |&pos| unsafe { self.vec.get_unchecked(pos).0.unwrap() }).collect();
        vec.sort_by(|x, y| unsafe {
            mem::transmute::<&ClauseIndex, &u64>(x).cmp(mem::transmute::<&ClauseIndex, &u64>(y)) });
        vec.dedup();
        vec
    }
    fn reduce_issues(&mut self) {
        for &pos in &self.missing {
            self.repetitions.push(pos)
        }
        self.missing.clear();
        let mut reps = self.repetitions.iter();
        let mut i = self.vec.as_ptr();
        let mut j = self.vec.as_mut_ptr();
        let end = unsafe { i.add(self.vec.len()) };
        let mut skip = unsafe { self.vec.as_ptr().add(*reps.next().unwrap()) };
        while i != end {
            if i == skip {
                let (_, opt_addr) = unsafe { *i };
                if let Some(addr) = opt_addr {
                    self.discard.push(addr);
                }
                skip = reps.next().map_or(end, |&n| unsafe { self.vec.as_ptr().add(n) });
            } else {
                unsafe { *j = *i };
                j = unsafe { j.add(1usize) };
            }
            i = unsafe { i.add(1usize) };
        }
        unsafe { self.vec.set_len(j.offset_from(self.vec.as_ptr()) as usize); }
        self.repetitions.clear();
    }
}

pub struct UncheckedMultichainBuffer {
    vec: Vec<(Option<ClauseIndex>, Option<ChainAddress>)>,
}
impl UncheckedMultichainBuffer {
    pub fn new() -> UncheckedMultichainBuffer {
        UncheckedMultichainBuffer { vec: vec![(None, None)] }
    }
    pub fn set_witness(&mut self, addr: WitnessAddress) {
        unsafe { self.vec.get_unchecked_mut(0usize).1 = Some(mem::transmute::<WitnessAddress, ChainAddress>(addr)); }
    }
    pub fn push_chain(&mut self, lat: ClauseIndex, addr: ChainAddress) {
        self.vec.push((Some(lat), Some(addr)));
    }
    pub fn push_deletion(&mut self, lat: ClauseIndex) {
        self.vec.push((Some(lat), None));
    }
    pub fn get(&self) -> Multichain<'_> {
        Multichain::from(&self.vec[..])
    }
    pub fn clear(&mut self) {
        self.vec.clear();
        self.vec.push((None, None));
    }
}