use std::{
    convert::{TryFrom},
    fmt::{Display, Formatter, Result as FmtResult, Debug},
    mem::{self},
};

use crate::{
    basic::{Literal, Variable, ClauseIndex}
};

pub enum DisplayInstruction {
    Core(ClauseIndex, Vec<Literal>),
    Rup(ClauseIndex, Vec<Literal>, Vec<ClauseIndex>),
    Wsr(ClauseIndex, Vec<Literal>, Vec<(Variable, Literal)>, Vec<(ClauseIndex, Option<Vec<ClauseIndex>>)>),
    Del(ClauseIndex),
}
impl Display for DisplayInstruction {
	fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
		match self {
            DisplayInstruction::Core(id, clause) => {
                write!(f, "k {} ", id.text())?;
                for lit in clause {
                    write!(f, "{} ", lit.text())?;
                }
                write!(f, "0\n")?;
            },
            DisplayInstruction::Rup(id, clause, chain) => {
                write!(f, "r {} ", id.text())?;
                for lit in clause {
                    write!(f, "{} ", lit.text())?;
                }
                write!(f, "0 ")?;
                for cid in chain {
                    write!(f, "{} ", cid.text())?;
                }
                write!(f, "0\n")?;
            },
            DisplayInstruction::Wsr(id, clause, witness, mchain) => {
                write!(f, "w {} ", id.text())?;
                for lit in clause {
                    write!(f, "{} ", lit.text())?;
                }
                write!(f, "0 ")?;
                for (var, lit) in witness {
                    write!(f, "{} {} ", var.text(), lit.text())?;
                }
                write!(f, "0 ")?;
                for (lid, subchain) in mchain {
                    if lid != id {
                        write!(f, "{} ", lid.text())?;
                    }
                    if let Some(chain) = subchain {
                        for cid in chain {
                            write!(f, "{} ", cid.text())?;
                        }
                        write!(f, "0 ")?;
                    } else {
                        write!(f, "d ")?;
                    }
                }
                write!(f, "0\n")?;
            },
            DisplayInstruction::Del(id) => {
                write!(f, "d {}\n", id.text())?;
            },
        }
        Ok(())
	}
}

pub struct PigeonHole {
    n: usize,
    orig: usize,
    proof: Vec<DisplayInstruction>,
    somes: Vec<ClauseIndex>,
    ones: Vec<Vec<Vec<Option<ClauseIndex>>>>,
    lemmas: Vec<ClauseIndex>,
    next: u64,
}
impl PigeonHole {
    pub fn new(n: usize) -> PigeonHole {
        let mut vec1 = Vec::<Vec<Vec<Option<ClauseIndex>>>>::new();
        for _ in 0 .. (n + 1) {
            let mut vec2 = Vec::<Vec<Option<ClauseIndex>>>::new();
            for _ in 0 .. (n + 1) {
                let mut vec3 = Vec::<Option<ClauseIndex>>::new();
                for _ in 0 .. n {
                    vec3.push(None);
                }
                vec2.push(vec3);
            }
            vec1.push(vec2);
        }
        let mut ph = PigeonHole {
            n: n,
            orig: n,
            proof: Vec::new(),
            somes: Vec::new(),
            lemmas: Vec::new(),
            ones: vec1,
            next: 0u64,
        };
        for i in 0 .. (ph.n + 1) {
            let cls = ph.some_hole_clause(i);
            let id = ph.next_index();
            ph.somes.push(id);
            ph.proof.push(DisplayInstruction::Core(id, cls));
        }
        for i in 0 .. (ph.n + 1) {
            for j in (i + 1) .. (ph.n + 1) {
                for k in 0 .. ph.n {
                    let cls = ph.one_pigeon_clause(i, j, k);
                    let id = ph.next_index();
                    *unsafe { ph.ones.get_unchecked_mut(i).get_unchecked_mut(j).get_unchecked_mut(k) } = Some(id);
                    *unsafe { ph.ones.get_unchecked_mut(j).get_unchecked_mut(i).get_unchecked_mut(k) } = Some(id);
                    ph.proof.push(DisplayInstruction::Core(id, cls));
                }
            }
        }
        while let Some(()) = ph.reduce() {
        }
        ph
    }
    fn next_index(&mut self) -> ClauseIndex {
        self.next += 1u64;
        unsafe { mem::transmute::<u64, ClauseIndex>(self.next) }
    }
    fn variable(&self, i: usize, j: usize) -> Variable {
        Variable::try_from(((i + j * (self.orig + 1usize)) + 1usize) as i64).unwrap()
    }
    fn literal(&self, i: usize, j: usize) -> Literal {
        Literal::try_from(((i + j * (self.orig + 1usize)) + 1usize) as i64).unwrap()
    }
    fn some_hole_clause(&self, i: usize) -> Vec<Literal> {
        let mut vec = Vec::new();
        for j in 0 .. self.n {
            vec.push(self.literal(i, j));
        }
        vec
    }
    fn one_pigeon_clause(&self, i: usize, j: usize, k: usize) -> Vec<Literal> {
        vec![self.literal(i, k).complement(), self.literal(j, k).complement()]
    }
    fn witness(&self, i: usize) -> Vec<(Variable, Literal)> {
        let mut vec = Vec::new();
        for j in 0 .. self.n {
            vec.push((self.variable(i, j), self.literal(self.n, j)));
            vec.push((self.variable(self.n, j), self.literal(i, j)));
        }
        vec
    }
    fn reduction_chain(&self, i: usize, id: ClauseIndex) -> Vec<(ClauseIndex, Option<Vec<ClauseIndex>>)> {
        let mut vec = Vec::new();
        vec.push((id, Some(vec![unsafe { self.ones.get_unchecked(i).get_unchecked(self.n).get_unchecked(self.n - 1).unwrap() }])));
        for k in 0 .. self.n {
            for j in 0 .. self.n + 1 {
                if j != i && j != self.n {
                    vec.push((unsafe { self.ones.get_unchecked(i).get_unchecked(j).get_unchecked(k).unwrap() },
                        Some(vec![unsafe { self.ones.get_unchecked(self.n).get_unchecked(j).get_unchecked(k).unwrap() }])));
                    vec.push((unsafe { self.ones.get_unchecked(self.n).get_unchecked(j).get_unchecked(k).unwrap() },
                        Some(vec![unsafe { self.ones.get_unchecked(i).get_unchecked(j).get_unchecked(k).unwrap() }])));
                }
            }
            vec.push((unsafe { self.ones.get_unchecked(i).get_unchecked(self.n).get_unchecked(k).unwrap() },
                Some(vec![unsafe { self.ones.get_unchecked(i).get_unchecked(self.n).get_unchecked(k).unwrap() }])));
        }
        vec.push((*unsafe { self.somes.get_unchecked(i) }, Some(vec![*unsafe { self.somes.get_unchecked(self.n) }])));
        // vec.push((*unsafe { self.somes.get_unchecked(self.n) }, Some(vec![*unsafe { self.somes.get_unchecked(i) }])));
        vec
    }
    fn reduction_chain_deleted(&self, i: usize, id: ClauseIndex) -> Vec<(ClauseIndex, Option<Vec<ClauseIndex>>)> {
        let mut vec = Vec::new();
        vec.push((id, Some(vec![unsafe { self.ones.get_unchecked(i).get_unchecked(self.n).get_unchecked(self.n - 1).unwrap() }])));
        for k in 0 .. self.n {
            for j in 0 .. self.n + 1 {
                if j != i && j != self.n {
                    vec.push((unsafe { self.ones.get_unchecked(i).get_unchecked(j).get_unchecked(k).unwrap() },
                        Some(vec![unsafe { self.ones.get_unchecked(self.n).get_unchecked(j).get_unchecked(k).unwrap() }])));
                    vec.push((unsafe { self.ones.get_unchecked(self.n).get_unchecked(j).get_unchecked(k).unwrap() },
                        Some(vec![unsafe { self.ones.get_unchecked(i).get_unchecked(j).get_unchecked(k).unwrap() }])));
                }
            }
            vec.push((unsafe { self.ones.get_unchecked(i).get_unchecked(self.n).get_unchecked(k).unwrap() }, None));
        }
        vec.push((*unsafe { self.somes.get_unchecked(i) }, Some(vec![*unsafe { self.somes.get_unchecked(self.n) }])));
        // vec.push((*unsafe { self.somes.get_unchecked(self.n) }, Some(vec![*unsafe { self.somes.get_unchecked(i) }])));
        vec
    }
    fn reduce(&mut self) -> Option<()> {
        if self.n == 0 {
            None
        } else {
            self.lemmas.clear();
            for i in 0 .. self.n {
                let id = self.next_index();
                let clause = vec![self.literal(i, self.n - 1usize).complement()];
                let witness = self.witness(i);
                let chain = if i != self.n - 1 {
                    self.reduction_chain(i, id)
                } else {
                    self.reduction_chain_deleted(i, id)
                };
                self.proof.push(DisplayInstruction::Wsr(id, clause, witness, chain));
                self.lemmas.push(id);
            }
            self.n -= 1usize;
            for i in 0 .. self.n + 1usize {
                let id = self.next_index();
                let clause = self.some_hole_clause(i);
                let chain = vec![*unsafe { self.lemmas.get_unchecked(i) }, *unsafe { self.somes.get_unchecked(i) }];
                self.proof.push(DisplayInstruction::Rup(id, clause, chain));
                *unsafe { self.somes.get_unchecked_mut(i) } = id;
            }
            if self.n == 0usize {
                let id = self.next_index();
                self.proof.push(DisplayInstruction::Rup(id, vec![], vec![*self.somes.first().unwrap(), *self.lemmas.first().unwrap()]));
            }
            for id in &self.lemmas {
                self.proof.push(DisplayInstruction::Del(*id));
            }
            Some(())
        }
    }
}
impl Display for PigeonHole {
	fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "p core\n")?;
        let mut core = true;
        for elem in &self.proof {
            if core {
                match elem {
                    DisplayInstruction::Core(_, _) => (),
                    _ => {
                        core = false;
                        write!(f, "p proof\n")?;
                    },
                }
            }
            write!(f, "{}", elem)?;
        }
        Ok(())
    }
}
impl Debug for PigeonHole {
	fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
        let mut count = 0u64;
        let mut var = 0usize;
        for elem in &self.proof {
            match elem {
                DisplayInstruction::Core(_, clause) => {
                    count += 1u64;
                    for lit in clause {
                        var = var.max(lit.index());
                    }
                },
                _ => (),
            }
        }
        write!(f, "p cnf {} {}\n", var >> 1, count)?;
        for elem in &self.proof {
            match elem {
                DisplayInstruction::Core(_, clause) => {
                    for lit in clause {
                        write!(f, "{} ", lit.text())?;
                    }
                    write!(f, "0\n")?;
                },
                _ => (),
            }
        }
        Ok(())
    }
}