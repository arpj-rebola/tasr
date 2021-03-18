use std::{
    cmp::{Ordering},
};

use crate::{
    basic::{Literal, Variable},
};

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum SubstitutionInsertion {
    Inserted,
    Repeated,
    Inconsistent,
}

struct SubstNode {
    var: Variable,
    lit: Literal,
    parent: u32,
    left: u32,
    right: u32,
}

pub struct Substitution {
    vec: Vec<SubstNode>,
}
impl Substitution {
    pub fn new() -> Substitution {
        Substitution { vec: Vec::new() }
    }
    pub fn insert(&mut self, var: Variable, lit: Literal) -> SubstitutionInsertion {
        let parent = if !self.vec.is_empty() {
            let child = if self.vec.len() >= u32::max_value() as usize {
                Substitution::capacity_exceeded();
            } else {
                self.vec.len() as u32
            };
            let mut current = 0u32;
            loop {
                let node = unsafe { self.vec.get_unchecked_mut(current as usize) };
                match var.cmp(&node.var) {
                    Ordering::Less => if node.left == u32::max_value() {
                        node.left = child;
                        break;
                    } else {
                        current = node.left;
                    },
                    Ordering::Greater => if node.right == u32::max_value() {
                        node.right = child;
                        break;
                    } else {
                        current = node.right;
                    },
                    Ordering::Equal => return if node.lit == lit {
                        SubstitutionInsertion::Repeated
                    } else {
                        SubstitutionInsertion::Inconsistent
                    },
                }
            }
            current
        } else {
            u32::max_value()
        };
        self.vec.push(SubstNode {
            var: var,
            lit: lit,
            parent: parent,
            left: u32::max_value(),
            right: u32::max_value(),
        });
        SubstitutionInsertion::Inserted
    }
    pub fn clear(&mut self) {
        self.vec.clear()
    }
    pub fn get(&self, var: Variable) -> Option<Literal> {
        if self.vec.is_empty() {
            None
        } else {
            let mut current = 0u32;
            loop {
                let node = unsafe { self.vec.get_unchecked(current as usize) };
                current = match var.cmp(&node.var) {
                    Ordering::Less => node.left,
                    Ordering::Greater => node.right,
                    Ordering::Equal => break Some(node.lit),
                };
                if current == u32::max_value() {
                    break None
                }
            }
        }
    }
    pub fn map(&self, lit: Literal) -> Literal {
        match lit.variable() {
            Some(var) => match self.get(var) {
                Some(l) => if lit.positive() {
                    l
                } else {
                    l.complement()
                },
                None => lit,
            },
            None => lit,
        }
    }
    fn capacity_exceeded() -> ! {
        panick!("maxiumum substitution length exceeded", lock, {
            append!(lock, "Could not process a substitution because it contains more than {} mappings.", u32::max_value() >> 1);
        })
    }
}
impl<'a> IntoIterator for &'a Substitution {
    type Item = (&'a Variable, &'a Literal);
    type IntoIter = SubstitutionIterator<'a>;
    fn into_iter(self) -> SubstitutionIterator<'a> {
        let current = if self.vec.is_empty() {
            u32::max_value()
        } else {
            0u32
        };
        let mut subst = SubstitutionIterator::<'a> {
            subst: &self.vec[..],
            current: current,
        };
        if subst.current != u32::max_value() {
            subst.down_left();
        }
        subst
    }
}

pub struct SubstitutionIterator<'a> {
    subst: &'a [SubstNode],
    current: u32,
}
impl<'a> SubstitutionIterator<'a> {
    fn down_left(&mut self) {
        loop {
            let left = unsafe { self.subst.get_unchecked(self.current as usize).left };
            if left == u32::max_value() {
                break;
            } else {
                self.current = left;
            }
        }
    }
    fn up_right(&mut self) {
        self.current = loop {
            let parent = unsafe { self.subst.get_unchecked(self.current as usize).parent };
            if parent == u32::max_value() {
                break u32::max_value();
            } else if unsafe { self.subst.get_unchecked(parent as usize).right } != self.current {
                break parent;
            } else {
                self.current = parent;
            }
        }
    }
}
impl<'a> Iterator for SubstitutionIterator<'a> {
    type Item = (&'a Variable, &'a Literal);
    fn next(&mut self) -> Option<(&'a Variable, &'a Literal)> {
        if self.current != u32::max_value() {
            let node = unsafe { self.subst.get_unchecked(self.current as usize) };
            if node.right == u32::max_value() {
                self.up_right();
            } else {
                self.current = node.right;
                self.down_left();
            }
            Some((&node.var, &node.lit))
        } else {
            None
        }
    }
}

#[cfg(test)]
pub mod test {
	use rand::{self, Rng};
	use crate::{
        basic::{Literal, Variable, test::{generate_literal, generate_external_literal, generate_variable, generate_external_variable}},
        substitution::{Substitution, SubstitutionInsertion},
    };
    
    pub fn generate_substitution<R: Rng>(rng: &mut R, maxsize: usize) -> Vec<(Variable, Literal)> {
        let size = rng.gen_range(0usize, maxsize + 1usize);
        let mut vec = Vec::<(Variable, Literal)>::new();
        for _ in 0..size {
            let var = generate_variable(rng);
            let lit = generate_literal(rng, None);
            vec.push((var, lit));
        }
        vec.sort_by(|&(var1, _), &(var2, _)| var1.cmp(&var2));
        vec.dedup_by(|&mut (var1, _), &mut (var2, _)| var1 == var2);
        vec
    }
    
    #[test]
    fn test_substitution() {
        let mut rng = rand::thread_rng();
        let mut subst = Substitution::new();
        for _ in 0..1000 {
            let vec = generate_substitution(&mut rng, 300usize);
            let vars: Vec<Variable> = vec.iter().map(|&(var, _)| var).collect();
            for &(var, lit) in &vec {
                assert!(subst.insert(var, lit) == SubstitutionInsertion::Inserted);
            }
            for &(var, lit) in &vec {
                let other = generate_external_literal(&mut rng, &[lit], None);
                assert!(subst.insert(var, lit) == SubstitutionInsertion::Repeated);
                assert!(subst.insert(var, other) == SubstitutionInsertion::Inconsistent);
            }
            let (mut itv, mut its) = (vec.iter(), subst.into_iter());
            loop { match (itv.next(), its.next()) {
                (Some(&(vvar, vlit)), Some((&svar, &slit))) => assert!(vvar == svar && vlit == slit),
                (None, None) => break,
                _ => assert!(false),
            } }
            for &(var, lit) in &vec {
                assert!(subst.get(var) == Some(lit));
                assert!(subst.map(var.positive()) == lit);
                assert!(subst.map(var.negative()) == lit.complement());
            }
            for _ in 0..100 {
                let var = generate_external_variable(&mut rng, &vars);
                assert!(subst.get(var) == None);
                assert!(subst.map(var.positive()) == var.positive());
                assert!(subst.map(var.negative()) == var.negative());
            }
            assert!(subst.map(Literal::Top) == Literal::Top);
            assert!(subst.map(Literal::Bottom) == Literal::Bottom);
            subst.clear();
        }
    }
}
