use std::{
    mem::{self},
};

use crate::{
    basic::{Literal},
};

#[repr(u8)]
pub enum ModelValue {
    Unassigned = 0b00u8,
    True = 0b01u8,
    False = 0b10u8,
}

#[derive(PartialEq, Eq, Debug)]
#[repr(u8)]
pub enum ModelMembership {
    Unassigned = 0b00u8,
    True = 0b01u8,
    False = 0b10u8,
    Tautology = 0b11u8,
}
impl ModelMembership {
    pub fn is_true(&self) -> bool {
        unsafe { mem::transmute::<&ModelMembership, &u8>(self) & 0b01u8 != 0u8 }
    }
    pub fn is_false(&self) -> bool {
        unsafe { mem::transmute::<&ModelMembership, &u8>(self) & 0b10u8 != 0u8 }
    }
    pub fn is_unassigned(&self) -> bool {
        match self {
            ModelMembership::Unassigned => true,
            _ => false,
        }
    }
    pub fn is_tautology(&self) -> bool {
        match self {
            ModelMembership::Tautology => true,
            _ => false,
        }
    }
}
impl From<ModelValue> for ModelMembership {
    fn from(mv: ModelValue) -> ModelMembership {
        unsafe { mem::transmute::<ModelValue, ModelMembership>(mv) }
    }
}

pub struct Model {
    vec: Vec<u8>,
}
impl Model {
    pub fn new() -> Model {
        Model { vec: Vec::new() }
    }
    pub unsafe fn value(&self, lit: Literal) -> ModelValue {
        let index = lit.index();
        match self.vec.get(index >> 3) {
            Some(chunk) => {
                let val = Model::get_value(chunk, index);
                mem::transmute::<u8, ModelValue>(val)
            },
            None => ModelValue::Unassigned,
        }
    }
    pub fn member(&self, lit: Literal) -> ModelMembership {
        let index = lit.index();
        match self.vec.get(index >> 3) {
            Some(chunk) => {
                let val = Model::get_value(chunk, index);
                unsafe { mem::transmute::<u8, ModelMembership>(val) }
            },
            None => ModelMembership::Unassigned,
        }
    }
    pub fn set(&mut self, lit: Literal) {
        let index = lit.index();
        let chunk_index = index >> 3;
        if chunk_index >= self.vec.len() {
            self.vec.resize((chunk_index << 1) + 1usize, 0u8);
        }
        let chunk = unsafe { self.vec.get_unchecked_mut(chunk_index) };
        let shift = index & 0b111usize;
        if *chunk & (0b1u8 << (shift ^ 1usize)) == 0u8 {
            *chunk |= 0b1u8 << shift;
        }
    }
    pub fn check_set(&mut self, lit: Literal) -> ModelValue {
        let index = lit.index();
        let chunk_index = index >> 3;
        if chunk_index >= self.vec.len() {
            self.vec.resize((chunk_index << 1) + 1usize, 0u8);
        }
        let chunk = unsafe { self.vec.get_unchecked_mut(chunk_index) };
        let val = Model::get_value(chunk, index);
        if val == 0u8 {
            *chunk |= 0b1u8 << (index & 0b111usize);
        }
        unsafe { mem::transmute::<u8, ModelValue>(val) }
    }
    pub fn force(&mut self, lit: Literal) {
        let index = lit.index();
        let chunk_index = index >> 3;
        if chunk_index >= self.vec.len() {
            self.vec.resize((chunk_index << 1) + 1usize, 0u8);
        }
        let chunk = unsafe { self.vec.get_unchecked_mut(chunk_index) };
        *chunk |= 0b1u8 << (index & 0b111usize);
    }
    pub fn check_force(&mut self, lit: Literal) -> ModelMembership {
        let index = lit.index();
        let chunk_index = index >> 3;
        if chunk_index >= self.vec.len() {
            self.vec.resize((chunk_index << 1) + 1usize, 0u8);
        }
        let chunk = unsafe { self.vec.get_unchecked_mut(chunk_index) };
        let val = Model::get_value(chunk, index);
        *chunk |= 0b1u8 << (index & 0b111usize);
        unsafe { mem::transmute::<u8, ModelMembership>(val) }
    }
    pub fn clear(&mut self, lit: Literal) {
        let index = lit.index();
        if let Some(chunk) = self.vec.get_mut(index >> 3) {
            let shift = index & 0b110usize;
            *chunk &= !(0b11u8 << shift);
        }
    }
    pub fn check_clear(&mut self, lit: Literal) -> ModelMembership {
        let index = lit.index();
        match self.vec.get_mut(index >> 3) {
            Some(chunk) => {
                let val = Model::get_value(chunk, index);
                let shift = index & 0b110usize;
                *chunk &= !(0b11u8 << shift);
                unsafe { mem::transmute::<u8, ModelMembership>(val) }
            },
            None => ModelMembership::Unassigned,
        }
    }
    fn get_value(chunk: &u8, index: usize) -> u8 {
        let shift: usize = index & 0b110usize;
        let val: u8 = *chunk >> shift;
        if index & 0b1usize == 0usize {
            val & 0b11u8
        } else {
            ((val & 0b01u8) << 1) | ((val & 0b10u8) >> 1)
        }
    }
}

#[cfg(test)]
pub mod test {
    use std::{
        mem::{self},
        convert::{TryFrom},
    };
    use rand::{self, Rng};
	use crate::{
        basic::{Literal, ClauseIndex, test::{generate_literal, generate_external_literal, generate_index}},
        model::{Model, ModelMembership, ModelValue},
    };

    #[test]
    fn test_model() {
        let mut rng = rand::thread_rng();
        let mut model = Model::new();
        let mut vec = Vec::new();
        for _ in 0 .. 50 {
            for _ in 0 .. 1000 {
                let lit = generate_literal(&mut rng, Some(3000u32));
                vec.push(lit);
            }
            for (n, &lit) in vec.iter().enumerate() {
                let comp = lit.complement();
                let pos = vec[0 .. n].iter().any(|&x| x == lit);
                let neg = vec[0 .. n].iter().any(|&x| x == comp);
                let member = model.member(lit);
                assert!(pos == member.is_true());
                assert!(neg == member.is_false());
                assert!((pos && neg) == member.is_tautology());
                assert!((!pos && !neg) == member.is_unassigned());
                if !pos || !neg {
                    assert!(ModelMembership::from(unsafe { model.value(lit) }) == member);
                }
                model.set(lit);
                if !neg {
                    assert!(model.member(lit).is_true());
                } else {
                    assert!(model.member(lit) == member);
                }
                model.force(lit);
                assert!(model.member(lit).is_true());
                assert!(model.member(lit).is_tautology() == neg);
                model.clear(lit);
                assert!(model.member(lit).is_unassigned());
                assert!(model.member(comp).is_unassigned());
                if pos {
                    model.force(lit);
                }
                if neg {
                    model.force(comp);
                }
                assert!(model.member(lit) == member);
                let check_set = model.check_set(lit);
                let check_clear = model.check_clear(lit);
                let check_force = model.check_force(lit);
                let check_force_comp = if neg {
                    model.check_force(comp)
                } else {
                    model.member(comp)
                };
                assert!(ModelMembership::from(check_set) == member);
                if !neg {
                    assert!(check_clear.is_true());
                } else {
                    assert!(check_clear == member);
                }
                assert!(check_force.is_unassigned());
                assert!(check_force_comp.is_false() && !check_force_comp.is_true());
                assert!(model.member(lit).is_true() && (model.member(lit).is_false() == neg));
                assert!(model.member(comp).is_false() && (model.member(comp).is_true() == neg));
            }
            for n in 0u32 .. 3000u32 {
                let lit = unsafe { mem::transmute::<u32, Literal>(n) };
                assert!(model.member(lit).is_true() == vec.iter().any(|&x| x == lit));
            }
            for &lit in &vec {
                model.clear(lit);
            }
            for n in 0u32 .. 3000u32 {
                let lit = unsafe { mem::transmute::<u32, Literal>(n) };
                assert!(model.member(lit).is_unassigned());
            }
            vec.clear();
        }
    }
}