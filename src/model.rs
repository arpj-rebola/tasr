use std::{
    mem::{self},
};

use crate::{
    basic::{Literal},
};

#[repr(u8)]
#[derive(Copy, Clone, Debug)]
pub enum ModelValue {
    Unassigned = 0b000u8,
    True = 0b101u8,
    False = 0b011u8,
}
impl ModelValue {
    #[inline]
    fn polarize(self, lit: Literal) -> ModelValue {
        let val = unsafe { mem::transmute::<ModelValue, u8>(self) };
        unsafe { mem::transmute::<u8, ModelValue>(val ^ ((lit.negative() as u8) * (val & 1u8) * 0b110 )) }
    }
    #[inline]
    fn assign(&mut self, mv: ModelValue) {
        let this = unsafe { mem::transmute::<&mut ModelValue, &mut u8>(self) };
        let that = unsafe { mem::transmute::<ModelValue, u8>(mv) };
        *this |= ((*this ^ that) & 1u8) * that;
    }
}

pub struct Model {
    vec: Vec<ModelValue>
}
impl Model {
    pub fn new() -> Model {
        let mut vec = Vec::with_capacity(1usize << 16);
        vec.resize((1usize << 16) - 1usize, ModelValue::Unassigned);
        Model { vec: vec }
    }
    pub fn value(&self, lit: Literal) -> ModelValue {
        let index = lit.index() >> 1;
        if let Some(val) = self.vec.get(index) {
            val.polarize(lit)
        } else {
            ModelValue::Unassigned
        }
    }
    pub fn set(&mut self, lit: Literal) {
        let index = lit.index() >> 1;
        if index >= self.vec.len() {
            self.vec.resize((index << 1) + 1usize, ModelValue::Unassigned);
        }
        let rf = unsafe { self.vec.get_unchecked_mut(index) };
        let new = ModelValue::True.polarize(lit);
        rf.assign(new);
    }
    pub fn check_set(&mut self, lit: Literal) -> ModelValue {
        let index = lit.index() >> 1;
        if index >= self.vec.len() {
            self.vec.resize((index << 1) + 1usize, ModelValue::Unassigned);
        }
        let rf = unsafe { self.vec.get_unchecked_mut(index) };
        let old = *rf;
        let new = ModelValue::True.polarize(lit);
        rf.assign(new);
        old.polarize(lit)
    }
    pub fn clear(&mut self, lit: Literal) {
        let index = lit.index() >> 1;
        if let Some(val) = self.vec.get_mut(index) {
            *val = ModelValue::Unassigned;
        }
    }
}

#[cfg(test)]
pub mod test {
    use rand::{self};
	use crate::{
        basic::{test::{generate_literal}},
        model::{Model, ModelValue},
    };

    #[test]
    fn test_model() {
        let mut rng = rand::thread_rng();
        let mut vec = Vec::new();
        let mut model = Model::new();
        for _ in 0 .. 50 {
            for _ in 0 .. 1000 {
                let lit = generate_literal(&mut rng, Some(3000u32));
                let comp = lit.complement();
                let pos = vec.iter().any(|&x| x == lit);
                let neg = vec.iter().any(|&x| x == comp);
                match model.value(lit) {
                    ModelValue::Unassigned => assert!(!pos && !neg),
                    ModelValue::True => assert!(pos && !neg),
                    ModelValue::False => assert!(!pos && neg),
                }
                match model.value(comp) {
                    ModelValue::Unassigned => assert!(!pos && !neg),
                    ModelValue::True => assert!(neg && !pos),
                    ModelValue::False => assert!(!neg && pos),
                }
                match model.check_set(lit) {
                    ModelValue::Unassigned => assert!(!pos && !neg),
                    ModelValue::True => assert!(pos && !neg),
                    ModelValue::False => assert!(!pos && neg),
                }
                if !pos && !neg {
                    if let ModelValue::True = model.value(lit) { } else {
                        assert!(false);
                    }
                    if let ModelValue::False = model.value(comp) { } else {
                        assert!(false);
                    }
                } else {
                    match model.value(lit) {
                        ModelValue::Unassigned => assert!(!pos && !neg),
                        ModelValue::True => assert!(pos && !neg),
                        ModelValue::False => assert!(!pos && neg),
                    }
                }
                model.clear(lit);
                if let ModelValue::Unassigned = model.value(lit) { } else {
                    assert!(false);
                }
                if let ModelValue::Unassigned = model.value(comp) { } else {
                    assert!(false);
                }
                if neg {
                    model.set(comp);
                } else {
                    model.set(lit);
                }
                match model.value(lit) {
                    ModelValue::Unassigned => assert!(false),
                    ModelValue::True => assert!(!neg),
                    ModelValue::False => assert!(neg),
                }
                match model.value(comp) {
                    ModelValue::Unassigned => assert!(false),
                    ModelValue::True => assert!(neg),
                    ModelValue::False => assert!(!neg),
                }
                if !neg && !pos {
                    vec.push(lit);
                }
            }
            for &lit in &vec {
                model.clear(lit);
            }
            vec.clear();
        }
        
    }
}