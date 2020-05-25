pub trait Hashable32 {
	fn hash<H: Hasher32>(&self, state: &mut H);
	fn hash_slice<H: Hasher32>(data: &[Self], state: &mut H) where
		Self: Sized
	{
		for one in data {
			one.hash(state)
		}
	}
}
impl Hashable32 for u32 {
	fn hash<H: Hasher32>(&self, state: &mut H) {
		state.write(self)
	}
}
impl<T: Hashable32 + Sized> Hashable32 for [T] {
	fn hash<H: Hasher32>(&self, state: &mut H) {
		T::hash_slice::<H>(self, state)
	}
}

pub trait Hasher32 {
	fn new() -> Self;
	fn write(&mut self, val: &u32);
	fn finish(&self) -> u32;
}

pub struct AmxHasher {
	add: u32,
	mul: u32,
	xor: u32,
}
impl Hasher32 for AmxHasher {
	fn new() -> AmxHasher {
		AmxHasher { add: 0u32, mul: 1u32, xor: 0u32 }
	}
	fn write(&mut self, val: &u32) {
		self.add = self.add.wrapping_add(*val);
		self.mul = self.mul.wrapping_mul(val | 1u32);
		self.xor ^= val;
	}
	fn finish(&self) -> u32 {
		1023u32.wrapping_mul(self.add).wrapping_add(self.mul ^ self.xor)
	}
}

pub struct UnwindHasher {
	digest: u32,
}
impl Hasher32 for UnwindHasher {
	fn new() -> UnwindHasher {
		UnwindHasher { digest: 0u32 }
	}
	fn write(&mut self, val: &u32) {
		let x: u32 = val ^ (val << 16u32);
		let y: u32 = val ^ (val >> 16u32);
		self.digest = self.digest.wrapping_add(x).wrapping_add(y);
	}
	fn finish(&self) -> u32 {
		self.digest
	}
}

#[cfg(test)]
mod test {
	use rand::{self, Rng,
		seq::{SliceRandom}
	};
	use crate::{
		hasher::{AmxHasher, UnwindHasher, Hashable32, Hasher32},
	};

	fn test_commutative_hasher<H: Hasher32>() {
		let mut rng = rand::thread_rng();
		let len: usize = rng.gen_range(0usize, 100usize);
		let mut vec1 = Vec::<u32>::new();
		for _ in 0usize..len {
			vec1.push(rng.gen())
		}
		let mut vec2 = vec1.clone();
		vec2.shuffle(&mut rng);
		let mut hash1 = H::new();
		vec1.hash(&mut hash1);
		let mut hash2 = H::new();
		vec2.hash(&mut hash2);
		assert!(hash1.finish() == hash2.finish())
	}

	#[test]
	fn test_commutative_hashers() {
		for _ in 0..1000 {
			test_commutative_hasher::<AmxHasher>();
			test_commutative_hasher::<UnwindHasher>();
		}
	}
}