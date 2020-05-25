use std::{
	error::{Error},
	fmt::{self, Formatter, Display, Binary, Debug},
};

use crate::{
	input::{FilePosition, InputStream, Positionable, InputError},
};

pub struct HeaderTooLong {
	pos: FilePosition,
	header: Vec<u8>,
}

pub struct OutOfRangeInteger {
	pos: FilePosition,
	number: Vec<u8>,
}

pub struct SyntaxError {
	pos: FilePosition,
	why: String,
}

pub enum LexingError {
	InputError(Box<InputError>),
	HeaderTooLong(Box<HeaderTooLong>),
	OutOfRangeInteger(Box<OutOfRangeInteger>),
	SyntaxError(Box<SyntaxError>),
}
impl LexingError {
	fn out_of_range_integer<P: Positionable>(input: &P, num: &Vec<u8>) -> LexingError {
		LexingError::OutOfRangeInteger(Box::<OutOfRangeInteger>::new(OutOfRangeInteger {
			pos: input.position().clone(),
			number: num.to_vec(),
		}))
	}
	fn header_too_long<P: Positionable>(input: &P, hd: &Vec<u8>) -> LexingError {
		LexingError::HeaderTooLong(Box::<HeaderTooLong>::new(HeaderTooLong {
			pos: input.position().clone(),
			header: hd.to_vec(),
		}))
	}
	fn syntax_error<P: Positionable>(input: &P, why: String) -> LexingError {
		LexingError::SyntaxError(Box::<SyntaxError>::new(SyntaxError {
			pos: input.position().clone(),
			why: why,
		}))
	}
	fn is_header_too_long(&self, vec: &Vec<u8>) -> bool {
		match self {
			LexingError::HeaderTooLong(bx) => &bx.header == vec,
			_ => false,
		}
	}
	fn is_out_of_range_integer(&self, vec: &Vec<u8>) -> bool {
		match self {
			LexingError::OutOfRangeInteger(bx) => &bx.number == vec,
			_ => false,
		}
	}
	fn is_syntax_error(&self, why: &str) -> bool {
		match self {
			LexingError::SyntaxError(bx) => &bx.why == why,
			_ => false,
		}
	}
}
impl From<InputError> for LexingError {
	fn from(err: InputError) -> LexingError {
		LexingError::InputError(Box::<InputError>::new(err))
	}
}
impl Debug for LexingError {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "{}", self)
	}
}
impl Display for LexingError {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			LexingError::InputError(bx) => write!(f, "{}", bx),
			LexingError::HeaderTooLong(bx) => {
				write!(f, "Found header too long in file {}:\n\"", &bx.pos)?;
				for &c in bx.header.iter() {
					write!(f, "{}", c as char)?
				}
				write!(f, "\"")
			},
			LexingError::OutOfRangeInteger(bx) => {
				write!(f, "Found number too large in file {}:\n\"", &bx.pos)?;
				for &c in bx.number.iter() {
					write!(f, "{}", c as char)?
				}
				write!(f, "\"")
			}
			LexingError::SyntaxError(bx) => write!(f, "Syntax error in file {}:\n{}", &bx.pos, &bx.why),
		}
	}
}
impl Binary for LexingError {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			LexingError::InputError(bx) => write!(f, "{:b}", &**bx),
			LexingError::HeaderTooLong(bx) => {
				write!(f, "Found header too long in file {:b}:\n[", &bx.pos)?;
				for &c in bx.header.iter() {
					write!(f, "{}", c as char)?
				}
				write!(f, "]")
			},
			LexingError::OutOfRangeInteger(bx) => {
				write!(f, "Found number too large in file {:b}:\n\"", &bx.pos)?;
				for &c in bx.number.iter() {
					write!(f, "{}", c as char)?
				}
				write!(f, "\"")
			}
			LexingError::SyntaxError(bx) => write!(f, "Syntax error in file {:b}:\n{}", &bx.pos, &bx.why),
		}
	}
}
impl Error for LexingError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
		match self {
			LexingError::InputError(bx) => Some(bx),
			_ => None,
		}
    }
}

type LexingResult<T> = Result<T, LexingError>;

#[derive(Debug, PartialEq, Eq)]
pub enum DimacsLexeme {
	Header([u8; 8]),
	Number(i64),
	Letter(u8),
}
impl DimacsLexeme {
	pub const CnfHeader: DimacsLexeme = DimacsLexeme::Header([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]);
	pub const CoreHeader: DimacsLexeme = DimacsLexeme::Header([b'a', b's', b'r', b'c', b'o', b'r', b'e', 0u8]);
	pub const ProofHeader: DimacsLexeme = DimacsLexeme::Header([b'a', b's', b'r', b'p', b'r', b'o', b'o', b'f']);
}
impl Display for DimacsLexeme {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
			DimacsLexeme::Header(arr) => {
				write!(f, "[")?;
				for &x in arr {
					if x != 0u8 {
						write!(f, "{}", x as char)?;
					} else {
						break;
					}
				}
				write!(f, "]")
			}
			DimacsLexeme::Number(x) => write!(f, "{}", x),
			DimacsLexeme::Letter(c) => write!(f, "{}", *c as char),
		}
	}
}

#[derive(Debug)]
pub enum DimacsDiscriminant {
	Header,
	Number,
	Letter,
}

pub struct DimacsLexer {
	input: InputStream
}
impl DimacsLexer {
	pub fn new(is: InputStream) -> DimacsLexer {
		DimacsLexer { input: is }
	}
	pub fn next(&mut self) -> LexingResult<Option<DimacsLexeme>> {
		self.skip_whitespace()?;
		match self.input.peek_err::<LexingError>()? {
			Some(c) if (*c as char).is_numeric() => self.read_number(1i64),
			Some(b'-') => self.read_number(-1i64),
			Some(b'p') => self.read_header(),
			Some(c) if (*c as char).is_alphabetic() => self.read_letter(),
			Some(c) => {
				let cc = *c;
				self.error_unrecognized_character(cc)?
			},
			None => Ok(None),
		}
	}
	#[inline]
	pub fn next_err<E>(&mut self) -> Result<Option<DimacsLexeme>, E> where
		E: Error + From<LexingError>
	{
		self.next().map_err(E::from)
	}
	pub fn peek(&mut self) -> LexingResult<Option<DimacsDiscriminant>> {
		self.skip_whitespace()?;
		match self.input.peek_err::<LexingError>()? {
			Some(c) if (*c as char).is_numeric() || c == &b'-' => Ok(Some(DimacsDiscriminant::Number)),
			Some(c) if (*c as char).is_alphabetic() && c != &b'p' => Ok(Some(DimacsDiscriminant::Letter)),
			Some(b'p') => Ok(Some(DimacsDiscriminant::Header)),
			Some(c) => {
				let cc = *c;
				self.error_unrecognized_character(cc)?
			},
			None => Ok(None),
		}
	}
	#[inline]
	pub fn peek_err<E>(&mut self) -> Result<Option<DimacsDiscriminant>, E> where
		E: Error + From<LexingError>
	{
		self.peek().map_err(E::from)
	}
	fn skip_whitespace(&mut self) -> LexingResult<()> {
		loop { match self.input.peek_err::<LexingError>()? {
			Some(c) if (*c as char).is_whitespace() => {
				self.input.next()?;
			},
			Some(b'c') => loop { match self.input.next_err::<LexingError>()? {
				None | Some(b'\n') => break,
				Some(_) => (),
			} },
			_ => break Ok(()),
		} }
	}
	fn read_number(&mut self, sign: i64) -> LexingResult<Option<DimacsLexeme>> {
		if sign < 0i64 {
			self.input.next()?;
		}
		match self.input.peek_err::<LexingError>()? {
			Some(c) if (*c as char).is_numeric() => (),
			_ => self.error_not_a_number()?,
		}
		let mut num: i64 = 0i64;
		let last: u8 = loop { match self.input.next_err::<LexingError>()? {
			Some(c) if (c as char).is_numeric() => {
				let digit: i64 = sign * ((c - b'0') as i64);
				match num.checked_mul(10i64).and_then(|x| x.checked_add(digit)) {
					Some(x) => num = x,
					None => self.error_out_of_range(num, c)?,
				}
			},
			Some(c) => break c,
			None => break b' ',
		} };
		if !(last as char).is_whitespace() {
			self.error_not_a_number()?;
		}
		Ok(Some(DimacsLexeme::Number(num)))
	}
	fn read_header(&mut self) -> LexingResult<Option<DimacsLexeme>> {
		self.input.next()?;
		let last = loop { match self.input.next_err::<LexingError>()? {
			Some(c) if (c as char).is_whitespace() => (),
			Some(c) => break c,
			None => self.error_not_a_header()?,
		} };
		let mut vec = Vec::<u8>::new();
		vec.push(last);
		loop { match self.input.next_err::<LexingError>()? {
			Some(c) if (c as char).is_alphabetic() => vec.push(c),
			Some(c) if (c as char).is_whitespace() => break,
			None => break,
			Some(_) => self.error_not_a_header()?,
		} }
		if vec.len() >= 8usize {
			self.error_header_too_long(&vec)?
		}
		let mut arr = [0u8; 8];
		for (n, c) in vec.iter().enumerate() {
			arr[n] = *c;
		}
		Ok(Some(DimacsLexeme::Header(arr)))
	}
	fn read_letter(&mut self) -> LexingResult<Option<DimacsLexeme>> {
		let cc = match self.input.next_err::<LexingError>()? {
			Some(c) if (c as char).is_alphabetic() => c,
			_ => self.error_not_a_letter()?,
		};
		match self.input.next_err::<LexingError>()? {
			Some(c) if !(c as char).is_whitespace() => self.error_not_a_letter()?,
			_ => Ok(Some(DimacsLexeme::Letter(cc))),
		}
	}
	fn error_unrecognized_character<T>(&self, c: u8) -> LexingResult<T> {
		Err(LexingError::syntax_error(&self.input, format!("Invalid character '{}'.", c)))
	}
	fn error_not_a_letter<T>(&mut self) -> LexingResult<T> {
		Err(LexingError::syntax_error(self, format!("Could not parse letter.")))
	}
	fn error_not_a_number<T>(&mut self) -> LexingResult<T> {
		Err(LexingError::syntax_error(self, format!("Could not parse number.")))
	}
	fn error_not_a_header<T>(&mut self) -> LexingResult<T> {
		Err(LexingError::syntax_error(self, format!("Could not parse header.")))
	}
	fn error_header_too_long<T>(&mut self, vec: &Vec<u8>) -> LexingResult<T> {
		Err(LexingError::header_too_long(self, &vec))
	}
	fn error_out_of_range<T>(&mut self, num: i64, c: u8) -> LexingResult<T> {
		let mut vec = format!("{}", num).chars().map(|c| c as u8).collect::<Vec<u8>>();
		vec.push(c);
		while let Ok(Some(cc)) = self.input.next() {
			if (c as char).is_numeric() {
				vec.push(cc)
			}
		}
		Err(LexingError::out_of_range_integer(self, &vec))
	}
}
impl Positionable for DimacsLexer {
	fn position(&self) -> &FilePosition {
		self.input.position()
	}
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum VbeLexeme {
	Number(u64),
	Header([u8; 8]),
}
impl VbeLexeme {
	pub const PlusZero: VbeLexeme = VbeLexeme::Number(0u64);
	pub const MinusZero: VbeLexeme = VbeLexeme::Number(1u64);
	pub const CharacterK: VbeLexeme = VbeLexeme::Number(b'k' as u64);
	pub const CharacterR: VbeLexeme = VbeLexeme::Number(b'r' as u64);
	pub const CharacterS: VbeLexeme = VbeLexeme::Number(b's' as u64);
	pub const CharacterX: VbeLexeme = VbeLexeme::Number(b'x' as u64);
	pub const CharacterD: VbeLexeme = VbeLexeme::Number(b'd' as u64);
	pub const CnfHeader: VbeLexeme = VbeLexeme::Header([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]);
	pub const CoreHeader: VbeLexeme = VbeLexeme::Header([b'a', b's', b'r', b'c', b'o', b'r', b'e', 0u8]);
	pub const ProofHeader: VbeLexeme = VbeLexeme::Header([b'a', b's', b'r', b'p', b'r', b'o', b'o', b'f']);
// 	pub fn header(&self) -> [u8; 8] {
// 		[self.val as u8,
// 		(self.val >> 8u32) as u8,
// 		(self.val << 16u32) as u8,
// 		(self.val << 24u32) as u8,
// 		(self.val << 32u32) as u8,
// 		(self.val << 40u32) as u8,
// 		(self.val << 48u32) as u8,
// 		(self.val << 56u32) as u8]
// 	}
// 	pub fn number(&self) -> &u64 {
// 		&self.val
// 	}
}
// impl Into<u64> for VbeLexeme {
// 	fn into(self) -> u64 {
// 		self.val
// 	}
// }
// impl Into<i64> for VbeLexeme {
// 	fn into(self) -> i64 {
// 		let res = (self.val >> 1u32) as i64;
// 		if self.val & 1u64 == 0u64 {
// 			res
// 		} else {
// 			-res
// 		}
// 	}
// }

pub struct VbeLexer {
	input: InputStream,
}
impl VbeLexer {
	pub fn new(is: InputStream) -> VbeLexer {
		VbeLexer { input: is }
	}
	pub fn next(&mut self) -> LexingResult<Option<VbeLexeme>> {
		let mut num: u64 = 0u64;
		let mut shift: u32 = 0u32;
		loop { match self.input.next_err::<LexingError>()? {
			Some(c) => if shift < 63u32 || (shift == 63u32 && (c & 0b0111_1111u8) <= 1u8) || (shift == 70u32 && c == 0u8) {
				if c == 0u8 && shift != 0u32 {
					break Ok(Some(VbeLexeme::Header(num.to_be_bytes())));
				}
				num |= ((c & 0b0111_1111u8) as u64) << shift;
				shift += 7u32;
				if c < 0b1000_0000u8 {
					break Ok(Some(VbeLexeme::Number(num)));
				}
			} else {
				self.error_out_of_range(num, c)?
			},
			None => if shift == 0u32 {
				break Ok(None)
			} else {
				self.error_unterminated_integer()?
			},
		} }
	}
	#[inline]
	pub fn next_err<E>(&mut self) -> Result<Option<VbeLexeme>, E> where
		E: Error + From<LexingError>
	{
		self.next().map_err(E::from)
	}
	fn error_leading_zeroes<T>(&mut self) -> LexingResult<T> {
		Err(LexingError::syntax_error(self, format!("Variable-bit encoded number contains leading zeroes.")))
	}
	fn error_unterminated_integer<T>(&mut self) -> LexingResult<T> {
		Err(LexingError::syntax_error(self, format!("Unterminated variable-bit encoded number.")))
	}
	fn error_out_of_range<T>(&mut self, num: u64, c: u8) -> LexingResult<T> {
		let mut vec = Vec::<u8>::new();
		let mut shift: u32 = 0u32;
		while shift < 63u32 {
			vec.push(((num >> shift) as u8) & 0b0111_1111u8);
			shift += 7u32;
		}
		vec.push(c & 0b0111_1111u8);
		while let Ok(Some(cc)) = self.input.next() {
			vec.push(cc & 0b0111_1111u8);
			if cc & 0b1000_0000 == 0u8 {
				break;
			}
		}
		Err(LexingError::out_of_range_integer(&self.input, &vec))
	}
}
impl Positionable for VbeLexer {
	fn position(&self) -> &FilePosition {
		self.input.position()
	}
}

mod test {
	use std::{
		path::{Path},
	};
	use crate::{
		input::{InputStream},
		lexer::{DimacsLexer, LexingError, VbeLexer, VbeLexeme,
			DimacsLexeme::{self, Header, Number, Letter},
		},
	};

	fn check_dimacs_lexer(path: &Path, check: &Vec<DimacsLexeme>, f: Option<Box<dyn Fn(&LexingError) -> bool>>) {
		let is = InputStream::open_plain(path).expect("Could not open file");
		let mut lexer = DimacsLexer::new(is);
		let mut vec = Vec::<DimacsLexeme>::new();
		loop {
			match lexer.next() {
				Ok(Some(x)) => vec.push(x),
				Err(e) => {
					match f {
						Some(ff) => assert!(ff(&e)),
						None => assert!(false),
					}
					break;
				}
				_ => break,
			}
		}
		let mut it1 = check.iter();
		let mut it2 = vec.iter();
		loop {
			match (it1.next(), it2.next()) {
				(Some(x), Some(y)) => assert!(x == y),
				(None, None) => break,
				_ => assert!(false),
			}
		}
	}

	fn check_vbe_lexer(path: &Path, check: &Vec<u64>, f: Option<Box<dyn Fn(&LexingError) -> bool>>) {
		let is = InputStream::open_plain(path).expect("Could not open file");
		let mut lexer = VbeLexer::new(is);
		let mut vec = Vec::<VbeLexeme>::new();
		loop {
			match lexer.next() {
				Ok(Some(x)) => {
					vec.push(x);
				},
				Err(e) => {
					match f {
						Some(ff) => assert!(ff(&e)),
						None => assert!(false),
					}
					break;
				}
				_ => break,
			}
		}
		let mut it1 = check.iter();
		let mut it2 = vec.iter();
		loop {
			match (it1.next(), it2.next()) {
				(Some(x), Some(y)) => assert!(y == &VbeLexeme::Number(*x)),
				(None, None) => break,
				_ => assert!(false),
			}
		}
	}

	#[test]
	fn test_dimacs_lexer() {
		let tests: Vec<(&str, Vec<DimacsLexeme>, Option<Box<dyn Fn(&LexingError) -> bool>>)> = vec![
			("test/dimacs_lexer/cnf0.txt", vec![
				Header([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]),
				Number(50i64), Number(80i64), Number(16i64), Number(17i64),
				Number(30i64), Number(0i64), Number(-17i64), Number(22i64),
				Number(30i64), Number(0i64), Number(-17i64), Number(-22i64),
				Number(30i64), Number(0i64), Number(16i64), Number(-30i64),
				Number(47i64), Number(0i64), Number(16i64), Number(-30i64),
				Number(-47i64), Number(0i64), Number(-16i64), Number(-21i64),
				Number(31i64), Number(0i64), Number(-16i64), Number(-21i64),
				Number(-31i64), Number(0i64), Number(-16i64), Number(21i64),
				Number(-28i64), Number(0i64)
			], None),
			("test/dimacs_lexer/cnf1.txt", vec![
				Header([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]),
				Number(50i64), Number(80i64), Number(16i64), Number(17i64),
				Number(30i64), Number(0i64), Number(-17i64), Number(22i64),
				Number(30i64), Number(0i64)
			], Some(Box::new(|err| err.is_syntax_error("Could not parse number.")))),
			("test/dimacs_lexer/cnf2.txt", vec![], Some(Box::new(|err| err.is_syntax_error("Could not parse header.")))),
			("test/dimacs_lexer/cnf3.txt", vec![],
				Some(Box::new(|err| err.is_header_too_long(&vec![b'a', b'v', b'e', b'r', b'y', b'l', b'o', b'n', b'g', b'h', b'e', b'a', b'd', b'e', b'r'])))),
			("test/dimacs_lexer/cnf4.txt", vec![
				Header([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]), Number(50i64), Number(9223372036854775807i64)
			], None),
			("test/dimacs_lexer/cnf5.txt", vec![
				Header([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]), Number(50i64)
			], Some(Box::new(|err| err.is_out_of_range_integer(&vec![b'9',b'2',b'2',b'3',b'3',b'7',b'2',b'0',b'3',b'6',b'8',b'5',b'4',b'7',b'7',b'5',b'8',b'0',b'8'])))),
			("test/dimacs_lexer/cnf6.txt", vec![
				Header([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]), Number(50i64), Number(-9223372036854775808i64)
			], None),
			("test/dimacs_lexer/cnf7.txt", vec![
				Header([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]), Number(50i64)
			], Some(Box::new(|err| err.is_out_of_range_integer(&vec![b'-', b'9',b'2',b'2',b'3',b'3',b'7',b'2',b'0',b'3',b'6',b'8',b'5',b'4',b'7',b'7',b'5',b'8',b'0',b'9'])))),
			("test/dimacs_lexer/cnf8.txt", vec![
				Header([b'l', b'e', b't', b't', b'e', b'r', b's', 0u8]),
				Number(50i64), Number(80i64), Number(16i64), Number(17i64),
				Number(30i64), Number(0i64), Number(-17i64), Letter(b'x'),
				Number(3i64), Number(0i64), Number(-17i64), Letter(b'b'),
				Letter(b'A'), Number(0i64)
			], None),
			("test/dimacs_lexer/cnf9.txt", vec![
				Header([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]),
				Number(50i64), Number(80i64)
			], Some(Box::new(|err| err.is_syntax_error("Could not parse letter."))))
		];
		for (path, check, err) in tests {
			check_dimacs_lexer(Path::new(path), &check, err)
		}
	}

	#[test]
	fn test_vbe_lexer() {
		let tests: Vec<(&str, Vec<u64>, Option<Box<dyn Fn(&LexingError) -> bool>>)> = vec![
			("test/vbe_lexer/vbe0.bin",
				vec![142848914u64, 85u64, 55u64, 104u64, 1u64, 36u64, 12595012903u64, 10279u64, 19u64, 67u64, 62028944u64, 113u64, 86u64, 37u64, 29888669399u64],
				None
			),
			("test/vbe_lexer/vbe1.bin",
				vec![142848914u64, 85u64, 55u64, 104u64, 1u64, 36u64, 12595012903u64, 10279u64, 19u64, 67u64, 62028944u64, 113u64, 86u64, 37u64, 29888669399u64, 18446744073709551615u64],
				None),
			("test/vbe_lexer/vbe2.bin",
				vec![142848914u64, 85u64, 55u64, 104u64, 1u64, 36u64, 12595012903u64, 10279u64, 19u64, 67u64, 62028944u64, 113u64, 86u64, 37u64, 29888669399u64],
				Some(Box::new(|err| err.is_out_of_range_integer(&vec![0x00u8, 0x00u8, 0x00u8, 0x00u8, 0x00u8, 0x00u8, 0x00u8, 0x00u8, 0x00u8, 0x02u8])))),
			("test/vbe_lexer/vbe3.bin",
				vec![142848914u64, 85u64, 55u64, 104u64, 1u64, 36u64, 12595012903u64, 10279u64, 19u64, 67u64, 62028944u64, 113u64, 86u64, 37u64, 29888669399u64],
				Some(Box::new(|err| err.is_syntax_error("Unterminated variable-bit encoded number.")))
			)
		];
		for (path, check, err) in tests {
			check_vbe_lexer(Path::new(path), &check, err)
		}
	}
}