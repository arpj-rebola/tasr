use std::{
	error::{Error},
	fs::{File},
	fmt::{self, Display, Binary, Formatter, Debug},
	io::{self, Read},
	mem::{self},
	path::{Path},
};

#[derive(Debug, Clone)]
pub struct FilePosition {
	name: String,
	col: usize,
	ln: usize,
	pos: usize,
}
impl FilePosition {
	pub fn new(name: &str) -> FilePosition {
		FilePosition {
			name: name.to_string(),
			col: 1usize,
			ln: 1usize,
			pos: 0usize,
		}
	}
	pub fn forward(&mut self) {
		self.col += 1usize;
		self.pos += 1usize;
	}
	pub fn newline(&mut self) {
		self.col = 1usize;
		self.ln += 1usize;
		self.pos += 1usize;
	}
}
impl Display for FilePosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "\"{:?}\": {:?}:{:?}", self.name, self.ln, self.col)
    }
}
impl Binary for FilePosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "\"{:?}\": {:?}", self.name, self.pos)
    }
}

pub trait Positionable {
	fn position(&self) -> &FilePosition;
	fn copy_position(&self) -> FilePosition {
		self.position().clone()
	}
}

pub struct IoError {
	pos: FilePosition,
	err: io::Error,
}

pub struct InputError(Box<IoError>);
impl InputError {
	fn io_error(pos: &FilePosition, err: io::Error) -> InputError {
		InputError(Box::<IoError>::new(IoError {
			pos: pos.clone(),
			err: err,
		}))
	}
}
impl Debug for InputError {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "{}", self)
	}
}
impl Display for InputError {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "Reading error while parsing file {}:\n{}", &self.0.pos, &self.0.err)
	}
}
impl Binary for InputError {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		write!(f, "Reading error while parsing file {:b}:\n{}", &self.0.pos, &self.0.err)
	}
}
impl Error for InputError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
		Some(&self.0.err)
    }
}

pub type InputResult<T> = Result<T, InputError>;

pub struct InputStream {
	it: Box<dyn Iterator<Item = Result<u8, io::Error>>>,
	pos: FilePosition,
	pk: InputResult<Option<u8>>,
}
impl InputStream {
	pub fn open_plain(path: &Path) -> Result<InputStream, io::Error> {
		let fs = File::open(&path)?;
		let name = format!("{}", path.display());
		Ok(InputStream::new(fs.bytes(), &name))
	}
	pub fn new<I>(mut it: I, name: &str) -> InputStream where
		I: 'static + Iterator<Item = Result<u8, io::Error>> + Sized,
	{
		let pos = FilePosition::new(name);
		let pk = match it.next() {
			Some(Ok(c)) => Ok(Some(c)),
			Some(Err(e)) => Err(InputError::io_error(&pos, e)),
			None => Ok(None),
		};
		InputStream {
			it: Box::<I>::new(it),
			pos: FilePosition::new(name),
			pk: pk,
		}
	}
	#[inline]
	pub fn next_err<E>(&mut self) -> Result<Option<u8>, E> where
		E: Error + From<InputError>
	{
		self.next().map_err(E::from)
	}
	pub fn next(&mut self) -> InputResult<Option<u8>> {
		let mut next = match self.pk {
			Ok(Some(c)) => {
				if c == b'\n' {
					self.pos.newline();
				} else {
					self.pos.forward();
				}
				match self.it.next() {
					Some(Ok(c)) => Ok(Some(c)),
					Some(Err(e)) => Err(InputError::io_error(&self.pos, e)),
					None => Ok(None),
				}
			},
			Ok(None) | Err(_) => Ok(None),
		};
		mem::swap(&mut next, &mut self.pk);
		next
	}
	#[inline]
	pub fn peek_err<E>(&mut self) -> Result<&Option<u8>, E> where
		E: Error + From<InputError>
	{
		self.peek().map_err(E::from)
	}
	pub fn peek(&mut self) -> InputResult<&Option<u8>> {
		let mt = &mut self.pk;
		match mt {
			Ok(res) => Ok(res),
			Err(_) => {
				let mut dummy = Ok(None);
				mem::swap(&mut dummy, mt);
				Err(dummy.unwrap_err())
			},
		}
	}
	pub fn disable(&mut self) {
		self.pk = Ok(None)
	}
}
impl Positionable for InputStream {
	fn position(&self) -> &FilePosition {
		&self.pos
	}
}

#[cfg(test)]
mod test {
	use std::{
		path::{Path},
	};
	use crate::{
		input::{InputStream},
	};

	#[test]
	fn test_input_stream() {
		let path = Path::new("test/file_reader/filereader.txt");
		let mut is = InputStream::open_plain(&path).expect("Couldn't find file");
		let mut vec = Vec::<u8>::new();
		while let Ok(Some(x)) = is.peek() {
			let y1 = *x;
			match is.next() {
				Ok(Some(y2)) => assert!(y1 == y2),
				_ => assert!(false),
			}
			vec.push(y1);
		}
		match is.next() {
			Ok(None) => (),
			_ => assert!(false),
		}
		let test = vec!(b't', b'h', b'i', b's', b'\n', b'i', b's', b' ', b'a', b'\n', b't', b'e', b's', b't', b'\n', b'f', b'i', b'l', b'e');
		assert!(vec == test);
	}
}