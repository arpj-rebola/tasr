use std::{
	convert::{TryFrom},
	error::{Error},
	fs::{File},
	fmt::{self, Display, Binary, Formatter, Debug},
	io::{self, Read, Stdin},
	marker::{PhantomData},
	path::{PathBuf, Path},
};

use zstd;
use flate2;
use bzip2;
use xz2;
use lz4;

#[derive(Debug, Clone)]
pub struct FilePosition {
	name: PathBuf,
	col: usize,
	ln: usize,
	pos: usize,
}
impl FilePosition {
	pub fn new(name: &Path) -> FilePosition {
		FilePosition {
			name: name.to_path_buf(),
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

#[derive(Clone, Copy, Debug)]
pub enum CompressionFormat {
	Plain,
	Zst,
	Gz,
	Bz2,
	Xz,
	Lz4,
}
impl<'a> TryFrom<Option<&'a str>> for CompressionFormat {
	type Error = &'a str;
	fn try_from(val: Option<&'a str>) -> Result<CompressionFormat, &'a str> {
		Ok(match val {
			Some("zst") => CompressionFormat::Zst,
			Some("gz") => CompressionFormat::Gz,
			Some("bz2") => CompressionFormat::Bz2,
			Some("xz") => CompressionFormat::Xz,
			Some("lz4") => CompressionFormat::Lz4,
			Some("plain") => CompressionFormat::Plain,
			None => CompressionFormat::Plain,
			Some(rf) => Err(rf)?
		})
	}
}

pub struct InputStream<'a, E: Error + From<io::Error>> {
	it: Box<dyn 'a + Iterator<Item = Result<u8, io::Error>>>,
	pos: FilePosition,
	_ph: PhantomData<E>
}
impl<'a, E: Error + From<io::Error>> InputStream<'a, E> {
	pub fn stdin<'b: 'a>(stdin: &'b Stdin) -> InputStream<'a, E> {
		InputStream::<'a>::new(stdin.lock().bytes(), Path::new("(stdin)"))
	}
	pub fn string<'b: 'a>(s: &'b str, origin: &str) -> InputStream<'a, E> {
		InputStream::<'a>::new(s.bytes().map(|x| Ok(x)), Path::new(origin))
	}
	pub fn open(path: &Path, format: CompressionFormat) -> Result<InputStream<'a, E>, E> {
		let file = File::open(&path).map_err(E::from)?;
		Ok(match format {
			CompressionFormat::Plain => InputStream::<'a>::new(file.bytes(), path),
			CompressionFormat::Zst => InputStream::<'a>::new(zstd::stream::read::Decoder::new(file).map_err(E::from)?.bytes(), path),
			CompressionFormat::Gz => InputStream::<'a>::new(flate2::read::GzDecoder::new(file).bytes(), path),
			CompressionFormat::Bz2 => InputStream::<'a>::new(bzip2::read::BzDecoder::new(file).bytes(), path),
			CompressionFormat::Xz => InputStream::<'a>::new(xz2::read::XzDecoder::new(file).bytes(), path),
			CompressionFormat::Lz4 => InputStream::<'a>::new(lz4::Decoder::new(file).map_err(E::from)?.bytes(), path),
		})
	}
	pub fn new<'b, I>(it: I, path: &Path) -> InputStream<'a, E> where
		I: 'b + Iterator<Item = Result<u8, io::Error>> + Sized,
		'b: 'a,
	{
		InputStream::<'a, E> {
			it: Box::new(it.fuse()),
			pos: FilePosition::new(path),
			_ph: PhantomData,
		}
	}
	pub fn next(&mut self) -> Result<Option<u8>, E> {
		let res = self.it.next().transpose().map_err(E::from);
		match res {
			Ok(Some(c)) => if c == b'\n' {
				self.pos.newline();
			} else {
				self.pos.forward();
			},
			_ => (),
		}
		res
	}
	pub fn position(&self) -> &FilePosition {
		&self.pos
	}
}

#[cfg(test)]
mod test {
	use std::{
		io::{self},
		path::{Path},
	};
	use crate::{
		input::{InputStream, CompressionFormat},
	};

	#[test]
	fn test_input_stream() {
		let path = Path::new("test/file_reader/filereader.txt");
		let mut is = InputStream::<'_, io::Error>::open(&path, CompressionFormat::Plain).expect("Couldn't find file");
		let mut vec = Vec::<u8>::new();
		loop { match is.next() {
			Ok(Some(x)) => vec.push(x),
			Ok(None) => break,
			Err(_) => assert!(false),
		} }
		let test = vec!(b't', b'h', b'i', b's', b'\n', b'i', b's', b' ', b'a', b'\n', b't', b'e', b's', b't', b'\n', b'f', b'i', b'l', b'e');
		assert!(vec == test);
	}
}