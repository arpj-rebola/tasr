use std::{
	convert::{TryFrom},
	fs::{File},
	fmt::{self, Display, Formatter, Debug},
	io::{Read, Stdin, BufWriter, Write, Result as IoResult, BufReader},
	path::{Path, PathBuf},
};

use zstd::stream::{
	read::Decoder as ZstDecoder,
	write::Encoder as ZstEncoder,
};
use flate2::{
	read::GzDecoder as GzDecoder,
	write::GzEncoder as GzEncoder,
	Compression as GzCompression,
};
use bzip2::{
	read::BzDecoder as Bz2Decoder,
	write::BzEncoder as Bz2Encoder,
	Compression as Bz2Compression,
};
use xz2::{
	read::XzDecoder as XzDecoder,
	write::XzEncoder as XzEncoder,
};
use lz4::{
	Decoder as Lz4Decoder,
	EncoderBuilder as Lz4Encoder,
};

#[derive(Debug, Clone)]
pub struct FilePosition<'a> {
	name: &'a Path,
	ln: usize,
	col: usize,
}
impl<'a> FilePosition<'a> {
	pub fn new<'b: 'a>(name: &'b Path, binary: bool) -> FilePosition<'a> {
		FilePosition::<'a> {
			name: name,
			col: 4usize,
			ln: if binary {
				0usize
			} else {
				1usize
			},
		}
	}
	pub fn forward(&mut self) {
		if self.col & 1usize == 0usize {
			self.col += 4usize;
		} else {
			self.ln += 1usize;
			self.col = 8usize;
		}
	}
	pub fn newline(&mut self) {
		if self.binary() {
			self.col += 4usize;
		} else {
			self.forward();
			self.col &= 1usize;
		}
	}
	pub fn finish(&mut self) {
		if self.col & 2usize == 0usize {
			self.forward();
			self.col &= 2usize;
		}
	}
	#[inline]
	pub fn binary(&self) -> bool {
		self.ln == 0usize
	}
	pub fn source(&self) -> SourcePosition {
		SourcePosition {
			name: PathBuf::from(self.name),
			col: self.column(),
			ln: self.line(),
		}
	}
	pub fn name(&self) -> &'a Path {
		self.name
	}
	pub fn column(&self) -> usize {
		if self.col & 1usize == 0usize {
			(self.col >> 2) - 1usize
		} else {
			1usize
		}
	}
	pub fn line(&self) -> usize {
		if self.ln & 1usize == 0usize {
			self.ln
		} else {
			self.ln + 1usize
		}
	}
}
impl<'a> Display for FilePosition<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		if self.binary() {
			write!(f, "{:?} (byte {:?})", self.name, self.column())
		} else {
			write!(f, "{:?} {:?}:{:?}", self.name, self.line(), self.column())
		}
    }
}

pub struct FilePositionTracker<'a> {
	name: &'a Path,
	current: u8,
	pos: [usize; 4],
}
impl<'a> FilePositionTracker<'a> {
	pub fn new<'b: 'a>(name: &'b Path) -> FilePositionTracker<'a> {
		FilePositionTracker::<'a> {
			current: 0u8,
			name: name,
			pos: [0usize, 0usize, 0usize, 0usize],
		}
	}
	pub fn push(&mut self, pos: &FilePosition<'_>) {
		self.current ^= 2u8;
		self.pos[self.current as usize] = pos.line();
		self.pos[self.current as usize + 1usize] = pos.column();
	}
	pub fn left(&self) -> StreamPosition {
		StreamPosition {
			ln: self.pos[(self.current ^ 2u8) as usize],
			col: self.pos[(self.current ^ 2u8) as usize + 1usize],
		}
	}
	pub fn right(&self) -> StreamPosition {
		StreamPosition {
			ln: self.pos[self.current as usize],
			col: self.pos[self.current as usize + 1usize],
		}
	}
	pub fn name(&self) -> &Path {
		self.name
	}
}

#[derive(Clone)]
pub struct StreamPosition {
	col: usize,
	ln: usize,
}
impl StreamPosition {
	pub fn source<'a>(&self, name: &Path) -> SourcePosition {
		SourcePosition {
			name: PathBuf::from(name),
			col: self.col,
			ln: self.ln,
		}
	}
}

pub struct Positioned<T>(pub T, pub StreamPosition);
impl<T> Positioned<T> {
	pub fn value(&self) -> &T {
		&self.0
	}
	pub fn position(&self) -> &StreamPosition {
		&self.1
	}
	pub fn take(self) -> T {
		self.0
	}
}

#[derive(Debug)]
pub struct SourcePosition {
	name: PathBuf,
	ln: usize,
	col: usize,
}
impl SourcePosition {
	pub fn binary(&self) -> bool {
		self.ln == 0usize
	}
}
impl Display for SourcePosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		if self.binary() {
			write!(f, "{:?} (byte {:?})", self.name, self.col)
		} else {
			write!(f, "{:?} {:?}:{:?}", self.name, self.ln, self.col)
		}
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
			Some("plain") | None => CompressionFormat::Plain,
			Some(rf) => Err(rf)?
		})
	}
}

pub struct InputStream<'a> {
	it: Box<dyn 'a + Iterator<Item = u8>>,
	pos: FilePosition<'a>,
}
impl<'a> InputStream<'a> {
	pub fn stdin<'b: 'a>(stdin: &'b Stdin, binary: bool) -> InputStream<'a> {
		InputStream::<'a>::new(BufReader::new(stdin.lock()).bytes(), Path::new("(stdin)"), binary)
	}
	pub fn string<'b: 'a, 'c: 'a>(source: &'b str, name: &'c str, binary: bool) -> InputStream<'a> {
		InputStream::<'a>::new(source.bytes().map(|x| Ok(x)), Path::new(name), binary)
	}
	pub fn open<'b: 'a>(path: &'b Path, format: CompressionFormat, binary: bool) -> InputStream<'a> {
		let file = File::open(&path).unwrap_or_else(|err| panic!(err));
		match format {
			CompressionFormat::Plain => InputStream::<'a>::new(BufReader::new(file).bytes(), path, binary),
			CompressionFormat::Zst => InputStream::<'a>::new(BufReader::new(ZstDecoder::new(file).unwrap_or_else(|err| panic!(err))).bytes(), path, binary),
			CompressionFormat::Gz => InputStream::<'a>::new(BufReader::new(GzDecoder::new(file)).bytes(), path, binary),
			CompressionFormat::Bz2 => InputStream::<'a>::new(BufReader::new(Bz2Decoder::new(file)).bytes(), path, binary),
			CompressionFormat::Xz => InputStream::<'a>::new(BufReader::new(XzDecoder::new(file)).bytes(), path, binary),
			CompressionFormat::Lz4 => InputStream::<'a>::new(BufReader::new(Lz4Decoder::new(file).unwrap_or_else(|err| panic!(err))).bytes(), path, binary),
		}
	}
	pub fn new<'b: 'a, 'c: 'a, I>(it: I, path: &'b Path, binary: bool) -> InputStream<'a> where
		I: 'c + Iterator<Item = IoResult<u8>>
	{
		InputStream::<'a> {
			it: Box::new(it.map(|x| x.unwrap_or_else(|err| panic!(err))).fuse()),
			pos: FilePosition::<'a>::new(path, binary),
		}
	}
	pub fn position(&self) -> &FilePosition {
		&self.pos
	}
	pub fn name(&self) -> &'a Path {
		self.pos.name
	}
	pub fn binary(&self) -> bool {
		self.pos.binary()
	}
}
impl<'a> Iterator for InputStream<'a> {
	type Item = u8;
	fn next(&mut self) -> Option<u8> {
		let res = self.it.next();
		match res {
			Some(b'\n') => self.pos.newline(),
			Some(_) => self.pos.forward(),
			None => self.pos.finish(),
		}
		res
	}
}

pub struct OutputStream<'a> {
	buf: BufWriter<Box<dyn 'a + Write>>,
}
impl<'a> OutputStream<'a> {
	pub fn new<'b: 'a, W: 'b + Write>(wt: W) -> OutputStream<'a> {
		OutputStream::<'a> {
			buf: BufWriter::new(Box::new(wt))
		}
	}
	pub fn open<'b: 'a>(path: &'b Path, format: CompressionFormat) -> OutputStream<'a> {
		let file = File::create(&path).unwrap_or_else(|err| panic!(err));
		match format {
			CompressionFormat::Plain => OutputStream::<'a>::new(file),
			CompressionFormat::Zst => OutputStream::<'a>::new(ZstEncoder::<File>::new(file, 0i32).unwrap_or_else(|err| panic!(err)).auto_finish()),
			CompressionFormat::Gz => OutputStream::<'a>::new(GzEncoder::<File>::new(file, GzCompression::default())),
			CompressionFormat::Bz2 => OutputStream::<'a>::new(Bz2Encoder::<File>::new(file, Bz2Compression::Default)),
			CompressionFormat::Xz => OutputStream::<'a>::new(XzEncoder::<File>::new(file, 3u32)),
			CompressionFormat::Lz4 => OutputStream::<'a>::new(Lz4Encoder::new().level(3u32).build(file).unwrap_or_else(|err| panic!(err))),
		}
	}
}
impl<'a> Write for OutputStream<'a> {
	fn write(&mut self, buf: &[u8]) -> IoResult<usize> {
		Ok(self.buf.write(buf).unwrap_or_else(|err| panic!(err)))
	}
	fn flush(&mut self) -> IoResult<()> {
		Ok(self.buf.flush().unwrap_or_else(|err| panic!(err)))
	}
}

#[cfg(test)]
mod test {
	use std::{
		path::{Path},
	};
	use crate::{
		input::{InputStream, CompressionFormat},
	};

	#[test]
	fn test_input_stream() {
		let path = Path::new("test/file_reader/filereader.txt");
		let mut is = InputStream::<'_>::open(&path, CompressionFormat::Plain, false);
		let mut vec = Vec::<u8>::new();
		loop { match is.next() {
			Some(x) => vec.push(x),
			None => break,
		} }
		let test = vec!(b't', b'h', b'i', b's', b'\n', b'i', b's', b' ', b'a', b'\n', b't', b'e', b's', b't', b'\n', b'f', b'i', b'l', b'e');
		assert!(vec == test);
	}
}