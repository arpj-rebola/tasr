use std::{
    io::{self, Result as IoResult, BufReader, Write, BufWriter, Read, Stdout, Stderr, StderrLock, StdoutLock},
    path::{PathBuf, Path},
    fmt::{self, Display, Result as FmtResult, Formatter, Arguments as FmtArguments},
};

/// A clonable file position structure. For binary files, this structure represent a byte position. For text files, it represents a line number.
#[derive(Debug, Clone)]
pub struct FilePositionRef<'a> {
    /// Reference to the file path.
    path: &'a Path,
    /// The first LSB marks whether this file is binary.
    /// The second LSB marks whether EOF has been reached.
    /// The rest of the integer is the line number, in the case of a text file, or the byte, in the case of a binary file.
	offset: u64,
}
impl<'a> FilePositionRef<'a> {
    /// Creates a new `FilePositionRef` in starting position.
    pub fn new<'b: 'a>(path: &'b Path, binary: bool) -> FilePositionRef<'a> {
        FilePositionRef::<'a> {
            path: path,
            offset: if binary {
                0b000u64
            } else {
                0b101u64
            },
        }
    }
    /// Checks if the contents should be updated upon advancing the cursor.
    #[inline(always)]
    fn advance(&mut self, c: u8) {
        if self.binary() || c == b'\n' {
            self.offset += 4u64
        }
    }
    /// Sets a flag that EOF has been reached.
    #[inline(always)]
    fn finish(&mut self) {
        self.offset |= 0b10u64
    }
    /// Checks if the file is binary.
    #[inline(always)]
    pub fn binary(&self) -> bool {
        self.offset & 0b01u64 == 0u64
    }
    /// Checks if EOF has been reached.
    #[inline(always)]
    pub fn finished(&self) -> bool {
        self.offset & 0b10u64 != 0u64
    }
    /// Returns the current offset, or `None` if EOF has been reached.
    pub fn offset(&self) -> Option<u64> {
        if self.finished() {
            None
        } else {
            Some(self.offset >> 2)
        }
    }
    /// Makes an owned copy of this position.
    pub fn position(&self) -> FilePosition {
        FilePosition {
            path: PathBuf::from(self.path),
            offset: self.offset,
        }
    }
}
impl<'a> Display for FilePositionRef<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		if self.finished() {
			write!(f, "{}: EOF", self.path.to_str().unwrap())
		} else if self.binary() {
			write!(f, "{}: byte {}", self.path.to_str().unwrap(), self.offset >> 2)
		} else {
			write!(f, "{}: line {}", self.path.to_str().unwrap(), self.offset >> 2)
		}
    }
}

/// The owned version of `FilePositionRef`, useful when it should outlive the path reference.
pub struct FilePosition {
    /// Copy of the file path.
    path: PathBuf,
    /// The first LSB marks whether this file is binary.
    /// The second LSB marks whether EOF has been reached.
    /// The rest of the integer is the line number, in the case of a text file, or the byte, in the case of a binary file.
    offset: u64,
}
impl FilePosition {
    /// Checks if the file is binary.
    fn binary(&self) -> bool {
        self.offset & 0b01u64 == 0u64
    }
    /// Checks if EOF has been reached.
    fn finished(&self) -> bool {
        self.offset & 0b10u64 != 0u64
    }
}
impl Display for FilePosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		if self.finished() {
			write!(f, "{}: EOF", self.path.to_str().unwrap())
		} else if self.binary() {
			write!(f, "{}: byte {}", self.path.to_str().unwrap(), self.offset >> 2)
		} else {
			write!(f, "{}: line {}", self.path.to_str().unwrap(), self.offset >> 2)
		}
    }
}

/// Manages a reader by buffering, panicking on errors and keeping track of file position.
pub struct InputReader<'a> {
    /// Iterator over the reader stream.
    it: Box<dyn 'a + Iterator<Item = u8>>,
    /// File position of the last read item.
	pos: FilePositionRef<'a>,
}
impl<'a> InputReader<'a> {
    /// Creates a new instance from a reader.
    pub fn new<'b: 'a, 'c: 'a, R>(reader: R, path: &'b Path, binary: bool) -> InputReader<'a> where
        R: 'c + Read
    {
        let it = BufReader::new(reader).bytes();
		InputReader::<'a> {
			it: Box::new(it.map(|x| x.unwrap_or_else(|err| panic!(format!("{}", err)))).fuse()),
			pos: FilePositionRef::<'a>::new(path, binary),
		}
    }
    /// Returns a copy of the current file position.
	pub fn position(&self) -> FilePositionRef<'a> {
		self.pos.clone()
    }
    /// Checks if the file is binary.
    pub fn binary(&self) -> bool {
        self.pos.binary()
    }
}
impl<'a> Iterator for InputReader<'a> {
	type Item = u8;
	fn next(&mut self) -> Option<u8> {
        let res = self.it.next();
        if let Some(c) = res {
            self.pos.advance(c)
        } else {
            self.pos.finish()
        }
		res
	}
}

/// Writes to a buffered output sink identified through a path, and manages panics on error.
pub struct OutputWriter<'a> {
    /// Buffered writer
    buf: BufWriter<Box<dyn 'a + Write>>,
    /// Identifying path
    path: &'a Path,
}
impl<'a> OutputWriter<'a> {
	pub fn new<'b: 'a, W: 'b + Write>(wt: W, path: &'b Path) -> OutputWriter<'a> {
		OutputWriter::<'a>::with_capacity(wt, path, !(!0usize << 16))
    }
    pub fn with_capacity<'b: 'a, W: 'b + Write>(wt: W, path: &'b Path, cap: usize) -> OutputWriter<'a> {
		OutputWriter::<'a> {
            buf: BufWriter::with_capacity(cap, Box::new(wt)),
            path: path,
		}
    }
}
impl<'a> Write for OutputWriter<'a> {
	fn write(&mut self, buf: &[u8]) -> IoResult<usize> {
		Ok(self.buf.write(buf).unwrap_or_else(|err| panic!(format!("{}", err))))
	}
	fn flush(&mut self) -> IoResult<()> {
		Ok(self.buf.flush().unwrap_or_else(|err| panic!(format!("{}", err))))
	}
}

pub struct OutputHandle {
    stdout: Stdout,
    stderr: Stderr,
}
impl OutputHandle {
    pub fn new() -> OutputHandle {
        OutputHandle {
            stdout: io::stdout(),
            stderr: io::stderr(),
        }
    }
    pub fn out(&self, args: FmtArguments) -> StdoutLock {
        let mut lock = self.stdout.lock();
        lock.write_fmt(args).unwrap_or_else(|err| panic!(format!("{}", err)));
        lock
    }
    pub fn err(&self, args: FmtArguments) -> StderrLock {
        let mut lock = self.stderr.lock();
        lock.write_fmt(args).unwrap_or_else(|err| panic!(format!("{}", err)));
        lock
    }
}

lazy_static! {
    pub static ref MainOutput: OutputHandle = {
        OutputHandle::new()
    };
}

pub struct PrintedPanic {
    message: String,
}
impl PrintedPanic {
    pub fn new(args: FmtArguments) -> PrintedPanic {
        PrintedPanic { message: fmt::format(args) }
    }
}
impl PrintedPanic {
    pub fn check(&self, s: &str) -> bool {
        self.message.contains(s)
    }
}
impl Write for PrintedPanic {
	fn write(&mut self, buf: &[u8]) -> IoResult<usize> {
        unsafe { self.message.as_mut_vec().extend_from_slice(buf) }
        Ok(buf.len())
	}
	fn flush(&mut self) -> IoResult<()> {
        Ok(())
    }
}
impl Display for PrintedPanic {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		write!(f, "{}", self.message)
	}
}

#[macro_export]
macro_rules! breakline {
    ($lock: expr) => {
        std::io::Write::write_fmt(&mut $lock, format_args!("\n  ")).unwrap_or_else(|err| panic!(format!("{}", err)))
    }
}

#[macro_export]
macro_rules! append {
    ($lock: expr, $($args: expr),*) => {
        std::io::Write::write_fmt(&mut $lock, format_args!("{}", format!($($args),*))).unwrap_or_else(|err| panic!(format!("{}", err)))
    }
}

#[macro_export]
macro_rules! titled_message {
    ($tag: literal, $title: literal @ $pos: expr, $lock: ident, $tag_style: expr, $title_style: expr, $block: block) => {{
        let mut $lock = $crate::io::PrintedPanic::new(format_args!("{} {}", $tag_style($tag), $title_style($title)));
        breakline!($lock);
        append!($lock, "{} {}", ::colored::Colorize::bold(::colored::Colorize::blue("-->")), format!("{}", &$pos));
        breakline!($lock);
        $block
        std::io::Write::write_fmt(&mut $lock, format_args!("\n\n")).unwrap_or_else(|err| panic!(format!("{}", err)));
        $lock
    }};
    ($tag: literal, $title: literal, $lock: ident, $tag_style: expr, $title_style: expr, $block: block) => {{
        let mut $lock = $crate::io::PrintedPanic::new(format_args!("{} {}", $tag_style($tag), $title_style($title)));
        breakline!($lock);
        $block
        std::io::Write::write_fmt(&mut $lock, format_args!("\n\n")).unwrap_or_else(|err| panic!(format!("{}", err)));
        $lock
    }};
}

#[macro_export]
macro_rules! panick {
    ($title: literal @ $pos: expr, $lock: ident, $block: block) => {{
        let $lock = fatal!($title @ $pos, $lock, $block);
        panic!($lock)
    }};
    ($title: literal, $lock: ident, $block: block) => {{
        let $lock = fatal!($title, $lock, $block);
        panic!($lock)
    }};
}

#[macro_export]
macro_rules! fatal {
    ($title: literal @ $pos: expr, $lock: ident, $block: block) => {{
        titled_message!("Fatal error:", $title @ $pos, $lock,
            |$lock| ::colored::Colorize::bold(::colored::Colorize::red($lock)),
            |$lock| ::colored::Colorize::bold($lock),
            $block);
    }};
    ($title: literal, $lock: ident, $block: block) => {{
        titled_message!("Fatal error:", $title, $lock,
            |$lock| ::colored::Colorize::bold(::colored::Colorize::red($lock)),
            |$lock| ::colored::Colorize::bold($lock),
            $block);
    }};
}

#[macro_export]
macro_rules! error {
    ($title: literal @ $pos: expr, $lock: ident, $block: block) => {{
        titled_message!("Error:", $title @ $pos, $lock,
            |$lock| ::colored::Colorize::bold(::colored::Colorize::red($lock)),
            |$lock| ::colored::Colorize::bold($lock),
            $block);
    }};
    ($title: literal, $lock: ident, $block: block) => {{
        titled_message!("Error:", $title, $lock,
            |$lock| ::colored::Colorize::bold(::colored::Colorize::red($lock)),
            |$lock| ::colored::Colorize::bold($lock),
            $block);
    }};
}

#[macro_export]
macro_rules! warning {
    ($title: literal @ $pos: expr, $lock: ident, $block: block) => {{
        titled_message!("Warning:", $title @ $pos, $lock,
            |$lock| ::colored::Colorize::bold(::colored::Colorize::yellow($lock)),
            |$lock| ::colored::Colorize::bold($lock),
            $block);
    }};
    ($title: literal, $lock: ident, $block: block) => {{
        titled_message!("Warning:", $title, $lock,
            |$lock| ::colored::Colorize::bold(::colored::Colorize::yellow($lock)),
            |$lock| ::colored::Colorize::bold($lock),
            $block);
    }};
}

#[macro_export]
macro_rules! result {
    ($title: literal @ $pos: expr, $lock: ident, $block: block) => {{
        titled_message!("Result:", $title @ $pos, $lock,
            |$lock| ::colored::Colorize::bold(::colored::Colorize::green($lock)),
            |$lock| ::colored::Colorize::bold($lock),
            $block);
    }};
    ($title: literal, $lock: ident, $block: block) => {{
        titled_message!("Result:", $title, $lock,
            |$lock| ::colored::Colorize::bold(::colored::Colorize::green($lock)),
            |$lock| ::colored::Colorize::bold($lock),
            $block);
    }};
}

#[cfg(test)]
pub mod test {
    use std::{
        fs::{File},
        path::{Path},
        io::{Write},
    };
	use crate::{
		io::{InputReader, OutputWriter},
	};

	#[test]
	fn test_input() {
        let file = File::open(Path::new("test/file_reader/filereader.txt")).unwrap();
        let mut reader = InputReader::new(file, &Path::new("test/file_reader/filereader.txt"), false);
        let check = [b't', b'h', b'i', b's', b'\n', b'i', b's', b' ', b'a', b'\n', b't', b'e', b's', b't', b'\n', b'f', b'i', b'l', b'e'];
        assert!(!reader.binary());
        let mut count = 0u64;
        while let Some(c) = reader.next() {
            assert!(&c == check.get(count as usize).unwrap());
            count += 1u64;
            assert!((count / 5u64) + 1u64 == reader.position().offset().unwrap());
        }
        assert!(count == 19u64);
        assert!(reader.position().finished())
    }

    #[test]
    fn test_output() {
        let mut vec = Vec::<u8>::new();
        {
            let mut writer = OutputWriter::new(&mut vec, Path::new("some/path"));
            write!(&mut writer, "this\nis a\ntest\nfile").unwrap();
            writer.flush().unwrap();
        }
        assert!(vec == [b't', b'h', b'i', b's', b'\n', b'i', b's', b' ', b'a', b'\n', b't', b'e', b's', b't', b'\n', b'f', b'i', b'l', b'e']);
    }
}