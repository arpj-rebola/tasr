use std::{
    io::{self, Result as IoResult, BufReader, Write, BufWriter, Read, Stdout, Stderr, StderrLock, StdoutLock, Bytes},
    path::{PathBuf, Path},
    fmt::{self, Display, Result as FmtResult, Formatter, Arguments as FmtArguments},
    sync::{Mutex},
};

#[derive(Copy, Clone, Debug)]
pub struct FilePosition {
    /// The first LSB marks whether this file is binary.
    /// The second LSB marks whether EOF has been reached.
    /// The rest of the integer is the line number, in the case of a text file, or the byte, in the case of a binary file.
    val: u64
}
impl FilePosition {
    #[inline(always)]
    pub fn new(binary: bool) -> FilePosition {
        FilePosition { val: if binary { 0b000u64 } else { 0b101u64 } }
    }
    #[inline(always)]
    fn advance(&mut self, c: u8) {
        if self.binary() || c == b'\n' {
            self.val += 4u64
        }
    }
    /// Sets a flag that EOF has been reached.
    #[inline(always)]
    fn finish(&mut self) {
        self.val |= 0b10u64
    }
    /// Checks if the file is binary.
    #[inline(always)]
    pub fn binary(&self) -> bool {
        self.val & 0b01u64 == 0u64
    }
    /// Checks if EOF has been reached.
    #[inline(always)]
    pub fn finished(&self) -> bool {
        self.val & 0b10u64 != 0u64
    }
    /// Returns the current offset, or `None` if EOF has been reached.
    pub fn offset(&self) -> Option<u64> {
        if self.finished() {
            None
        } else {
            Some(self.val >> 2)
        }
    }
    pub fn text(&self) -> FilePositionText<'_> {
        FilePositionText(self)
    }
    pub fn as_binary<W: Write>(&self, w: &mut W) -> IoResult<()> {
		let mut num: u64 = self.val << 1;
		loop {
			let c = (num & 0b0111_1111u64) as u8;
			num >>= 7;
			let cont = num != 0u64;
			w.write_all(&[c | ((cont as u8) * 0b1000_0000u8)])?;
			if !cont {
				return Ok(())
			}
		}
    }
    pub fn with_path(self, path: &Path) -> PathFilePosition {
        PathFilePosition {
            path: path.to_path_buf(),
            offset: self,
        }
    }
}
impl From<i64> for FilePosition {
    fn from(num: i64) -> FilePosition {
        FilePosition { val: num as u64 }
    }
}
impl Display for FilePosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		if self.finished() {
			write!(f, "EOF")
        } else {
            let word = if self.binary() { "byte" } else { "line" };
            write!(f, "{} {}", word, self.val >> 2)
        }
    }
}

pub struct FilePositionText<'a>(&'a FilePosition);
impl<'a> Display for FilePositionText<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", self.0.val)
    }
}

pub struct PathFilePosition {
    path: PathBuf,
    offset: FilePosition
}
impl PathFilePosition {
    pub fn new(path: &Path, binary: bool) -> PathFilePosition {
        PathFilePosition {
            path: path.to_path_buf(),
            offset: FilePosition::new(binary),
        }
    }
    pub fn path(&self) -> &Path {
        self.path.as_path()
    }
    pub fn offset(&self) -> FilePosition {
        self.offset
    }
    #[inline(always)]
    fn advance(&mut self, c: u8) {
        self.offset.advance(c)
    }
    /// Sets a flag that EOF has been reached.
    #[inline(always)]
    pub fn finish(&mut self) {
        self.offset.finish()
    }
    /// Checks if the file is binary.
    #[inline(always)]
    pub fn binary(&self) -> bool {
        self.offset.binary()
    }
    /// Checks if EOF has been reached.
    #[inline(always)]
    pub fn finished(&self) -> bool {
        self.offset.finished()
    }
    pub fn deferred(self) -> DeferredPosition {
        DeferredPosition(self, None)
    }
}
impl Display for PathFilePosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}: {}", self.path.display(), self.offset)
    }
}

pub struct DeferredPosition(PathFilePosition, Option<PathFilePosition>);
impl DeferredPosition {
    pub fn with_paths(ofp: FilePosition, dfp: Option<FilePosition>, opath: &Path, dpath: &Path) -> DeferredPosition {
        let opfp = ofp.with_path(opath);
        let dpfp = dfp.map(|fp| fp.with_path(dpath));
        DeferredPosition(opfp, dpfp)
    }
}
impl Display for DeferredPosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", &self.0)?;
        if let Some(pfp) = &self.1 {
            write!(f, " (originally from {})", pfp)?;
        }
        Ok(())
    }
}

/// Manages a reader by buffering, panicking on errors and keeping track of file position.
pub struct InputReader<R> where R: Read {
    it: Bytes<BufReader<R>>,
    /// File position of the last read item.
	pos: PathFilePosition,
}
impl<R> InputReader<R> where
    R: Read
{
    /// Creates a new instance from a reader.
    pub fn new(reader: R, path: &Path, binary: bool) -> InputReader<R> {
        InputReader {
            it: BufReader::with_capacity(1usize << 20, reader).bytes(),
            pos: PathFilePosition::new(path, binary),
        }
    }
    
    #[inline(always)]
    pub fn path(&self) -> &Path {
        self.pos.path()
    }
    /// Returns a copy of the current file position.
	pub fn position(&self) -> FilePosition {
		self.pos.offset()
    }
    /// Checks if the file is binary.
    pub fn binary(&self) -> bool {
        self.pos.binary()
    }
}
impl<R> Iterator for InputReader<R> where
    R: Read
{
	type Item = u8;
	fn next(&mut self) -> Option<u8> {
        match self.it.next() {
            Some(Ok(c)) => {
                self.pos.advance(c);
                Some(c)
            }
            None => {
                self.pos.finish();
                None
            },
            Some(Err(err)) => panic!(format!("{}", err)),
        }
	}
}

/// Writes to a buffered output sink identified through a path, and manages panics on error.
pub struct OutputWriter<'a, W: Write> {
    /// Buffered writer
    buf: BufWriter<W>,
    /// Identifying path
    path: &'a Path,
}
impl<'a, W: Write> OutputWriter<'a, W> {
	pub fn new(wt: W, path: &'a Path) -> OutputWriter<'a, W> {
		OutputWriter::<'a, W>::with_capacity(wt, path, 1usize << 20)
    }
    pub fn with_capacity(wt: W, path: &'a Path, cap: usize) -> OutputWriter<'a, W> {
		OutputWriter::<'a> {
            buf: BufWriter::with_capacity(cap, wt),
            path: path,
		}
    }
}
impl<'a, W: Write> Write for OutputWriter<'a, W> {
	fn write(&mut self, buf: &[u8]) -> IoResult<usize> {
		Ok(self.buf.write(buf).unwrap_or_else(|err| panic!(format!("{}", err))))
	}
	fn flush(&mut self) -> IoResult<()> {
		Ok(self.buf.flush().unwrap_or_else(|err| panic!(format!("{}", err))))
	}
}

pub struct OutputMuting {
    mute: bool
}
impl OutputMuting {
    fn new() -> OutputMuting {
        OutputMuting { mute: false }
    }
    pub fn mute(&mut self) {
        self.mute = true;
    }
    pub fn unmute(&mut self) {
        self.mute = false;
    }
    pub fn is_muted(&self) -> bool {
        self.mute
    }
}

lazy_static! {
    pub static ref MutedOutput: Mutex<OutputMuting> = Mutex::new(OutputMuting::new());
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
    pub fn maybe_out(&self, args: FmtArguments) -> Option<StdoutLock> {
        if MutedOutput.lock().unwrap().is_muted() {
            None
        } else {
            Some(self.out(args))
        }
    }
    pub fn maybe_err(&self, args: FmtArguments) -> Option<StderrLock> {
        if MutedOutput.lock().unwrap().is_muted() {
            None
        } else {
            Some(self.err(args))
        }
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
macro_rules! headed_message {
    ($constructor: expr, $tag_style: expr, $tag: literal, $title_style: expr, $title: literal, $pos: expr, $lock: ident, $block: block, $after: expr) => {
        if let Some(mut $lock) = $constructor(format_args!("{} {}", $tag_style($tag), $title_style($title))) {
            breakline!($lock);
            if let Some(pos) = $pos {
                append!($lock, "{} {}", ::colored::Colorize::bold(::colored::Colorize::blue("-->")), format!("{}", pos));
                breakline!($lock);
            }
            $block
            std::io::Write::write_fmt(&mut $lock, format_args!("\n\n")).unwrap_or_else(|err| panic!(format!("{}", err)));
            $after($lock)
        } else {
            let $lock = ();
            $after($lock)
        }
    };
    ($constructor: expr, $title_style: expr, $title: literal, $pos: expr, $lock: ident, $block: block, $after: expr) => {
        if let Some(mut $lock) = $constructor(format_args!("{}", $title_style($title))) {
            breakline!($lock);
            if let Some(pos) = $pos {
                append!($lock, "{} {}", ::colored::Colorize::bold(::colored::Colorize::blue("-->")), format!("{}", pos));
                breakline!($lock);
            }
            $block
            std::io::Write::write_fmt(&mut $lock, format_args!("\n\n")).unwrap_or_else(|err| panic!(format!("{}", err)));
            $after($lock)
        } else {
            let $lock = ();
            $after($lock)
        }
    };
}

#[macro_export]
macro_rules! create_message {
    (panick @ $title: literal, $pos: expr, $lock: ident, $block: block) => {{
        headed_message!(|lock| Some($crate::io::PrintedPanic::new(lock)),
            |msg| ::colored::Colorize::bold(::colored::Colorize::red(msg)), "Fatal error:",
            |msg| ::colored::Colorize::bold(msg), $title,
            $pos, $lock, $block, |lock| panic!(lock))
    }};
    (fatal @ $title: literal, $pos: expr, $lock: ident, $block: block) => {{
        headed_message!(|lock| $crate::io::MainOutput.maybe_err(lock),
            |msg| ::colored::Colorize::bold(::colored::Colorize::red(msg)), "Fatal error:",
            |msg| ::colored::Colorize::bold(msg), $title,
            $pos, $lock, $block, |_| ())
    }};
    (error @ $title: literal, $pos: expr, $lock: ident, $block: block) => {{
        headed_message!(|lock| $crate::io::MainOutput.maybe_err(lock),
            |msg| ::colored::Colorize::bold(::colored::Colorize::red(msg)), "Error:",
            |msg| ::colored::Colorize::bold(msg), $title,
            $pos, $lock, $block, |_| ())
    }};
    (warning @ $title: literal, $pos: expr, $lock: ident, $block: block) => {{
        headed_message!(|lock| $crate::io::MainOutput.maybe_err(lock),
            |msg| ::colored::Colorize::bold(::colored::Colorize::yellow(msg)), "Warning:",
            |msg| ::colored::Colorize::bold(msg), $title,
            $pos, $lock, $block, |_| ())
    }};
    (info @ $title: literal, $pos: expr, $lock: ident, $block: block) => {{
        headed_message!(|lock| $crate::io::MainOutput.maybe_out(lock),
            |msg| ::colored::Colorize::bold(msg), $title,
            $pos, $lock, $block, |_| ())
    }};
    (success @ $title: literal, $pos: expr, $lock: ident, $block: block) => {{
        headed_message!(|lock| $crate::io::MainOutput.maybe_out(lock),
            |msg| ::colored::Colorize::bold(::colored::Colorize::green(msg)), "Success:",
            |msg| ::colored::Colorize::bold(msg), $title,
            $pos, $lock, $block, |_| ())
    }};
    (progress @ $title: literal, $pos: expr, $lock: ident, $block: block) => {{
        headed_message!(|lock| $crate::io::MainOutput.maybe_out(lock),
            |msg| ::colored::Colorize::bold(::colored::Colorize::blue(msg)), "Progress:",
            |msg| ::colored::Colorize::bold(msg), $title,
            $pos, $lock, $block, |_| ())
    }};
}

#[macro_export]
macro_rules! panick {
    ($title: literal @ $pos: expr, $lock: ident, $block: block) => {
        create_message!(panick @ $title, Some(&$pos), $lock, $block)
    };
    ($title: literal, $lock: ident, $block: block) => {
        create_message!(panick @ $title, Option::<&$crate::io::PathFilePosition>::None, $lock, $block)
    };
}

#[macro_export]
macro_rules! fatal {
    ($title: literal @ $pos: expr, $lock: ident, $block: block) => {
        create_message!(fatal @ $title, Some(&$pos), $lock, $block)
    };
    ($title: literal, $lock: ident, $block: block) => {
        create_message!(fatal @ $title, Option::<&$crate::io::PathFilePosition>::None, $lock, $block)
    };
}

#[macro_export]
macro_rules! error {
    ($title: literal @ $pos: expr, $lock: ident, $block: block) => {
        create_message!(error @ $title, Some(&$pos), $lock, $block)
    };
    ($title: literal, $lock: ident, $block: block) => {
        create_message!(error @ $title, Option::<&$crate::io::PathFilePosition>::None, $lock, $block)
    };
}

#[macro_export]
macro_rules! warning {
    ($title: literal @ $pos: expr, $lock: ident, $block: block) => {
        create_message!(warning @ $title, Some(&$pos), $lock, $block)
    };
    ($title: literal, $lock: ident, $block: block) => {
        create_message!(warning @ $title, Option::<&$crate::io::PathFilePosition>::None, $lock, $block)
    };
}

#[macro_export]
macro_rules! info {
    ($title: literal @ $pos: expr, $lock: ident, $block: block) => {
        create_message!(info @ $title, Some(&$pos), $lock, $block)
    };
    ($title: literal, $lock: ident, $block: block) => {
        create_message!(info @ $title, Option::<&$crate::io::PathFilePosition>::None, $lock, $block)
    };
}

#[macro_export]
macro_rules! success {
    ($title: literal @ $pos: expr, $lock: ident, $block: block) => {
        create_message!(success @ $title, Some(&$pos), $lock, $block)
    };
    ($title: literal, $lock: ident, $block: block) => {
        create_message!(success @ $title, Option::<&$crate::io::PathFilePosition>::None, $lock, $block)
    };
}

#[macro_export]
macro_rules! progress {
    ($title: literal @ $pos: expr, $lock: ident, $block: block) => {
        create_message!(progress @ $title, Some(&$pos), $lock, $block)
    };
    ($title: literal, $lock: ident, $block: block) => {
        create_message!(progress @ $title, Option::<&$crate::io::PathFilePosition>::None, $lock, $block)
    };
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