use std::{
    io::{self, Result as IoResult, BufReader, Write, BufWriter, Read, Stdout, Stderr, StderrLock, StdoutLock, Bytes},
    path::{PathBuf, Path},
    fmt::{self, Display, Debug, Result as FmtResult, Formatter, Arguments as FmtArguments},
    sync::{Mutex},
    convert::{TryFrom},
};
use owning_ref::{MutexGuardRef};

lazy_static! {
    pub static ref FilePathDb: Mutex<Vec<PathBuf>> = {
        Mutex::new(Vec::<PathBuf>::new())
    };
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct FilePath {
    off: usize
}
impl FilePath {
    #[inline(always)]
    pub fn new(path: PathBuf) -> FilePath {
        let mut lock = FilePathDb.lock().unwrap();
        let off = lock.len();
        lock.push(path);
        FilePath { off: off }
    }
    #[inline(always)]
    pub fn unknown() -> FilePath {
        FilePath { off: usize::max_value() }
    }
    #[inline(always)]
    pub fn path<'a, 'b: 'a>(&'b self) -> MutexGuardRef<'a, Vec<PathBuf>, Path> {
        MutexGuardRef::new(FilePathDb.lock().unwrap()).map(
            |mg| mg.get(self.off).map_or_else(|| Path::new("(unknown)"), |pb| pb.as_path())
        )
    }
    #[inline(always)]
    pub fn join(&self, postfix: &Path) -> FilePath {
        let newpath = {
            let lock = FilePathDb.lock().unwrap();
            let prefix = lock.get(self.off).map_or_else(|| Path::new("(unknown)"), |pb| pb.as_path());
            prefix.join(postfix)
        };
        FilePath::new(newpath)
    }
}
impl Display for FilePath {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let lock = FilePathDb.lock().unwrap();
        let path = lock.get(self.off).map_or_else(|| Path::new("(unknown)"), |pb| pb.as_path());
        write!(f, "{}", path.display())
    }
}

#[repr(transparent)]
#[derive(Clone, Copy)]
pub struct FileOffset {
    val: u64
}
impl FileOffset {
    unsafe fn new(num: u64) -> FileOffset {
        FileOffset { val: num & !1u64 }
    }
    fn put(self, num: &mut u64) {
        *num &= 1u64;
        *num |= self.val;
    }
}
impl TryFrom<i64> for FileOffset {
    type Error = i64;
    fn try_from(num: i64) -> Result<FileOffset, i64> {
        if num >= 0i64 {
            Ok(FileOffset { val: (num as u64) << 1 })
        } else {
            Err(num)
        }
    }
}
impl Debug for FileOffset {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		write!(f, "{}", self.val >> 1)
	}
}

#[derive(Clone, Copy)]
pub struct FilePosition {
    pos: u64,
    file: FilePath,
}
impl FilePosition {
    #[inline(always)]
    pub fn new(binary: bool, path: FilePath) -> FilePosition {
        FilePosition {
            pos: if binary { 0b01u64 } else { 0b10u64 },
            file: path,
        }
    }
    #[inline(always)]
    pub fn offset(&self) -> FileOffset {
        unsafe { FileOffset::new(self.pos) }
    }
    #[inline(always)]
    pub fn set_offset(&mut self, off: FileOffset) {
        off.put(&mut self.pos)
    }
    #[inline(always)]
    pub fn binary_file(&self) -> bool {
        self.pos & 1u64 == 1u64
    }
    #[inline(always)]
    pub fn finished(&self) -> bool {
        self.pos & !1u64 == !1u64
    }
    #[inline(always)]
    pub fn path(&self) -> &FilePath {
        &self.file
    }
    #[inline(always)]
    pub fn text(&self) -> TextFilePosition<'_> {
        TextFilePosition(self)
    }
    #[inline(always)]
    pub fn deferred(&self) -> DeferredFilePosition {
        DeferredFilePosition::new_origin(self.clone())
    }
    #[inline(always)]
    fn advance(&mut self, c: u8) {
        if self.binary_file() || c == b'\n' {
            self.pos += 2u64
        }
    }
    #[inline(always)]
    fn finish(&mut self) {
        self.pos |= !1u64
    }
	#[inline]
	pub fn binary<W: Write>(&self, w: &mut W) -> IoResult<()> {
		let mut num: u64 = self.pos << 1;
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
}
impl Display for FilePosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}: ", &self.file)?;
		if self.finished() {
			write!(f, "EOF")
        } else {
            let word = if self.binary_file() { "byte" } else { "line" };
            write!(f, "{} {}", word, self.pos >> 1)
        }
    }
}
impl Debug for FilePosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}: ", &self.file)?;
		if self.finished() {
			write!(f, "EOF")
        } else {
            let word = if self.binary_file() { "byte" } else { "line" };
            write!(f, "{} {}", word, self.pos >> 1)
        }
    }
}

pub struct TextFilePosition<'a>(&'a FilePosition);
impl<'a> Display for TextFilePosition<'a> {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
		write!(f, "{}", self.0.pos >> 1)
	}
}

#[derive(Clone, Debug)]
pub struct DeferredFilePosition {
    prec: Option<FilePosition>,
    curr: FilePosition,
}
impl DeferredFilePosition {
    #[inline(always)]
    pub fn new_derived(curr: FilePosition, prec: FilePosition) -> DeferredFilePosition {
        DeferredFilePosition {
            prec: Some(prec),
            curr: curr
        }
    }
    #[inline(always)]
    pub fn new_origin(curr: FilePosition) -> DeferredFilePosition {
        DeferredFilePosition {
            prec: None,
            curr: curr,
        }
    }
    #[inline(always)]
    pub fn origin(&self) -> &FilePosition {
        if let Some(x) = &self.prec {
            x
        } else {
            &self.curr
        }
    }
}
impl Display for DeferredFilePosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", &self.curr)?;
        if let Some(pos) = &self.prec {
            write!(f, " (originally from {})", pos)?;
        }
        Ok(())
    }
}

/// Manages a reader by buffering, panicking on errors and keeping track of file position.
pub struct InputReader<R> where R: Read {
    it: Bytes<BufReader<R>>,
    /// File position of the last read item.
	pos: FilePosition,
}
impl<R> InputReader<R> where
    R: Read
{
    /// Creates a new instance from a reader.
    pub fn new(reader: R, path: FilePath, binary: bool) -> InputReader<R> { 
        InputReader {
            it: BufReader::with_capacity(1usize << 20, reader).bytes(),
            pos: FilePosition::new(binary, path),
        }
    }
    #[inline(always)]
	pub fn position(&self) -> &FilePosition {
		&self.pos
    }
    /// Checks if the file is binary.
    pub fn binary_file(&self) -> bool {
        self.pos.binary_file()
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
            Some(Err(err)) => panic!("{}", err),
        }
	}
}

/// Writes to a buffered output sink identified through a path, and manages panics on error.
pub struct OutputWriter<W: Write> {
    /// Buffered writer
    buf: BufWriter<W>,
}
impl<'a, W: Write> OutputWriter<W> {
	pub fn new(wt: W) -> OutputWriter<W> {
		OutputWriter::<W>::with_capacity(wt, 1usize << 20)
    }
    pub fn with_capacity(wt: W, cap: usize) -> OutputWriter<W> {
		OutputWriter::<W> {
            buf: BufWriter::with_capacity(cap, wt),
		}
    }
}
impl<W: Write> Write for OutputWriter<W> {
	fn write(&mut self, buf: &[u8]) -> IoResult<usize> {
		Ok(self.buf.write(buf).unwrap_or_else(|err| panic!("{}", err)))
	}
	fn flush(&mut self) -> IoResult<()> {
		Ok(self.buf.flush().unwrap_or_else(|err| panic!("{}", err)))
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
        lock.write_fmt(args).unwrap_or_else(|err| panic!("{}", err));
        lock
    }
    pub fn err(&self, args: FmtArguments) -> StderrLock {
        let mut lock = self.stderr.lock();
        lock.write_fmt(args).unwrap_or_else(|err| panic!("{}", err));
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
        std::io::Write::write_fmt(&mut $lock, format_args!("\n  ")).unwrap_or_else(|err| panic!("{}", err))
    }
}

#[macro_export]
macro_rules! append {
    ($lock: expr, $($args: expr),*) => {
        std::io::Write::write_fmt(&mut $lock, format_args!("{}", format!($($args),*))).unwrap_or_else(|err| panic!("{}", err))
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
            std::io::Write::write_fmt(&mut $lock, format_args!("\n\n")).unwrap_or_else(|err| panic!("{}", err));
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
            std::io::Write::write_fmt(&mut $lock, format_args!("\n\n")).unwrap_or_else(|err| panic!("{}", err));
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
            $pos, $lock, $block, |lock| std::panic::panic_any(lock))
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
        create_message!(panick @ $title, Option::<&$crate::io::FilePosition>::None, $lock, $block)
    };
}

#[macro_export]
macro_rules! fatal {
    ($title: literal @ $pos: expr, $lock: ident, $block: block) => {
        create_message!(fatal @ $title, Some(&$pos), $lock, $block)
    };
    ($title: literal, $lock: ident, $block: block) => {
        create_message!(fatal @ $title, Option::<&$crate::io::FilePosition>::None, $lock, $block)
    };
}

#[macro_export]
macro_rules! error {
    ($title: literal @ $pos: expr, $lock: ident, $block: block) => {
        create_message!(error @ $title, Some(&$pos), $lock, $block)
    };
    ($title: literal, $lock: ident, $block: block) => {
        create_message!(error @ $title, Option::<&$crate::io::FilePosition>::None, $lock, $block)
    };
}

#[macro_export]
macro_rules! warning {
    ($title: literal @ $pos: expr, $lock: ident, $block: block) => {
        create_message!(warning @ $title, Some(&$pos), $lock, $block)
    };
    ($title: literal, $lock: ident, $block: block) => {
        create_message!(warning @ $title, Option::<&$crate::io::FilePosition>::None, $lock, $block)
    };
}

#[macro_export]
macro_rules! info {
    ($title: literal @ $pos: expr, $lock: ident, $block: block) => {
        create_message!(info @ $title, Some(&$pos), $lock, $block)
    };
    ($title: literal, $lock: ident, $block: block) => {
        create_message!(info @ $title, Option::<&$crate::io::FilePosition>::None, $lock, $block)
    };
}

#[macro_export]
macro_rules! success {
    ($title: literal @ $pos: expr, $lock: ident, $block: block) => {
        create_message!(success @ $title, Some(&$pos), $lock, $block)
    };
    ($title: literal, $lock: ident, $block: block) => {
        create_message!(success @ $title, Option::<&$crate::io::FilePosition>::None, $lock, $block)
    };
}

#[macro_export]
macro_rules! progress {
    ($title: literal @ $pos: expr, $lock: ident, $block: block) => {
        create_message!(progress @ $title, Some(&$pos), $lock, $block)
    };
    ($title: literal, $lock: ident, $block: block) => {
        create_message!(progress @ $title, Option::<&$crate::io::FilePosition>::None, $lock, $block)
    };
}

