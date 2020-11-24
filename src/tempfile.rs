use std::{
    path::{PathBuf, Path},
    fs::{OpenOptions, self, File},
    io::{ErrorKind as IoErrorKind, Error as IoError, self, BufWriter, BufReader},
};

use rand::{
    self, Rng
};

use crate::{
    io::{OutputWriter, InputReader},
};

pub struct SplittingFiles {
    root: PathBuf,
    split: Vec<PathBuf>,
    binary: bool,
}
impl SplittingFiles {
    pub fn new(path: &Path, binary: bool) -> SplittingFiles {
        if !path.is_dir() {
            fs::create_dir_all(path).unwrap_or_else(|err| SplittingFiles::temp_dir_error(path, err));
        }
        SplittingFiles {
            root: PathBuf::from(path),
            split: Vec::new(),
            binary: binary,
        }
    }
    pub fn get(&mut self) -> OutputWriter<'_> {
        match self.generate() {
            Some((path, file)) => {
                self.split.push(path);
                OutputWriter::new(file, self.split.last().unwrap())
            },
            None => SplittingFiles::naming_error(self.root.as_path()),
        }
    }
    pub fn trimming_files(self) -> TrimmingFiles {
        TrimmingFiles {
            sf: self,
            trim: Vec::new(),
            held: None,
        }
    }
    fn generate(&self) -> Option<(PathBuf, File)> {
        let mut rng = rand::thread_rng();
        let mut count = 0usize;
        loop {
            count += 1usize;
            let num: u64 = rng.gen();
            let filename = self.root.join(&Path::new(&format!("temp_{:x}.tmp", num)));
            match OpenOptions::new().write(true).create_new(true).open(&filename) {
                Ok(file) => break Some((filename, file)),
                Err(e) if e.kind() == IoErrorKind::AlreadyExists && count < 100000usize => (),
                Err(e) if e.kind() == IoErrorKind::AlreadyExists => break None,
                Err(e) => SplittingFiles::creation_error(&filename, e),
            }
        }
    }
    fn temp_dir_error(path: &Path, err: IoError) -> ! {
        panick!("could not create temporary directory", lock, {
            append!(lock, "Could not create a temporary directory in {}:", path.to_str().unwrap());
            breakline!(lock);
            append!(lock, "{}", err);
        })
    }
    fn naming_error(path: &Path) -> ! {
        panick!("unable to name temporary file", lock, {
            append!(lock, "Could not find an available temporary filename in {}.", path.to_str().unwrap());
        })
    }
    fn creation_error(path: &Path, err: IoError) -> ! {
        panick!("unable to create temporary file", lock, {
            append!(lock, "Could not generate temporary file in {}:", path.to_str().unwrap());
            breakline!(lock);
            append!(lock, "{}", err);
        })
    }
    fn opening_error(path: &Path, err: IoError) -> ! {
        panick!("unable to open temporary file", lock, {
            append!(lock, "Could not open temporary file in {}:", path.to_str().unwrap());
            breakline!(lock);
            append!(lock, "{}", err);
        })
    }
    fn deletion_error(path: &Path, err: IoError) -> ! {
        panick!("unable to remove temporary file", lock, {
            append!(lock, "Could not remove temporary file in {}:", path.to_str().unwrap());
            breakline!(lock);
            append!(lock, "{}", err);
        })
    }
}
impl Drop for SplittingFiles {
    fn drop(&mut self) {
        for path in &self.split {
            fs::remove_file(path).unwrap_or_else(|err| SplittingFiles::deletion_error(path.as_path(), err));
        }
    }
}

pub struct TrimmingFiles {
    sf: SplittingFiles,
    trim: Vec<PathBuf>,
    held: Option<PathBuf>,
}
impl TrimmingFiles {
    pub fn get(&mut self) -> Option<(InputReader<'_>, OutputWriter<'_>)> {
        if let Some(path) = self.held.take() {
            fs::remove_file(&path).unwrap_or_else(|err| SplittingFiles::deletion_error(path.as_path(), err));
        }
        let split_path = self.sf.split.pop()?;
        let split_file = match OpenOptions::new().read(true).open(&split_path) {
            Ok(file) => file,
            Err(err) => SplittingFiles::opening_error(&split_path, err),
        };
        self.held = Some(split_path);
        let (trim_path, trim_file) = self.sf.generate().unwrap_or_else(|| SplittingFiles::naming_error(self.sf.root.as_path()));
        self.trim.push(trim_path);
        let input = InputReader::new(split_file, self.held.as_ref().unwrap(), self.sf.binary);
        let output = OutputWriter::new(trim_file, self.trim.last().unwrap());
        Some((input, output))
    }
    pub fn output(self, out: &Path) -> PreprocessedFile {
        PreprocessedFile {
            tf: self,
            out: PathBuf::from(out),
        }
    }
}
impl Drop for TrimmingFiles {
    fn drop(&mut self) {
        if let Some(path) = self.held.take() {
            self.trim.push(path);
        }
        for path in &self.trim {
            fs::remove_file(path).unwrap_or_else(|err| SplittingFiles::deletion_error(path.as_path(), err));
        }
    }
}

pub struct PreprocessedFile {
    tf: TrimmingFiles,
    out: PathBuf,
}
impl PreprocessedFile {
    pub fn get(&mut self) -> OutputWriter<'_> {
        let file = match OpenOptions::new().write(true).truncate(true).create(true).open(&self.out) {
            Ok(file) => file,
            Err(e) => PreprocessedFile::creation_error(&self.out, e),
        };
        OutputWriter::new(file, &self.out)
    }
    pub fn flush(&mut self) {
        let mut output_file = {
            let file = OpenOptions::new().append(true).open(&self.out).unwrap_or_else(|err| PreprocessedFile::opening_error(&self.out, err));
            BufWriter::with_capacity(!(!0usize << 16), file)
        };
        while let Some(input_path) = self.tf.trim.pop() {
            let mut input_file = {
                let file = OpenOptions::new().read(true).open(&input_path).unwrap_or_else(|err| SplittingFiles::opening_error(&input_path, err));
                BufReader::with_capacity(!(!0usize << 16), file)
            };
            io::copy(&mut input_file, &mut output_file).unwrap_or_else(|err| panic!(format!("{}", err)));
            fs::remove_file(&input_path).unwrap_or_else(|err| SplittingFiles::deletion_error(&input_path, err));
        }
    }
    fn creation_error(path: &Path, err: IoError) -> ! {
        panick!("unable to create output file", lock, {
            append!(lock, "Could not generate proof preprocessing output file in {}:", path.to_str().unwrap());
            breakline!(lock);
            append!(lock, "{}", err);
        })
    }
    fn opening_error(path: &Path, err: IoError) -> ! {
        panick!("unable to open output file", lock, {
            append!(lock, "Could not open proof preprocessing output file in {}:", path.to_str().unwrap());
            breakline!(lock);
            append!(lock, "{}", err);
        })
    }
}
