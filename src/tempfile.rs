use std::{
    fs::{self, OpenOptions, File},
    io::{self, Write, ErrorKind as IoErrorKind, Error as IoError, BufReader},
    path::{Path, PathBuf},
};

use rand::{
    self, Rng
};


pub struct TempFiles {
    root: PathBuf,
    split: Vec<PathBuf>,
    trimmed: Vec<PathBuf>,
    core: Option<PathBuf>,
}
impl TempFiles {
    pub fn new(path: &Path) -> TempFiles {
        if !path.is_dir() {
            fs::create_dir_all(path).unwrap_or_else(|err| TempFiles::temp_dir_error(path, err));
        }
        TempFiles {
            root: PathBuf::from(path),
            split: Vec::new(),
            trimmed: Vec::new(),
            core: None,
        }
    }
    pub fn split(&mut self) -> SplitTempFiles<'_> {
        SplitTempFiles::<'_> { temp: self }
    }
    pub fn trimmed(&mut self) -> TrimmedTempFiles<'_> {
        let index = self.split.len();
        TrimmedTempFiles::<'_> {
            temp: self,
            index: index,
        }
    }
    pub fn conflate_text<W: Write>(self, source: &Path, count: u64, wt: &mut W) {
        let mut core: bool = true;
        for path in self.core.iter().chain(self.trimmed.iter()) {
            let mut file = match OpenOptions::new().read(true).open(&path) {
                Ok(file) => BufReader::new(file),
                Err(err) => TempFiles::opening_error(&path, err),
            };
            if core {
                write!(wt, "p source \"{}\"\n", source.to_string_lossy().escape_default())
                    .unwrap_or_else(|err| panic!(format!("{}", err)));
                write!(wt, "p count {}\n", count)
                    .unwrap_or_else(|err| panic!(format!("{}", err)));
                write!(wt, "p core\n")
                    .unwrap_or_else(|err| panic!(format!("{}", err)));
            }
            io::copy(&mut file, wt).unwrap_or_else(|err| panic!(format!("{}", err)));
            if core {
                write!(wt, "p proof\n")
                    .unwrap_or_else(|err| panic!(format!("{}", err)));
            }
            core = false;
            if path.exists() {
                fs::remove_file(path).unwrap_or_else(|err| TempFiles::deletion_error(path.as_path(), err))
            }
        }
    }
    pub fn conflate_binary<W: Write>(self, source: &Path, count: u64, wt: &mut W) {
        let mut core: bool = true;
        for path in self.core.iter().chain(self.trimmed.iter()) {
            let mut file = match OpenOptions::new().read(true).open(&path) {
                Ok(file) => BufReader::new(file),
                Err(err) => TempFiles::opening_error(&path, err),
            };
            if core {
                wt.write_all(&[0x00, 0x01, b'p', b's', b'o', b'u', b'r', b'c', b'e', 0x00, 0x01, b'"']).unwrap_or_else(|err| panic!(format!("{}", err)));
                write!(wt, "{}\"", source.to_string_lossy().escape_default()).unwrap_or_else(|err| panic!(format!("{}", err)));
                wt.write_all(&[0x01, b'p', b'c', b'o', b'u', b'n', b't', 0x00]).unwrap_or_else(|err| panic!(format!("{}", err)));
                let mut num: u64 = count << 1;
                loop {
                    let c = (num & 0b0111_1111u64) as u8;
                    num >>= 7;
                    let cont = num != 0u64;
                    wt.write_all(&[c | ((cont as u8) * 0b1000_0000u8)]).unwrap_or_else(|err| panic!(format!("{}", err)));
                    if !cont {
                        break;
                    }
                }
                wt.write_all(&[0x01, b'p', b'c', b'o', b'r', b'e', 0x00]).unwrap_or_else(|err| panic!(format!("{}", err)));
            }
            io::copy(&mut file, wt).unwrap_or_else(|err| panic!(format!("{}", err)));
            if core {
                wt.write_all(&[0x01, b'p', b'p', b'r', b'o', b'o', b'f', 0x00]).unwrap_or_else(|err| panic!(format!("{}", err)));
            }
            core = false;
            if path.exists() {
                fs::remove_file(path).unwrap_or_else(|err| TempFiles::deletion_error(path.as_path(), err))
            }
        }
    }
    pub fn clear(&mut self) {
        self.split.clear();
        self.trimmed.clear();
    }
    fn generate(&self) -> (PathBuf, File) {
        let mut rng = rand::thread_rng();
        loop {
            let num: u64 = rng.gen();
            let filename = self.root.join(&Path::new(&format!("temp_{:x}.tmp", num)));
            match OpenOptions::new().write(true).create_new(true).open(&filename) {
                Ok(file) => break (filename, file),
                Err(e) if e.kind() != IoErrorKind::AlreadyExists => TempFiles::creation_error(&filename, e),
                _ => (),
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
impl Drop for TempFiles {
    fn drop(&mut self) {
        for path in &self.split {
            if path.exists() {
                fs::remove_file(path).unwrap_or_else(|err| TempFiles::deletion_error(path.as_path(), err))
            }
        }
        for path in &self.trimmed {
            if path.exists() {
                fs::remove_file(path).unwrap_or_else(|err| TempFiles::deletion_error(path.as_path(), err))
            }
        }
        if let Some(path) = &self.core {
            if path.exists() {
                fs::remove_file(path).unwrap_or_else(|err| TempFiles::deletion_error(path.as_path(), err))
            }
        }
    }
}

pub struct SplitTempFiles<'a> {
    temp: &'a mut TempFiles
}
impl<'a> SplitTempFiles<'a> {
    pub fn get(&mut self) -> (&Path, File) {
        let (pb, file) = self.temp.generate();
        self.temp.split.push(pb);
        (self.temp.split.last().unwrap(), file)
    }
}

pub struct TrimmedTempFiles<'a> {
    temp: &'a mut TempFiles,
    index: usize,
}
impl<'a> TrimmedTempFiles<'a> {
    pub fn get(&mut self) -> Option<(&Path, &Path, File)> {
        if let Some(path) = self.temp.split.get(self.index) {
            if path.exists() {
                fs::remove_file(path).unwrap_or_else(|err| TempFiles::deletion_error(path.as_path(), err))
            }
        }
        if self.index == 0usize {
            None
        } else {
            self.index -= 1usize;
            let path_split = unsafe { self.temp.split.get_unchecked(self.index) };
            let (pb_trim, file_trim) = self.temp.generate();
            self.temp.trimmed.push(pb_trim);
            let path_trim = self.temp.trimmed.last().unwrap();
            Some((path_split, path_trim, file_trim))
        }
    }
    pub fn core(self) -> (&'a Path, File) {
        self.temp.trimmed.reverse();
        let (pb, file) = self.temp.generate();
        self.temp.core = Some(pb);
        (self.temp.core.as_ref().unwrap(), file)
    }
}