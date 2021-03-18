use std::{
    convert::{TryFrom},
    io::{Read},
    path::{Path},
    char::{self},
    fmt::{Result as FmtResult, Debug, Display, Formatter},
    ptr::{self},
};

use crate::{
    io::{InputReader, FilePosition},
    basic::{Literal, Variable, ClauseIndex},
};

#[derive(Debug)]
pub enum Lexeme {
    Number(i64),
    Letter(u8),
    Header([u8; 8]),
    Quote(Box<String>),
    Eof,
}
impl Lexeme {
    pub fn as_nvariables(&self) -> Result<u32, &Lexeme> {
        match self {
            Lexeme::Number(num) if *num <= Variable::MaxValue => u32::try_from(*num).map_err(|_| self),
            _ => Err(self),
        }
    }
    pub fn as_nclauses(&self) -> Result<u64, &Lexeme> {
        match self {
            Lexeme::Number(num) => u64::try_from(*num).map_err(|_| self),
            _ => Err(self),
        }
    }
    pub fn as_literal(&self) -> Result<Literal, &Lexeme> {
        match self {
            Lexeme::Number(num) => Literal::try_from(*num).map_err(|_| self),
            Lexeme::Letter(b't') => Ok(Literal::Top),
            Lexeme::Letter(b'b') => Ok(Literal::Bottom),
            _ => Err(self),
        }
    }
    pub fn as_variable(&self) -> Result<Variable, &Lexeme> {
        match self {
            Lexeme::Number(num) => Variable::try_from(*num).map_err(|_| self),
            _ => Err(self),
        }
    }
    pub fn as_index(&self) -> Result<ClauseIndex, &Lexeme> {
        match self {
            Lexeme::Number(num) => ClauseIndex::try_from(*num).map_err(|_| self),
            _ => Err(self),
        }
    }
    pub fn as_file_position(&self) -> Result<FilePosition, &Lexeme> {
        match self {
            Lexeme::Number(num) => Ok(FilePosition::from(*num)),
            _ => Err(self),
        }
    }
    pub fn as_this_header(&self, hd: [u8; 8]) -> Result<(), &Lexeme> {
        match self {
            Lexeme::Header(h) if h == &hd => Ok(()),
            _ => Err(self),
        }
    }
    pub fn as_quote(&self) -> Result<&Box<String>, &Lexeme> {
        match self {
            Lexeme::Quote(bx) => Ok(bx),
            _ => Err(self),
        }
    }
}
impl Display for Lexeme {
	fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            Lexeme::Number(num) => write!(f, "number {}", num),
            Lexeme::Letter(c) => write!(f, "letter '{}'", *c as char),
            Lexeme::Header(hd) => {
                write!(f, "header '")?;
                for c in hd {
                    if c == &0u8 {
                        break;
                    }
                    write!(f, "{}", *c as char)?;
                }
                write!(f, "'")
            },
            Lexeme::Quote(bx) => write!(f, "string '{}'", bx),
            Lexeme::Eof => write!(f, "EOF"),
        }
	}
}

pub trait AsrLexer {
    fn consume(&mut self);
    fn peek(&self) -> &Lexeme;
    fn position(&self) -> FilePosition;
    fn path(&self) -> &Path;
}

pub struct UnbufferedAsrTextLexer<R: Read> {
    input: InputReader<R>,
    position: FilePosition,
    cache: Lexeme,
}
impl<R: Read> UnbufferedAsrTextLexer<R> {
    pub fn new(input: InputReader<R>) -> UnbufferedAsrTextLexer<R> {
        let pos = input.position();
        let mut lexer = UnbufferedAsrTextLexer::<R> {
            input: input,
            position: pos,
            cache: Lexeme::Eof,
        };
        lexer.consume();
        lexer
    }
    fn ignore(&mut self) -> Option<u8> {
        loop { match self.input.next() {
            Some(c) if (c as char).is_whitespace() => (),
            Some(b'c') => {
                while let Some(c) = self.input.next() {
                    if c == b'\n' {
                        break;
                    }
                }
            },
            x => {
                self.position = self.input.position();
                break x;
            }
        } }
    }
    fn read_number(&mut self, c: u8) {
        let (sign, mut num, mut found) = if c == b'-' {
            (-1i64, 0i64, false)
        } else {
            (1i64, (c - b'0') as i64, true)
        };
        while let Some(d) = self.input.next() {
            found = true;
            if (d as char).is_ascii_digit() {
                match num.checked_mul(10i64).and_then(|x| x.checked_add((d - b'0') as i64)) {
                    Some(res) => num = res,
                    None => self.out_of_range_integer(),
                }
            } else {
                found &= (d as char).is_whitespace();
                break;
            }
        }
        if !found {
            self.not_a_number();
        }
        match num.checked_mul(sign) {
            Some(res) => self.cache = Lexeme::Number(res),
            None => self.out_of_range_integer(),
        }
    }
    #[cold]
    fn read_header(&mut self) {
        let c = loop { match self.input.next() {
            Some(c) if (c as char).is_whitespace() => (),
            Some(c) if (c as char).is_ascii_alphabetic() => break c,
            _ => self.not_a_header(),
        } };
        let mut header = [c, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8];
        let mut count = 1usize;
        loop { match self.input.next() {
            Some(c) if (c as char).is_ascii_alphabetic() && count < 8usize => {
                header[count] = c;
                count += 1usize;
            },
            Some(c) if (c as char).is_whitespace() => break,
            None => break,
            Some(c) if (c as char).is_ascii_alphabetic() => self.out_of_range_header(),
            _ => self.not_a_header(),
        } }
        self.cache = Lexeme::Header(header)
    }
    fn read_letter(&mut self, c: u8) {
        match self.input.next() {
            Some(d) if (d as char).is_whitespace() => self.cache = Lexeme::Letter(c),
            None => self.cache = Lexeme::Letter(c),
            _ => self.not_a_letter(),
        }
    }
    #[cold]
    fn read_quote(&mut self) {
        let mut state: u32 = 3u32;
        let mut quote = String::new();
        loop {
            let i = self.input.next();
            match state {
                3u32 => match i {
                    Some(b'"') => break,
                    Some(b'\\') => state -= 1u32,
                    Some(c) => quote.push(c as char),
                    None => self.not_a_quote(),
                },
                2u32 => {
                    state = 3u32;
                    match i {
                        Some(b't') => quote.push('\t'),
                        Some(b'r') => quote.push('\r'),
                        Some(b'n') => quote.push('\n'),
                        Some(b'\'') => quote.push('\''),
                        Some(b'\"') => quote.push('\"'),
                        Some(b'\\') => quote.push('\\'),
                        Some(b'u') => state = 1u32,
                        s => self.invalid_escape(s),
                    }
                },
                1u32 => match i {
                    Some(b'{') => state -= 1u32,
                    _ => self.invalid_unicode_escape(None),
                },
                _ => match i {
                    Some(b'}') => {
                        if let Some(c) = char::from_u32(state >> 2) {
                            quote.push(c);
                        } else {
                            self.invalid_unicode_escape(Some(state >> 2));
                        }
                        state = 3u32;
                    }
                    Some(c) if (c as char).is_ascii_hexdigit() => {
                        if state > (u32::max_value() >> 4) {
                            self.invalid_unicode_escape(Some(state >> 2));
                        }
                        state <<= 4;
                        state |= (c as char).to_digit(16u32).unwrap() << 2;
                    },
                    _ => self.invalid_unicode_escape(Some(state >> 2)),
                },
            }
        }
        self.cache = Lexeme::Quote(Box::new(quote));
    }
    #[cold]
    #[inline(never)]
    fn invalid_input(&self, c: u8) -> ! {
        panick!("invalid input" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Could not recognize character '{}'.", c as char);
        })
    }
    #[cold]
    #[inline(never)]
    fn invalid_escape(&self, s: Option<u8>) -> ! {
        panick!("invalid escape" @ self.position.with_path(self.input.path()), lock, {
            match s {
                Some(c) => append!(lock, "Invalid escaped character '\\{}'.", c as char),
                None => append!(lock, "Unfinished escaped character."),
            }
        })
    }
    #[cold]
    #[inline(never)]
    fn invalid_unicode_escape(&self, s: Option<u32>) -> ! {
        panick!("invalid unicode escape" @ self.position.with_path(self.input.path()), lock, {
            match s {
                Some(c) => append!(lock, "Invalid escaped Unicode character with code {:#X}.", c),
                None => append!(lock, "Unfinished escaped Unicode character."),
            }
        })
    }
    #[cold]
    #[inline(never)]
    fn not_a_number(&self) -> ! {
        panick!("invalid integer" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Could not parse an integer.");
        })
    }
    #[cold]
    #[inline(never)]
    fn not_a_letter(&self) -> ! {
        panick!("invalid letter" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Could not parse a letter.");
        })
    }
    #[cold]
    #[inline(never)]
    fn not_a_header(&self) -> ! {
        panick!("invalid header" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Could not parse a 'p' header.");
        })
    }
    #[cold]
    #[inline(never)]
    fn not_a_quote(&self) -> ! {
        panick!("invalid quoted string" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Unclosed quoted string.");
        })
    }
    #[cold]
    #[inline(never)]
    fn out_of_range_integer(&self) -> ! {
        panick!("out-of-range integer" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Parsed a number outside of the 64-bit signed range [{}..{}].", i64::min_value(), i64::max_value());
        })
    }
    #[cold]
    #[inline(never)]
    fn out_of_range_header(&self) -> ! {
        panick!("out-of-range header" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Parsed a header longer than 8 characters.");
        })
    }
}
impl<R: Read> AsrLexer for UnbufferedAsrTextLexer<R> {
    #[inline]
    fn peek(&self) -> &Lexeme {
        &self.cache
    }
    fn consume(&mut self) {
        match self.ignore() {
            Some(c) if (c as char).is_ascii_digit() || c == b'-' => self.read_number(c),
            Some(b'p') => self.read_header(),
            Some(c) if (c as char).is_ascii_alphabetic() => self.read_letter(c),
            Some(b'"') => self.read_quote(),
            None => self.cache = Lexeme::Eof,
            Some(c) => self.invalid_input(c),
        }
    }
    #[inline]
    fn position(&self) -> FilePosition {
        self.position.clone()
    }
    #[inline]
    fn path(&self) -> &Path {
        self.input.path()
    }
}

pub struct BufferedAsrTextLexer<R: Read> {
    input: InputReader<R>,
    vec: Vec<(Lexeme, FilePosition)>,
    current: *const (Lexeme, FilePosition),
    end: *const (Lexeme, FilePosition),
    position: FilePosition,
}
impl<R: Read> BufferedAsrTextLexer<R> {
    const Length: usize = (1usize << 20);
    pub fn new(input: InputReader<R>) -> BufferedAsrTextLexer<R> {
        let pos = input.position();
        let mut lexer = BufferedAsrTextLexer::<R> {
            input: input,
            vec: Vec::<(Lexeme, FilePosition)>::with_capacity(BufferedAsrTextLexer::<R>::Length),
            current: ptr::null_mut(),
            end: ptr::null_mut(),
            position: pos,
        };
        lexer.buffer();
        lexer
    }
    fn buffer(&mut self) {
        self.vec.clear();
        while self.vec.len() < BufferedAsrTextLexer::<R>::Length {
            let lx = self.parse();
            if let Lexeme::Eof = lx {
                break;
            }
            self.vec.push((lx, self.position));
        }
        while self.vec.len() < BufferedAsrTextLexer::<R>::Length {
            self.vec.push((Lexeme::Eof, self.position));
        }
        self.current = self.vec.as_ptr();
        self.end = unsafe { self.current.add(self.vec.len()) };
    }
    fn ignore(&mut self) -> Option<u8> {
        loop { match self.input.next() {
            Some(c) if (c as char).is_whitespace() => (),
            Some(b'c') => {
                while let Some(c) = self.input.next() {
                    if c == b'\n' {
                        break;
                    }
                }
            },
            x => {
                self.position = self.input.position();
                break x;
            },
        } }
    }
    fn parse(&mut self) -> Lexeme {
        match self.ignore() {
            Some(c) if (c as char).is_ascii_digit() || c == b'-' => self.read_number(c),
            Some(b'p') => self.read_header(),
            Some(c) if (c as char).is_ascii_alphabetic() => self.read_letter(c),
            Some(b'"') => self.read_quote(),
            None => Lexeme::Eof,
            Some(c) => self.invalid_input(c),
        }
    }
    fn read_number(&mut self, c: u8) -> Lexeme {
        let (sign, mut num, mut found) = if c == b'-' {
            (-1i64, 0i64, false)
        } else {
            (1i64, (c - b'0') as i64, true)
        };
        while let Some(d) = self.input.next() {
            found = true;
            if (d as char).is_ascii_digit() {
                match num.checked_mul(10i64).and_then(|x| x.checked_add((d - b'0') as i64)) {
                    Some(res) => num = res,
                    None => self.out_of_range_integer(),
                }
            } else {
                found &= (d as char).is_whitespace();
                break;
            }
        }
        if !found {
            self.not_a_number();
        }
        match num.checked_mul(sign) {
            Some(res) => Lexeme::Number(res),
            None => self.out_of_range_integer(),
        }
    }
    #[cold]
    fn read_header(&mut self) -> Lexeme {
        let c = loop { match self.input.next() {
            Some(c) if (c as char).is_whitespace() => (),
            Some(c) if (c as char).is_ascii_alphabetic() => break c,
            _ => self.not_a_header(),
        } };
        let mut header = [c, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8];
        let mut count = 1usize;
        loop { match self.input.next() {
            Some(c) if (c as char).is_ascii_alphabetic() && count < 8usize => {
                header[count] = c;
                count += 1usize;
            },
            Some(c) if (c as char).is_whitespace() => break,
            None => break,
            Some(c) if (c as char).is_ascii_alphabetic() => self.out_of_range_header(),
            _ => self.not_a_header(),
        } }
        Lexeme::Header(header)
    }
    fn read_letter(&mut self, c: u8) -> Lexeme {
        match self.input.next() {
            Some(d) if (d as char).is_whitespace() => Lexeme::Letter(c),
            None => Lexeme::Letter(c),
            _ => self.not_a_letter(),
        }
    }
    #[cold]
    fn read_quote(&mut self) -> Lexeme {
        let mut state: u32 = 3u32;
        let mut quote = String::new();
        loop {
            let i = self.input.next();
            match state {
                3u32 => match i {
                    Some(b'"') => break,
                    Some(b'\\') => state -= 1u32,
                    Some(c) => quote.push(c as char),
                    None => self.not_a_quote(),
                },
                2u32 => {
                    state = 3u32;
                    match i {
                        Some(b't') => quote.push('\t'),
                        Some(b'r') => quote.push('\r'),
                        Some(b'n') => quote.push('\n'),
                        Some(b'\'') => quote.push('\''),
                        Some(b'\"') => quote.push('\"'),
                        Some(b'\\') => quote.push('\\'),
                        Some(b'u') => state = 1u32,
                        s => self.invalid_escape(s),
                    }
                },
                1u32 => match i {
                    Some(b'{') => state -= 1u32,
                    _ => self.invalid_unicode_escape(None),
                },
                _ => match i {
                    Some(b'}') => {
                        if let Some(c) = char::from_u32(state >> 2) {
                            quote.push(c);
                        } else {
                            self.invalid_unicode_escape(Some(state >> 2));
                        }
                        state = 3u32;
                    }
                    Some(c) if (c as char).is_ascii_hexdigit() => {
                        if state > (u32::max_value() >> 4) {
                            self.invalid_unicode_escape(Some(state >> 2));
                        }
                        state <<= 4;
                        state |= (c as char).to_digit(16u32).unwrap() << 2;
                    },
                    _ => self.invalid_unicode_escape(Some(state >> 2)),
                },
            }
        }
        Lexeme::Quote(Box::new(quote))
    }
    #[cold]
    #[inline(never)]
    fn invalid_input(&self, c: u8) -> ! {
        panick!("invalid input" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Could not recognize character '{}'.", c as char);
        })
    }
    #[cold]
    #[inline(never)]
    fn invalid_escape(&self, s: Option<u8>) -> ! {
        panick!("invalid escape" @ self.position.with_path(self.input.path()), lock, {
            match s {
                Some(c) => append!(lock, "Invalid escaped character '\\{}'.", c as char),
                None => append!(lock, "Unfinished escaped character."),
            }
        })
    }
    #[cold]
    #[inline(never)]
    fn invalid_unicode_escape(&self, s: Option<u32>) -> ! {
        panick!("invalid unicode escape" @ self.position.with_path(self.input.path()), lock, {
            match s {
                Some(c) => append!(lock, "Invalid escaped Unicode character with code {:#X}.", c),
                None => append!(lock, "Unfinished escaped Unicode character."),
            }
        })
    }
    #[cold]
    #[inline(never)]
    fn not_a_number(&self) -> ! {
        panick!("invalid integer" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Could not parse an integer.");
        })
    }
    #[cold]
    #[inline(never)]
    fn not_a_letter(&self) -> ! {
        panick!("invalid letter" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Could not parse a letter.");
        })
    }
    #[cold]
    #[inline(never)]
    fn not_a_header(&self) -> ! {
        panick!("invalid header" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Could not parse a 'p' header.");
        })
    }
    #[cold]
    #[inline(never)]
    fn not_a_quote(&self) -> ! {
        panick!("invalid quoted string" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Unclosed quoted string.");
        })
    }
    #[cold]
    #[inline(never)]
    fn out_of_range_integer(&self) -> ! {
        panick!("out-of-range integer" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Parsed a number outside of the 64-bit signed range [{}..{}].", i64::min_value(), i64::max_value());
        })
    }
    #[cold]
    #[inline(never)]
    fn out_of_range_header(&self) -> ! {
        panick!("out-of-range header" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Parsed a header longer than 8 characters.");
        })
    }
}
impl<R: Read> AsrLexer for BufferedAsrTextLexer<R> {
    #[inline]
    fn peek(&self) -> &Lexeme {
        unsafe { &(*self.current).0 }
    }
    #[inline]
    fn consume(&mut self) {
        self.current = unsafe { self.current.add(1usize) };
        if self.current == self.end {
            self.buffer();
        }
    }
    #[inline]
    fn position(&self) -> FilePosition {
        unsafe { (*self.current).1.clone() }
    }
    #[inline]
    fn path(&self) -> &Path {
        self.input.path()
    }
}

pub struct UnbufferedAsrBinaryLexer<R: Read> {
    input: InputReader<R>,
    position: FilePosition,
    cache: Lexeme,
}
impl<R: Read> UnbufferedAsrBinaryLexer<R> {
    pub fn new(mut input: InputReader<R>) -> UnbufferedAsrBinaryLexer<R> {
        input.next();
        let pos = input.position();
        let mut lexer = UnbufferedAsrBinaryLexer::<R> {
            input: input,
            position: pos,
            cache: Lexeme::Eof,
        };
        lexer.consume();
        lexer
    }
    fn read_number(&mut self, c: u8) {
        let mut unum: u64 = ((c & 0b0111_1111u8) >> 1) as u64;
        let inum: i64 = if c & 0b0000_0001u8 == 0u8 {
            1i64
        } else {
            -1i64
        };
        let mut cont: bool = c & 0b1000_0000u8 != 0u8;
        let mut shift: u8 = 6u8;
        while cont {
            if shift >= 62u8 {
                self.out_of_range_integer();
            } else if let Some(cc) = self.input.next() {
                unum |= ((cc & 0b0111_1111u8) as u64) << shift;
                shift += 7u8;
                cont = cc & 0b1000_0000u8 != 0u8;
            } else {
                self.not_a_number();
            }
        }
        self.cache = Lexeme::Number(inum * (unum as i64));
    }
    fn read_other(&mut self) {
        match self.input.next() {
            Some(b'p') => self.read_header(),
            Some(c) if (c as char).is_ascii_alphabetic() => self.cache = Lexeme::Letter(c),
            Some(b'"') => self.read_quote(),
            Some(_) | None => self.not_a_letter(),
        }
    }
    #[cold]
    fn read_header(&mut self) {
        let mut header = [0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8];
        let mut count = 0usize;
        loop { match self.input.next() {
            Some(c) if (c as char).is_ascii_alphabetic() && count < 8usize => {
                header[count] = c;
                count += 1usize;
            },
            Some(0u8) | None => break,
            Some(c) if (c as char).is_ascii_alphabetic() => self.out_of_range_header(),
            _ => self.not_a_header(),
        } }
        self.cache = Lexeme::Header(header);
    }
    #[cold]
    fn read_quote(&mut self) {
        let mut state: u32 = 3u32;
        let mut quote = String::new();
        loop {
            let i = self.input.next();
            match state {
                3u32 => match i {
                    Some(b'"') => break,
                    Some(b'\\') => state -= 1u32,
                    Some(c) => quote.push(c as char),
                    None => self.not_a_quote(),
                },
                2u32 => {
                    state = 3u32;
                    match i {
                        Some(b't') => quote.push('\t'),
                        Some(b'r') => quote.push('\r'),
                        Some(b'n') => quote.push('\n'),
                        Some(b'\'') => quote.push('\''),
                        Some(b'\"') => quote.push('\"'),
                        Some(b'\\') => quote.push('\\'),
                        Some(b'u') => state = 1u32,
                        s => self.invalid_escape(s),
                    }
                },
                1u32 => match i {
                    Some(b'{') => state -= 1u32,
                    _ => self.invalid_unicode_escape(None),
                },
                _ => match i {
                    Some(b'}') => {
                        if let Some(c) = char::from_u32(state >> 2) {
                            quote.push(c);
                        } else {
                            self.invalid_unicode_escape(Some(state >> 2));
                        }
                        state = 3u32;
                    }
                    Some(c) if (c as char).is_ascii_hexdigit() => {
                        if state > (u32::max_value() >> 4) {
                            self.invalid_unicode_escape(Some(state >> 2));
                        }
                        state <<= 4;
                        state |= (c as char).to_digit(16u32).unwrap() << 2;
                    },
                    _ => self.invalid_unicode_escape(Some(state >> 2)),
                },
            }
        }
        self.cache = Lexeme::Quote(Box::new(quote))
    }
    #[cold]
    #[inline(never)]
    fn invalid_escape(&self, s: Option<u8>) -> ! {
        panick!("invalid escape" @ self.position.with_path(self.input.path()), lock, {
            match s {
                Some(c) => append!(lock, "Invalid escaped character '\\{}'.", c as char),
                None => append!(lock, "Unfinished escaped character."),
            }
        })
    }
    #[cold]
    #[inline(never)]
    fn invalid_unicode_escape(&self, s: Option<u32>) -> ! {
        panick!("invalid unicode escape" @ self.position.with_path(self.input.path()), lock, {
            match s {
                Some(c) => append!(lock, "Invalid escaped Unicode character with code {:#X}.", c),
                None => append!(lock, "Unfinished escaped Unicode character."),
            }
        })
    }
    #[cold]
    #[inline(never)]
    fn not_a_number(&self) -> ! {
        panick!("invalid integer" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Could not parse an integer.");
        })
    }
    #[cold]
    #[inline(never)]
    fn not_a_letter(&self) -> ! {
        panick!("invalid letter" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Could not parse a letter.");
        })
    }
    #[cold]
    #[inline(never)]
    fn not_a_header(&self) -> ! {
        panick!("invalid header" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Could not parse a header.");
        })
    }
    #[cold]
    #[inline(never)]
    fn not_a_quote(&self) -> ! {
        panick!("invalid quoted string" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Unclosed quoted string.");
        })
    }
    #[cold]
    #[inline(never)]
    fn out_of_range_integer(&self) -> ! {
        panick!("out-of-range integer" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Parsed a number outside of the 64-bit signed range [{}..{}].",
                -(((1u64 << 63) - 1u64) as i64), ((1u64 << 63) - 1u64) as i64);
        })
    }
    #[cold]
    #[inline(never)]
    fn out_of_range_header(&self) -> ! {
        panick!("out-of-range header" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Parsed a header longer than 8 characters.");
        })
    }
}
impl<R: Read> AsrLexer for UnbufferedAsrBinaryLexer<R> {
    #[inline]
    fn peek(&self) -> &Lexeme {
        &self.cache
    }
    fn consume(&mut self) {
        match self.input.next() {
            Some(1u8) => self.read_other(),
            Some(c) => self.read_number(c),
            None => self.cache = Lexeme::Eof,
        }
    }
    #[inline]
    fn position(&self) -> FilePosition {
        self.position.clone()
    }
    #[inline]
    fn path(&self) -> &Path {
        self.input.path()
    }
}