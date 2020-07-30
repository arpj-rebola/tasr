use std::{
    convert::{TryFrom},
    error::{Error},
    fmt::{self, Formatter, Display},
    mem::{self},
    path::{Path},
};

use crate::{
    basic::{CnfHeaderStats, ClauseIndex},
    input::{SourcePosition, InputStream, FilePositionTracker, Positioned},
    variable::{Variable, Literal, MaybeVariable},
};

#[derive(Debug)]
pub struct LexingError {
    pos: SourcePosition,
    format: String,
}

#[derive(Debug)]
pub struct ExpectedField {
    field: String,
    pos: SourcePosition,
    format: String,
}

#[derive(Debug)]
pub struct OutOfRangeField {
    field: String,
    found: String,
    range: String,
    pos: SourcePosition,
    format: String,
}

#[derive(Debug)]
pub enum ParsingError {
    InvalidCharacter(Box<LexingError>),
    InvalidNumber(Box<LexingError>),
    InvalidLetter(Box<LexingError>),
    InvalidHeader(Box<LexingError>),
    InvalidVbeNumber(Box<LexingError>),
    InvalidVbeHeader(Box<LexingError>),
    OutOfRangeI64(Box<LexingError>),
    OutOfRangeU64(Box<LexingError>),
    OutOfRangeHeader(Box<LexingError>),
    ExpectedField(Box<ExpectedField>),
    OutOfRangeField(Box<OutOfRangeField>),
}
impl Display for ParsingError {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
            ParsingError::InvalidCharacter(bx) => write!(f, "Invalid character in {} file {}.", &bx.format, &bx.pos),
            ParsingError::InvalidNumber(bx) => write!(f, "Invalid number in {} file {}.", &bx.format, &bx.pos),
            ParsingError::InvalidLetter(bx) => write!(f, "Invalid letter in {} file {}.", &bx.format, &bx.pos),
            ParsingError::InvalidHeader(bx) => write!(f, "Invalid header in {} file {}.", &bx.format, &bx.pos),
            ParsingError::InvalidVbeNumber(bx) => write!(f, "Invalid variable-bit encoded integer in {} file {}:\nEOF found before terminating character.", &bx.format, &bx.pos),
            ParsingError::InvalidVbeHeader(bx) => write!(f, "Invalid variable-bit encoded header in {} file {}:\nEOF found within 8 header characters.", &bx.format, &bx.pos),
            ParsingError::OutOfRangeI64(bx) => write!(f, "Out of range number in {} file {}:\nNumber does not fit into an i64 integer.", &bx.format, &bx.pos),
            ParsingError::OutOfRangeU64(bx) => write!(f, "Out of range number in {} file {}:\nNumber does not fit into an u64 integer.", &bx.format, &bx.pos),
            ParsingError::OutOfRangeHeader(bx) => write!(f, "Out of range header in {} file {}:\nHeader exceeds the 8-character limit.", &bx.format, &bx.pos),
            ParsingError::ExpectedField(bx) => write!(f, "Unexpected input in {} file {}:\nExpected {}.", &bx.format, &bx.pos, &bx.field),
            ParsingError::OutOfRangeField(bx) => write!(f, "Out of range input in {} file {}:\nExpected {}, found {} but the admissible range is {}.", &bx.format, &bx.pos, &bx.field, &bx.found, &bx.range),
        }
    }
}
impl Error for ParsingError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
		None
    }
}

pub enum AsrInstructionKind {
    Rup(ClauseIndex),
    Wsr(ClauseIndex),
    Del
}

pub trait AsrParser {
    fn skip_to_header(&mut self) -> Positioned<Option<[u8; 8]>>;
    fn parse_cnf_header(&mut self) -> CnfHeaderStats;
    fn parse_formula(&mut self) -> Positioned<Option<()>>;
    fn parse_core(&mut self) -> Positioned<Option<ClauseIndex>>;
    fn parse_proof(&mut self) -> Positioned<Option<AsrInstructionKind>>;
    fn parse_clause(&mut self) -> Option<Literal>;
    fn parse_witness(&mut self) -> Option<(Variable, Literal)>;
    fn parse_chain(&mut self) -> Option<ClauseIndex>;
    fn name(&self) -> &Path;
}

#[derive(Debug, PartialEq)]
enum DimacsLexeme {
    Header([u8; 8]),
    Letter(u8),
    Number(i64),
}

pub struct DimacsParser<'a> {
    input: InputStream<'a>,
    format: &'static str,
    cache: Option<DimacsLexeme>,
    pos: FilePositionTracker<'a>,
}
impl<'a> DimacsParser<'a> {
    pub fn new(is: InputStream<'a>, format: &'static str) -> DimacsParser<'a> {
        let pos = FilePositionTracker::<'a>::new(is.name());
        let mut parser = DimacsParser::<'a> {
            input: is,
            format: format,
            cache: None,
            pos: pos,
        };
        parser.cache = parser.read();
        parser
    }
    fn peek(&mut self) -> &Option<DimacsLexeme> {
        &self.cache
    }
    fn next(&mut self) -> Option<DimacsLexeme> {
        let mut read = self.read();
        mem::swap(&mut self.cache, &mut read);
        read
    }
    fn read(&mut self) -> Option<DimacsLexeme> {
        match self.ignore() {
            Some(c) if (c as char).is_numeric() => self.read_number(c, true),
            Some(c) if c == b'-' => self.read_negative_number(),
            Some(c) if (c as char).is_alphabetic() => self.read_letter(c),
            None => None,
            _ => panic!(self.error_invalid_character()),
        }
    }
    fn read_negative_number(&mut self) -> Option<DimacsLexeme> {
        match self.input.next() {
            Some(c) if (c as char).is_numeric() => self.read_number(c, false),
            _ => panic!(self.error_invalid_number()),
        }
    }
    fn read_number(&mut self, initial: u8, sign: bool) -> Option<DimacsLexeme> {
        let factor = if sign {
            1i64
        } else {
            -1i64
        };
        let mut num: i64 = factor * ((initial - b'0') as i64);
        loop { match self.input.next() {
            Some(c) if (c as char).is_numeric() => {
                match num.checked_mul(10i64).and_then(|x| x.checked_add(factor * ((c - b'0') as i64))) {
                    Some(x) => num = x,
                    None => panic!(self.error_out_of_range_i64()),
                }
            },
            Some(c) if (c as char).is_whitespace() => break Some(DimacsLexeme::Number(num)),
            None => break Some(DimacsLexeme::Number(num)),
            _ => panic!(self.error_invalid_number()),
        } }
    }
    fn read_letter(&mut self, letter: u8) -> Option<DimacsLexeme> {
        if letter == b'p' {
            self.read_header()
        } else {
            match self.input.next() {
                Some(c) if (c as char).is_whitespace() => Some(DimacsLexeme::Letter(letter)),
                None => Some(DimacsLexeme::Letter(letter)),
                _ => panic!(self.error_invalid_letter()),
            }
        }
    }
    fn read_header(&mut self) -> Option<DimacsLexeme> {
        let mut hd = [0u8; 8usize];
        hd[0] = loop { match self.input.next() {
            Some(c) if (c as char).is_whitespace() => (),
            None => panic!(self.error_invalid_header()),
            Some(c) => break c,
        } };
        for i in 0usize..8usize {
            match self.input.next() {
                Some(c) if (c as char).is_alphabetic() => if i < 7usize {
                    hd[i + 1] = c;
                } else {
                    panic!(self.error_out_of_range_header());
                },
                Some(c) if (c as char).is_whitespace() => break,
                None => break,
                _ => panic!(self.error_invalid_header()),
            }
        }
        Some(DimacsLexeme::Header(hd))
    }
    fn ignore(&mut self) -> Option<u8> {
        loop { match self.input.next() {
            Some(c) if c == b'c' => self.skip_comment(),
            Some(c) if (c as char).is_whitespace() => (),
            x => {
                let pos = self.input.position();
                self.pos.push(pos);
                break x;
            },
        } }
    }
    fn skip_comment(&mut self) {
        loop { match self.input.next() {
            Some(c) if c == b'\n' => break,
            None => break,
            _ => (),
        } }
    }
    fn error_invalid_character(&self) -> ParsingError {
        ParsingError::InvalidCharacter(Box::new(LexingError {
            pos: self.input.position().source(),
            format: self.format.to_string(),
        }))
    }
    fn error_invalid_number(&self) -> ParsingError {
        ParsingError::InvalidNumber(Box::new(LexingError {
            pos: self.input.position().source(),
            format: self.format.to_string(),
        }))
    }
    fn error_out_of_range_i64(&self) -> ParsingError {
        ParsingError::OutOfRangeI64(Box::new(LexingError {
            pos: self.input.position().source(),
            format: self.format.to_string(),
        }))
    }
    fn error_invalid_letter(&self) -> ParsingError {
        ParsingError::InvalidLetter(Box::new(LexingError {
            pos: self.input.position().source(),
            format: self.format.to_string(),
        }))
    }
    fn error_invalid_header(&self) -> ParsingError {
        ParsingError::InvalidHeader(Box::new(LexingError {
            pos: self.input.position().source(),
            format: self.format.to_string(),
        }))
    }
    fn error_out_of_range_header(&self) -> ParsingError {
        ParsingError::OutOfRangeHeader(Box::new(LexingError {
            pos: self.input.position().source(),
            format: self.format.to_string(),
        }))
    }
    fn error_expected_field(&self, field: &str) -> ParsingError {
        ParsingError::ExpectedField(Box::new(ExpectedField {
            field: field.to_string(),
            pos: self.input.position().source(),
            format: self.format.to_string(),
        }))
    }
    fn error_out_of_range_id(&self, num: i64) -> ParsingError {
        ParsingError::OutOfRangeField(Box::new(OutOfRangeField {
            found: format!("{}", num),
            range: format!("[{} .. {}]", 1i64, ClauseIndex::MaxValue),
            field: "clause identifier".to_string(),
            pos: self.input.position().source(),
            format: self.format.to_string(),
        }))
    }
    fn error_out_of_range_literal(&self, num: i64) -> ParsingError {
        ParsingError::OutOfRangeField(Box::new(OutOfRangeField {
            found: format!("{}", num),
            range: format!("[{} .. {}] U [{} .. {}]", -Literal::MaxValue, -1i64, 1i64, Literal::MaxValue),
            field: "literal".to_string(),
            pos: self.input.position().source(),
            format: self.format.to_string(),
        }))
    }
    fn error_out_of_range_variable(&self, num: i64) -> ParsingError {
        ParsingError::OutOfRangeField(Box::new(OutOfRangeField {
            found: format!("{}", num),
            range: format!("[{} .. {}]", 1i64, Variable::MaxValue),
            field: "variable".to_string(),
            pos: self.input.position().source(),
            format: self.format.to_string(),
        }))
    }
    fn error_out_of_range_num_variables(&self, num: i64) -> ParsingError {
        ParsingError::OutOfRangeField(Box::new(OutOfRangeField {
            found: format!("{}", num),
            range: format!("[{} .. {}]", 0i64, Variable::MaxValue),
            field: "number of variables".to_string(),
            pos: self.input.position().source(),
            format: self.format.to_string(),
        }))
    }
    fn error_out_of_range_num_clauses(&self, num: i64) -> ParsingError {
        let max = i64::try_from(usize::max_value()).unwrap_or(i64::max_value());
        ParsingError::OutOfRangeField(Box::new(OutOfRangeField {
            found: format!("{}", num),
            range: format!("[{} .. {}]", 1i64, max),
            field: "number of clauses".to_string(),
            pos: self.input.position().source(),
            format: self.format.to_string(),
        }))
    }
}
impl<'a> AsrParser for DimacsParser<'a> {
    fn skip_to_header(&mut self) -> Positioned<Option<[u8; 8]>> {
        let res = loop { match self.next() {
            Some(DimacsLexeme::Header(hd)) => break Some(hd),
            None => break None,
            _ => (),
        } };
        Positioned(res, self.pos.left())
    }
    fn parse_cnf_header(&mut self) -> CnfHeaderStats {
        let vars = match self.next() {
            Some(DimacsLexeme::Number(num)) => match MaybeVariable::try_from(num) {
                Ok(var) => var,
                Err(num) => panic!(self.error_out_of_range_num_variables(num)),
            },
            _ => panic!(self.error_expected_field("number of variables")),
        };
        let cls = match self.next() {
            Some(DimacsLexeme::Number(num)) => match usize::try_from(num) {
                Ok(cls) => cls,
                Err(_) => panic!(self.error_out_of_range_num_clauses(num)),
            },
            _ => panic!(self.error_expected_field("number of clauses")),
        };
        CnfHeaderStats {
            variables: vars,
            clauses: cls,
        }
    }
    fn parse_formula(&mut self) -> Positioned<Option<()>> {
        let res = match self.peek() {
            Some(DimacsLexeme::Number(_)) => Some(()),
            None => None,
            _ => panic!(self.error_expected_field("clause literal, or EOF")),
        };
        Positioned(res, self.pos.right())
    }
    fn parse_core(&mut self) -> Positioned<Option<ClauseIndex>> {
        if let &Some(DimacsLexeme::Header(_)) = self.peek() {
            Positioned(None, self.pos.right())
        } else {
            match self.next() {
                Some(DimacsLexeme::Letter(b'k')) => {
                    let pos = self.pos.left();
                    match self.next() {
                        Some(DimacsLexeme::Number(num)) => match ClauseIndex::try_from(num) {
                            Ok(id) => Positioned(Some(id), pos),
                            Err(num) => panic!(self.error_out_of_range_id(num)),
                        },
                        _ => panic!(self.error_expected_field("clause identifier")),
                    }
                },
                None => Positioned(None, self.pos.left()),
                _ => panic!(self.error_expected_field("core marker 'k', or EOF")),
            }
        }
    }
    fn parse_proof(&mut self) -> Positioned<Option<AsrInstructionKind>> {
        if let &Some(DimacsLexeme::Header(_)) = self.peek() {
            Positioned(None, self.pos.right())
        } else {
            match self.next() {
                Some(DimacsLexeme::Letter(b'r')) => {
                    let pos = self.pos.left();
                    match self.next() {
                        Some(DimacsLexeme::Number(num)) => match ClauseIndex::try_from(num) {
                            Ok(id) => Positioned(Some(AsrInstructionKind::Rup(id)), pos),
                            Err(num) => panic!(self.error_out_of_range_id(num)),
                        },
                        _ => panic!(self.error_expected_field("clause identifier")),
                    }
                },
                Some(DimacsLexeme::Letter(b's')) => {
                    let pos = self.pos.left();
                    match self.next() {
                        Some(DimacsLexeme::Number(num)) => match ClauseIndex::try_from(num) {
                            Ok(id) => Positioned(Some(AsrInstructionKind::Wsr(id)), pos),
                            Err(num) => panic!(self.error_out_of_range_id(num)),
                        },
                        _ => panic!(self.error_expected_field("clause identifier")),
                    }
                },
                Some(DimacsLexeme::Letter(b'd')) => Positioned(Some(AsrInstructionKind::Del), self.pos.left()),
                None => Positioned(None, self.pos.left()),
                _ => panic!(self.error_expected_field("instruction marker 'r', 's', 'd', or EOF"))
            }
        }
    }
    fn parse_clause(&mut self) -> Option<Literal> {
        match self.next() {
            Some(DimacsLexeme::Number(num)) => match Literal::try_from(num) {
                Ok(lit) => Some(lit),
                Err(0i64) => None,
                Err(num) => panic!(self.error_out_of_range_literal(num)),
            },
            _ => panic!(self.error_expected_field("literal, or end-of-clause zero")),
        }
    }
    fn parse_witness(&mut self) -> Option<(Variable, Literal)> {
        let var = match self.next() {
            Some(DimacsLexeme::Number(num)) => match Variable::try_from(num) {
                Ok(var) => var,
                Err(0i64) => return None,
                Err(num) => panic!(self.error_out_of_range_variable(num)),
            },
            _ => panic!(self.error_expected_field("variable, or end-of-witness zero")),
        };
        let lit = match self.next() {
            Some(DimacsLexeme::Number(num)) => match Literal::try_from(num) {
                Ok(lit) => lit,
                Err(num) => panic!(self.error_out_of_range_literal(num)),
            },
            Some(DimacsLexeme::Letter(b't')) => Literal::Top,
            Some(DimacsLexeme::Letter(b'f')) => Literal::Bottom,
            _ => panic!(self.error_expected_field("literal, top 't', or bottom 'b'")),
        };
        Some((var, lit))
    }
    fn parse_chain(&mut self) -> Option<ClauseIndex> {
        match self.next() {
            Some(DimacsLexeme::Number(num)) => match ClauseIndex::try_from(num) {
                Ok(lit) => Some(lit),
                Err(0i64) => None,
                Err(num) => panic!(self.error_out_of_range_id(num)),
            },
            _ => panic!(self.error_expected_field("clause identifier, or end-of-chain zero")),
        }
    }
    fn name(&self) -> &Path {
        &self.pos.name()
    }
}

#[derive(Debug, PartialEq)]
enum VbeLexeme {
    Header([u8; 8]),
    Number(u64),
}
pub struct VbeParser<'a> {
    input: InputStream<'a>,
    format: &'static str,
    cache: Option<VbeLexeme>,
    pos: FilePositionTracker<'a>,
}
impl<'a> VbeParser<'a> {
    const LowercaseK: u64 = b'k' as u64;
    const LowercaseR: u64 = b'r' as u64;
    const LowercaseS: u64 = b's' as u64;
    const LowercaseD: u64 = b'd' as u64;
    pub fn new(is: InputStream<'a>, format: &'static str) -> VbeParser<'a> {
        let pos = FilePositionTracker::<'a>::new(is.name());
        let mut parser = VbeParser::<'a> {
            input: is,
            format: format,
            cache: None,
            pos: pos,
        };
        parser.cache = parser.read_number();
        parser
    }
    fn peek(&mut self) -> &Option<VbeLexeme> {
        &self.cache
    }
    fn next(&mut self) -> Option<VbeLexeme> {
        let mut read = self.read_number();
        mem::swap(&mut self.cache, &mut read);
        read
    }
    fn read_number(&mut self) -> Option<VbeLexeme> {
        let mut num: u64 = 0u64;
        let mut shift: u8 = 0u8;
        loop { 
            if shift == 0u8 {
                let pos = self.input.position();
                self.pos.push(pos);
            }
            match self.input.next() {
                Some(c) => {
                    if shift < 63u8 || (shift == 63u8 && (c & 0b0111_1111u8) <= 1u8) || c & 0b0111_1111u8 == 0u8 {
                        if num == 0u64 && shift != 0u8 && c == 0u8 {
                            break self.read_header()
                        }
                        if c & 0b0111_1111u8 != 0u8 {
                            num |= ((c & 0b0111_1111u8) as u64) << shift;
                        }
                        shift += 7u8;
                        if c < 0b1000_0000u8 {
                            break Some(VbeLexeme::Number(num))
                        }
                    } else {
                        panic!(self.error_out_of_range_number())
                    }
                },
                None => if shift == 0u8 {
                    break None
                } else {
                    panic!(self.error_invalid_vbe_number())
                },
            }
        }
    }
    fn read_header(&mut self) -> Option<VbeLexeme> {
        let mut hd = [0u8; 8];
        for i in 0usize..8usize {
            match self.input.next() {
                Some(c) => hd[i] = c,
                None => panic!(self.error_invalid_vbe_header()),
            }
        }
        Some(VbeLexeme::Header(hd))
    }
    fn error_out_of_range_number(&self) -> ParsingError {
        ParsingError::OutOfRangeU64(Box::new(LexingError {
            pos: self.input.position().source(),
            format: self.format.to_string(),
        }))
    }
    fn error_invalid_vbe_number(&self) -> ParsingError {
        ParsingError::InvalidVbeNumber(Box::new(LexingError {
            pos: self.input.position().source(),
            format: self.format.to_string(),
        }))
    }
    fn error_invalid_vbe_header(&self) -> ParsingError {
        ParsingError::InvalidVbeHeader(Box::new(LexingError {
            pos: self.input.position().source(),
            format: self.format.to_string(),
        }))
    }
    fn error_expected_field(&self, field: &str) -> ParsingError {
        ParsingError::ExpectedField(Box::new(ExpectedField {
            field: field.to_string(),
            pos: self.input.position().source(),
            format: self.format.to_string(),
        }))
    }
    fn error_out_of_range_id(&self, num: u64) -> ParsingError {
        ParsingError::OutOfRangeField(Box::new(OutOfRangeField {
            found: format!("{}", num),
            range: format!("[{} .. {}]", 1i64, ClauseIndex::MaxValue),
            field: "clause identifier".to_string(),
            pos: self.input.position().source(),
            format: self.format.to_string(),
        }))
    }
    fn error_out_of_range_literal(&self, num: u64) -> ParsingError {
        ParsingError::OutOfRangeField(Box::new(OutOfRangeField {
            found: format!("{}", num),
            range: format!("[{} .. {}]", 2u64, u32::max_value()),
            field: "literal".to_string(),
            pos: self.input.position().source(),
            format: self.format.to_string(),
        }))
    }
    fn error_out_of_range_atom(&self, num: u64) -> ParsingError {
        ParsingError::OutOfRangeField(Box::new(OutOfRangeField {
            found: format!("{}", num),
            range: format!("[{} .. {}]", 0u64, u32::max_value()),
            field: "literal, top or bottom".to_string(),
            pos: self.input.position().source(),
            format: self.format.to_string(),
        }))
    }
    fn error_out_of_range_variable(&self, num: u64) -> ParsingError {
        ParsingError::OutOfRangeField(Box::new(OutOfRangeField {
            found: format!("{}", num),
            range: format!("[{} .. {}]", 1i64, Variable::MaxValue),
            field: "variable".to_string(),
            pos: self.input.position().source(),
            format: self.format.to_string(),
        }))
    }
    fn error_out_of_range_num_variables(&self, num: u64) -> ParsingError {
        ParsingError::OutOfRangeField(Box::new(OutOfRangeField {
            found: format!("{}", num),
            range: format!("[{} .. {}]", 0i64, Variable::MaxValue),
            field: "number of variables".to_string(),
            pos: self.input.position().source(),
            format: self.format.to_string(),
        }))
    }
    fn error_out_of_range_num_clauses(&self, num: u64) -> ParsingError {
        let max = u64::try_from(usize::max_value()).unwrap_or(u64::max_value());
        ParsingError::OutOfRangeField(Box::new(OutOfRangeField {
            found: format!("{}", num),
            range: format!("[{} .. {}]", 1i64, max),
            field: "number of clauses".to_string(),
            pos: self.input.position().source(),
            format: self.format.to_string(),
        }))
    }
}
impl<'a> AsrParser for VbeParser<'a> {
    fn skip_to_header(&mut self) -> Positioned<Option<[u8; 8]>> {
        let res = loop { match self.next() {
            Some(VbeLexeme::Header(hd)) => break Some(hd),
            None => break None,
            _ => (),
        } };
        Positioned(res, self.pos.left())
    }
    fn parse_cnf_header(&mut self) -> CnfHeaderStats {
        let vars = match self.next() {
            Some(VbeLexeme::Number(num)) => match MaybeVariable::try_from(num) {
                Ok(var) => var,
                Err(num) => panic!(self.error_out_of_range_num_variables(num)),
            },
            _ => panic!(self.error_expected_field("number of variables")),
        };
        let clauses = match self.next() {
            Some(VbeLexeme::Number(num)) => match usize::try_from(num) {
                Ok(cls) => cls,
                Err(_) => panic!(self.error_out_of_range_num_clauses(num)),
            },
            _ => panic!(self.error_expected_field("number of clauses")),
        };
        CnfHeaderStats {
            variables: vars,
            clauses: clauses,
        }
    }
    fn parse_formula(&mut self) -> Positioned<Option<()>> {
        let res = match self.peek() {
            Some(VbeLexeme::Number(_)) => Some(()),
            None => None,
            _ => panic!(self.error_expected_field("clause literal, or EOF")),
        };
        Positioned(res, self.pos.right())
    }
    fn parse_core(&mut self) -> Positioned<Option<ClauseIndex>> {
        if let &Some(VbeLexeme::Header(_)) = self.peek() {
            Positioned(None, self.pos.right())
        } else {
            match self.next() {
                Some(VbeLexeme::Number(VbeParser::<'a>::LowercaseK)) => {
                    let pos = self.pos.left();
                    match self.next() {
                        Some(VbeLexeme::Number(num)) => match ClauseIndex::try_from(num) {
                            Ok(id) => Positioned(Some(id), pos),
                            Err(num) => panic!(self.error_out_of_range_id(num)),
                        },
                        _ => panic!(self.error_expected_field("clause identifier")),
                    }
                },
                None => Positioned(None, self.pos.left()),
                _ => panic!(self.error_expected_field("core marker '0x6B', or EOF")),
            }
        }
    }
    fn parse_proof(&mut self) -> Positioned<Option<AsrInstructionKind>> {
        if let &Some(VbeLexeme::Header(_)) = self.peek() {
            Positioned(None, self.pos.right())
        } else {
            match self.next() {
                Some(VbeLexeme::Number(VbeParser::<'a>::LowercaseR)) => {
                    let pos = self.pos.left();
                    match self.next() {
                        Some(VbeLexeme::Number(num)) => match ClauseIndex::try_from(num) {
                            Ok(id) => Positioned(Some(AsrInstructionKind::Rup(id)), pos),
                            Err(num) => panic!(self.error_out_of_range_id(num)),
                        },
                        _ => panic!(self.error_expected_field("clause identifier")),
                    }
                },
                Some(VbeLexeme::Number(VbeParser::<'a>::LowercaseS)) => {
                    let pos = self.pos.left();
                    match self.next() {
                        Some(VbeLexeme::Number(num)) => match ClauseIndex::try_from(num) {
                            Ok(id) => Positioned(Some(AsrInstructionKind::Wsr(id)), pos),
                            Err(num) => panic!(self.error_out_of_range_id(num)),
                        },
                        _ => panic!(self.error_expected_field("clause identifier")),
                    }
                },
                Some(VbeLexeme::Number(VbeParser::<'a>::LowercaseD)) => Positioned(Some(AsrInstructionKind::Del), self.pos.left()),
                None => Positioned(None, self.pos.left()),
                _ => panic!(self.error_expected_field("instruction marker '0x72', '0x73', '0x64', or EOF"))
            }
        }
    }
    fn parse_clause(&mut self) -> Option<Literal> {
        match self.next() {
            Some(VbeLexeme::Number(num)) => match Literal::try_from(num) {
                Ok(lit) => Some(lit),
                Err(0u64) => None,
                Err(num) => panic!(self.error_out_of_range_literal(num)),
            },
            _ => panic!(self.error_expected_field("literal, or end-of-clause zero")),
        }
    }
    fn parse_witness(&mut self) -> Option<(Variable, Literal)> {
        let var = match self.next() {
            Some(VbeLexeme::Number(num)) => match Variable::try_from(num) {
                Ok(var) => var,
                Err(0u64) => return None,
                Err(num) => panic!(self.error_out_of_range_variable(num)),
            },
            _ => panic!(self.error_expected_field("variable, or end-of-witness zero")),
        };
        let lit = match self.next() {
            Some(VbeLexeme::Number(num)) => match Literal::try_from(num) {
                Ok(lit) => lit,
                Err(0u64) => Literal::Top,
                Err(1u64) => Literal::Bottom,
                Err(num) => panic!(self.error_out_of_range_atom(num)),
            },
            _ => panic!(self.error_expected_field("literal, positive zero or negative zero")),
        };
        Some((var, lit))
    }
    fn parse_chain(&mut self) -> Option<ClauseIndex> {
        match self.next() {
            Some(VbeLexeme::Number(num)) => match ClauseIndex::try_from(num) {
                Ok(lit) => Some(lit),
                Err(0u64) => None,
                Err(num) => panic!(self.error_out_of_range_id(num)),
            },
            _ => panic!(self.error_expected_field("clause identifier, or end-of-chain zero")),
        }
    }
    fn name(&self) -> &Path {
        &self.pos.name()
    }
}

// #[cfg(test)]
// mod test {
// 	use std::{
//         error::{Error},
//         fmt::{self, Formatter, Display},
//         io::{self},
//         mem::{self},
//         path::{Path},
//         panic::{self},
// 	};
// 	use crate::{
//         parser::{
//             DimacsLexeme::{self, Header as DHeader, Number as DNumber, Letter as DLetter},
//             VbeLexeme::{self, Header as VHeader, Number as VNumber},
//             ParsingError, DimacsParser, VbeParser, LexingError,
//         },
// 		input::{InputStream, CompressionFormat, FilePosition},
//     };

// 	fn check_dimacs_lexer(path: &Path, check: &Vec<DimacsLexeme>, err: Option<ParsingError>) {
//         let res = panic::catch_unwind(|| {
//             let mut vec = Vec::<DimacsLexeme>::new();
//             let is = InputStream::<'_>::open(path, CompressionFormat::Plain, false);
//             let mut parser = DimacsParser::<'_>::new(is, "CNF");
//             loop { match parser.next() {
//                 Some(x) => vec.push(x),
//                 None => break vec,
//             } }
//         });
//         match res {
//             Ok(vec) => {
//                 let mut it1 = check.iter();
//                 let mut it2 = vec.iter();
//                 loop {
//                     match (it1.next(), it2.next()) {
//                         (Some(x), Some(y)) => assert!(x == y),
//                         (None, None) => break,
//                         _ => assert!(false),
//                     }
//                 }
//             },
//             Err(pain) => match pain.downcast::<ParsingError>() {
//                 Ok(ps) => assert!(mem::discriminant(&err.unwrap()) == mem::discriminant(&ps)),
//                 Err(e) => panic!(e),
//             }
//         }
// 	}

// 	fn check_vbe_lexer(path: &Path, check: &Vec<VbeLexeme>, err: Option<ParsingError>) {
//         let res = panic::catch_unwind(|| {
//             let mut vec = Vec::<VbeLexeme>::new();
//             let is = InputStream::<'_>::open(path, CompressionFormat::Plain, true);
//             let mut parser = VbeParser::<'_>::new(is, "CNF");
//             loop { match parser.next() {
//                 Some(x) => vec.push(x),
//                 None => break vec,
//             } }
//         });
//         match res {
//             Ok(vec) => {
//                 let mut it1 = check.iter();
//                 let mut it2 = vec.iter();
//                 loop {
//                     match (it1.next(), it2.next()) {
//                         (Some(x), Some(y)) => assert!(x == y),
//                         (None, None) => break,
//                         _ => assert!(false),
//                     }
//                 }
//             },
//             Err(pain) => match pain.downcast::<ParsingError>() {
//                 Ok(ps) => assert!(mem::discriminant(&err.unwrap()) == mem::discriminant(&ps)),
//                 Err(e) => panic!(e),
//             }
//         }
//     }

//     fn lexing_error() -> Box<LexingError> {
//         Box::new(LexingError {
//             pos: FilePosition::new(Path::new(""), true).source(),
//             format: "CNF".to_string(),
//         })
//     }

// 	#[test]
// 	fn test_dimacs_lexer() {
// 		let tests: Vec<(&str, Vec<DimacsLexeme>, Option<ParsingError>)> = vec![
// 			("test/dimacs_lexer/cnf0.txt", vec![
// 				DHeader([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]),
// 				DNumber(50i64), DNumber(80i64), DNumber(16i64), DNumber(17i64),
// 				DNumber(30i64), DNumber(0i64), DNumber(-17i64), DNumber(22i64),
// 				DNumber(30i64), DNumber(0i64), DNumber(-17i64), DNumber(-22i64),
// 				DNumber(30i64), DNumber(0i64), DNumber(16i64), DNumber(-30i64),
// 				DNumber(47i64), DNumber(0i64), DNumber(16i64), DNumber(-30i64),
// 				DNumber(-47i64), DNumber(0i64), DNumber(-16i64), DNumber(-21i64),
// 				DNumber(31i64), DNumber(0i64), DNumber(-16i64), DNumber(-21i64),
// 				DNumber(-31i64), DNumber(0i64), DNumber(-16i64), DNumber(21i64),
// 				DNumber(-28i64), DNumber(0i64)
// 			], None),
// 			("test/dimacs_lexer/cnf1.txt", vec![
// 				DHeader([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]),
// 				DNumber(50i64), DNumber(80i64), DNumber(16i64), DNumber(17i64),
// 				DNumber(30i64), DNumber(0i64), DNumber(-17i64), DNumber(22i64),
// 				DNumber(30i64), DNumber(0i64)
// 			], Some(ParsingError::InvalidNumber(lexing_error()))),
// 			("test/dimacs_lexer/cnf2.txt", vec![], Some(ParsingError::InvalidHeader(lexing_error()))),
// 			("test/dimacs_lexer/cnf3.txt", vec![], Some(ParsingError::OutOfRangeHeader(lexing_error()))),
// 			("test/dimacs_lexer/cnf4.txt", vec![
// 				DHeader([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]), DNumber(50i64), DNumber(9223372036854775807i64)
// 			], None),
// 			("test/dimacs_lexer/cnf5.txt", vec![
// 				DHeader([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]), DNumber(50i64)
// 			], Some(ParsingError::OutOfRangeI64(lexing_error()))),
// 			("test/dimacs_lexer/cnf6.txt", vec![
// 				DHeader([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]), DNumber(50i64), DNumber(-9223372036854775808i64)
// 			], None),
// 			("test/dimacs_lexer/cnf7.txt", vec![
// 				DHeader([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]), DNumber(50i64)
// 			], Some(ParsingError::OutOfRangeI64(lexing_error()))),
// 			("test/dimacs_lexer/cnf8.txt", vec![
// 				DHeader([b'l', b'e', b't', b't', b'e', b'r', b's', 0u8]),
// 				DNumber(50i64), DNumber(80i64), DNumber(16i64), DNumber(17i64),
// 				DNumber(30i64), DNumber(0i64), DNumber(-17i64), DLetter(b'x'),
// 				DNumber(3i64), DNumber(0i64), DNumber(-17i64), DLetter(b'b'),
// 				DLetter(b'A'), DNumber(0i64)
// 			], None),
// 			("test/dimacs_lexer/cnf9.txt", vec![
// 				DHeader([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]),
// 				DNumber(50i64), DNumber(80i64)
// 			], Some(ParsingError::InvalidLetter(lexing_error())))
// 		];
// 		for (path, check, err) in tests {
// 			check_dimacs_lexer(Path::new(path), &check, err)
// 		}
// 	}

// 	#[test]
// 	fn test_vbe_lexer() {
// 		let tests: Vec<(&str, Vec<VbeLexeme>, Option<ParsingError>)> = vec![
// 			("test/vbe_lexer/vbe0.bin",
//                 vec![VNumber(142848914u64), VNumber(85u64), VNumber(55u64), VNumber(104u64), VNumber(1u64), VNumber(36u64), VNumber(12595012903u64),
//                 VNumber(10279u64), VNumber(19u64), VNumber(67u64), VNumber(62028944u64), VNumber(113u64), VNumber(86u64), VNumber(37u64), VNumber(29888669399u64)],
// 				None
// 			),
// 			("test/vbe_lexer/vbe1.bin",
// 				vec![VNumber(142848914u64), VNumber(85u64), VNumber(55u64), VNumber(104u64), VNumber(1u64), VNumber(36u64), VNumber(12595012903u64),
//                 VNumber(10279u64), VNumber(19u64), VNumber(67u64), VNumber(62028944u64), VNumber(113u64), VNumber(86u64), VNumber(37u64), VNumber(29888669399u64), VNumber(18446744073709551615u64)],
//                 None),
//             ("test/vbe_lexer/vbe2.bin",
// 				vec![VNumber(142848914u64), VNumber(85u64), VNumber(55u64), VNumber(104u64), VNumber(1u64), VNumber(36u64), VNumber(12595012903u64),
//                 VNumber(10279u64), VNumber(19u64), VNumber(67u64), VNumber(62028944u64), VNumber(113u64), VNumber(86u64), VNumber(37u64), VNumber(29888669399u64)],
// 				Some(ParsingError::OutOfRangeU64(lexing_error()))),
// 			("test/vbe_lexer/vbe3.bin",
//                 vec![VNumber(142848914u64), VNumber(85u64), VNumber(55u64), VNumber(104u64), VNumber(1u64), VNumber(36u64), VNumber(12595012903u64),
//                 VNumber(10279u64), VNumber(19u64), VNumber(67u64), VNumber(62028944u64), VNumber(113u64), VNumber(86u64), VNumber(37u64), VNumber(29888669399u64)],
// 				Some(ParsingError::InvalidVbeNumber(lexing_error()))),
// 			("test/vbe_lexer/vbe4.bin",
//                 vec![VNumber(142848914u64), VNumber(85u64), VNumber(55u64), VNumber(104u64), VNumber(1u64), VNumber(36u64), VNumber(12595012903u64),
//                 VNumber(10279u64), VNumber(19u64), VNumber(67u64), VNumber(62028944u64), VNumber(113u64), VNumber(86u64), VNumber(37u64), VNumber(29888669399u64),
//                 VHeader([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8])], None),
//             ("test/vbe_lexer/vbe5.bin",
//                 vec![VNumber(142848914u64), VNumber(85u64), VNumber(55u64), VNumber(104u64), VNumber(1u64), VNumber(36u64), VNumber(12595012903u64),
//                 VNumber(10279u64), VNumber(19u64), VNumber(67u64), VNumber(62028944u64), VNumber(113u64), VNumber(86u64), VNumber(37u64), VNumber(29888669399u64),
//                 VHeader([b'a', b'b', b'c', b'd', b'e', b'f', b'g', b'h']), VNumber(1u64)], None),
//             ("test/vbe_lexer/vbe6.bin",
//                 vec![VNumber(142848914u64), VNumber(85u64), VNumber(55u64), VNumber(104u64), VNumber(1u64), VNumber(36u64), VNumber(12595012903u64),
//                 VNumber(10279u64), VNumber(19u64), VNumber(67u64), VNumber(62028944u64), VNumber(113u64), VNumber(86u64), VNumber(37u64), VNumber(29888669399u64)],
//                 Some(ParsingError::InvalidVbeHeader(lexing_error()))),
// 		];
// 		for (path, check, err) in tests {
// 			check_vbe_lexer(Path::new(path), &check, err)
// 		}
// 	}
// }