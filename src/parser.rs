use std::{
    convert::{TryFrom},
    error::{Error},
    fmt::{self, Formatter, Binary, Display},
    io::{self},
    mem::{self},
};

use crate::{
    clausedb::{ClauseIndex},
    input::{FilePosition, InputStream},
    variable::{Variable, Literal, MaybeVariable},
};

#[derive(Debug)]
pub struct LexingError {
    pos: FilePosition,
    format: String,
}

#[derive(Debug)]
pub struct ExpectedField {
    field: String,
    pos: FilePosition,
    format: String,
}

#[derive(Debug)]
pub struct OutOfRangeField {
    field: String,
    found: String,
    range: String,
    pos: FilePosition,
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
impl Binary for ParsingError {
	fn fmt(&self, f: &mut Formatter) -> fmt::Result {
		match self {
            ParsingError::InvalidCharacter(bx) => write!(f, "Invalid character in {} file {:b}.", &bx.format, &bx.pos),
            ParsingError::InvalidNumber(bx) => write!(f, "Invalid number in {} file {:b}.", &bx.format, &bx.pos),
            ParsingError::InvalidLetter(bx) => write!(f, "Invalid letter in {} file {:b}.", &bx.format, &bx.pos),
            ParsingError::InvalidHeader(bx) => write!(f, "Invalid header in {} file {:b}.", &bx.format, &bx.pos),
            ParsingError::InvalidVbeNumber(bx) => write!(f, "Invalid variable-bit encoded integer in {} file {:b}:\nEOF found before terminating character.", &bx.format, &bx.pos),
            ParsingError::InvalidVbeHeader(bx) => write!(f, "Invalid variable-bit encoded header in {} file {:b}:\nEOF found within 8 header characters.", &bx.format, &bx.pos),
            ParsingError::OutOfRangeI64(bx) => write!(f, "Out of range number in {} file {:b}:\nNumber does not fit into an i64 integer.", &bx.format, &bx.pos),
            ParsingError::OutOfRangeU64(bx) => write!(f, "Out of range number in {} file {:b}:\nNumber does not fit into an u64 integer.", &bx.format, &bx.pos),
            ParsingError::OutOfRangeHeader(bx) => write!(f, "Out of range header in {} file {:b}:\nHeader exceeds the 8-character limit.", &bx.format, &bx.pos),
            ParsingError::ExpectedField(bx) => write!(f, "Unexpected input in {} file {:b}:\nExpected {}.", &bx.format, &bx.pos, &bx.field),
            ParsingError::OutOfRangeField(bx) => write!(f, "Out of range input in {} file {:b}:\nExpected {}, found {} but the admissible range is {}.", &bx.format, &bx.pos, &bx.field, &bx.found, &bx.range),
        }
    }
}
impl ParsingError {
    pub fn file_format(&self) -> &str {
        match self {
            ParsingError::InvalidCharacter(bx) |
            ParsingError::InvalidNumber(bx) |
            ParsingError::InvalidLetter(bx) |
            ParsingError::InvalidHeader(bx) |
            ParsingError::InvalidVbeNumber(bx) |
            ParsingError::InvalidVbeHeader(bx) |
            ParsingError::OutOfRangeI64(bx) |
            ParsingError::OutOfRangeU64(bx) |
            ParsingError::OutOfRangeHeader(bx) => &bx.format,
            ParsingError::ExpectedField(bx) => &bx.format,
            ParsingError::OutOfRangeField(bx) => &bx.format,
        }
    }
}
impl Error for ParsingError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
		None
    }
}

pub struct CnfHeaderStats {
	pub variables: MaybeVariable,
	pub clauses: usize
}

pub enum AsrInstructionKind {
    Rup(ClauseIndex),
    Sr(ClauseIndex),
    Del
}

pub trait AsrParser<E: Error> {
    fn skip_to_header(&mut self) -> Result<Option<[u8; 8]>, E>;
    fn parse_cnf_header(&mut self) -> Result<CnfHeaderStats, E>;
    fn parse_formula(&mut self) -> Result<Option<()>, E>;
    fn parse_core(&mut self) -> Result<Option<ClauseIndex>, E>;
    fn parse_proof(&mut self) -> Result<Option<AsrInstructionKind>, E>;
    fn parse_clause(&mut self) -> Result<Option<Literal>, E>;
    fn parse_witness(&mut self) -> Result<Option<(Variable, Literal)>, E>;
    fn parse_chain(&mut self) -> Result<Option<ClauseIndex>, E>;
    fn position(&self) -> &FilePosition;
}

#[derive(Debug, PartialEq)]
enum DimacsLexeme {
    Header([u8; 8]),
    Letter(u8),
    Number(i64),
}

pub struct DimacsParser<'a, E: Error + From<ParsingError> + From<io::Error>> {
    input: InputStream<'a, E>,
    format: &'a str,
    cache: Result<Option<DimacsLexeme>, E>,
}
impl<'a, E: Error + From<ParsingError> + From<io::Error>> DimacsParser<'a, E> {
    pub fn new(is: InputStream<'a, E>, format: &'a str) -> DimacsParser<'a, E> {
        let mut parser = DimacsParser::<'a, E> {
            input: is,
            format: format,
            cache: Ok(None),
        };
        parser.cache = parser.read();
        parser
    }
    fn peek(&mut self) -> &Result<Option<DimacsLexeme>, E> {
        &self.cache
    }
    fn next(&mut self) -> Result<Option<DimacsLexeme>, E> {
        let mut read = self.read();
        mem::swap(&mut self.cache, &mut read);
        read
    }
    fn read(&mut self) -> Result<Option<DimacsLexeme>, E> {
        match self.ignore()? {
            Some(c) if (c as char).is_numeric() => self.read_number(c, true),
            Some(c) if c == b'-' => self.read_negative_number(),
            Some(c) if (c as char).is_alphabetic() => self.read_letter(c),
            None => Ok(None),
            _ => self.error_invalid_character(),
        }
    }
    fn read_negative_number(&mut self) -> Result<Option<DimacsLexeme>, E> {
        match self.input.next()? {
            Some(c) if (c as char).is_numeric() => self.read_number(c, false),
            _ => self.error_invalid_number(),
        }
    }
    fn read_number(&mut self, initial: u8, sign: bool) -> Result<Option<DimacsLexeme>, E> {
        let factor = if sign {
            1i64
        } else {
            -1i64
        };
        let mut num: i64 = factor * ((initial - b'0') as i64);
        loop { match self.input.next()? {
            Some(c) if (c as char).is_numeric() => {
                match num.checked_mul(10i64).and_then(|x| x.checked_add(factor * ((c - b'0') as i64))) {
                    Some(x) => num = x,
                    None => self.error_out_of_range_i64()?,
                }
            },
            Some(c) if (c as char).is_whitespace() => break Ok(Some(DimacsLexeme::Number(num))),
            None => break Ok(Some(DimacsLexeme::Number(num))),
            _ => self.error_invalid_number()?,
        } }
    }
    fn read_letter(&mut self, letter: u8) -> Result<Option<DimacsLexeme>, E> {
        if letter == b'p' {
            self.read_header()
        } else {
            match self.input.next()? {
                Some(c) if (c as char).is_whitespace() => Ok(Some(DimacsLexeme::Letter(letter))),
                None => Ok(Some(DimacsLexeme::Letter(letter))),
                _ => self.error_invalid_letter()?,
            }
        }
    }
    fn read_header(&mut self) -> Result<Option<DimacsLexeme>, E> {
        let mut hd = [0u8; 8usize];
        hd[0] = loop { match self.input.next()? {
            Some(c) if (c as char).is_whitespace() => (),
            None => self.error_invalid_header()?,
            Some(c) => break c,
        } };
        for i in 0usize..8usize {
            match self.input.next()? {
                Some(c) if (c as char).is_alphabetic() => if i < 7usize {
                    hd[i + 1] = c;
                } else {
                    self.error_out_of_range_header()?;
                },
                Some(c) if (c as char).is_whitespace() => break,
                None => break,
                _ => self.error_invalid_header()?,
            }
        }
        Ok(Some(DimacsLexeme::Header(hd)))
    }
    fn ignore(&mut self) -> Result<Option<u8>, E> {
        loop { match self.input.next()? {
            Some(c) if c == b'c' => self.skip_comment()?,
            Some(c) if (c as char).is_whitespace() => (),
            x => break Ok(x),
        } }
    }
    fn skip_comment(&mut self) -> Result<(), E> {
        loop { match self.input.next()? {
            Some(c) if c == b'\n' => break Ok(()),
            None => break Ok(()),
            _ => (),
        } }
    }
    fn error_invalid_character<T>(&self) -> Result<T, E> {
        Err(E::from(ParsingError::InvalidCharacter(Box::new(LexingError {
            pos: self.input.position().clone(),
            format: self.format.to_string(),
        }))))
    }
    fn error_invalid_number<T>(&self) -> Result<T, E> {
        Err(E::from(ParsingError::InvalidNumber(Box::new(LexingError {
            pos: self.input.position().clone(),
            format: self.format.to_string(),
        }))))
    }
    fn error_out_of_range_i64<T>(&self) -> Result<T, E> {
        Err(E::from(ParsingError::OutOfRangeI64(Box::new(LexingError {
            pos: self.input.position().clone(),
            format: self.format.to_string(),
        }))))
    }
    fn error_invalid_letter<T>(&self) -> Result<T, E> {
        Err(E::from(ParsingError::InvalidLetter(Box::new(LexingError {
            pos: self.input.position().clone(),
            format: self.format.to_string(),
        }))))
    }
    fn error_invalid_header<T>(&self) -> Result<T, E> {
        Err(E::from(ParsingError::InvalidHeader(Box::new(LexingError {
            pos: self.input.position().clone(),
            format: self.format.to_string(),
        }))))
    }
    fn error_out_of_range_header<T>(&self) -> Result<T, E> {
        Err(E::from(ParsingError::OutOfRangeHeader(Box::new(LexingError {
            pos: self.input.position().clone(),
            format: self.format.to_string(),
        }))))
    }
    fn error_expected_field<T>(&self, field: &str) -> Result<T, E> {
        Err(E::from(ParsingError::ExpectedField(Box::new(ExpectedField {
            field: field.to_string(),
            pos: self.input.position().clone(),
            format: self.format.to_string(),
        }))))
    }
    fn error_out_of_range_id<T>(&self, num: i64) -> Result<T, E> {
        Err(E::from(ParsingError::OutOfRangeField(Box::new(OutOfRangeField {
            found: format!("{}", num),
            range: format!("[{} .. {}]", 1i64, ClauseIndex::MaxValue),
            field: "clause identifier".to_string(),
            pos: self.input.position().clone(),
            format: self.format.to_string(),
        }))))
    }
    fn error_out_of_range_literal<T>(&self, num: i64) -> Result<T, E> {
        Err(E::from(ParsingError::OutOfRangeField(Box::new(OutOfRangeField {
            found: format!("{}", num),
            range: format!("[{} .. {}] U [{} .. {}]", -Literal::MaxValue, -1i64, 1i64, Literal::MaxValue),
            field: "literal".to_string(),
            pos: self.input.position().clone(),
            format: self.format.to_string(),
        }))))
    }
    fn error_out_of_range_variable<T>(&self, num: i64) -> Result<T, E> {
        Err(E::from(ParsingError::OutOfRangeField(Box::new(OutOfRangeField {
            found: format!("{}", num),
            range: format!("[{} .. {}]", 1i64, Variable::MaxValue),
            field: "variable".to_string(),
            pos: self.input.position().clone(),
            format: self.format.to_string(),
        }))))
    }
    fn error_out_of_range_num_variables<T>(&self, num: i64) -> Result<T, E> {
        Err(E::from(ParsingError::OutOfRangeField(Box::new(OutOfRangeField {
            found: format!("{}", num),
            range: format!("[{} .. {}]", 0i64, Variable::MaxValue),
            field: "number of variables".to_string(),
            pos: self.input.position().clone(),
            format: self.format.to_string(),
        }))))
    }
    fn error_out_of_range_num_clauses<T>(&self, num: i64) -> Result<T, E> {
        let max = i64::try_from(usize::max_value()).unwrap_or(i64::max_value());
        Err(E::from(ParsingError::OutOfRangeField(Box::new(OutOfRangeField {
            found: format!("{}", num),
            range: format!("[{} .. {}]", 1i64, max),
            field: "number of clauses".to_string(),
            pos: self.input.position().clone(),
            format: self.format.to_string(),
        }))))
    }
}
impl<'a, E: Error + From<ParsingError> + From<io::Error>> AsrParser<E> for DimacsParser<'a, E> {
    fn skip_to_header(&mut self) -> Result<Option<[u8; 8]>, E> {
        loop { match self.next()? {
            Some(DimacsLexeme::Header(hd)) => break Ok(Some(hd)),
            None => break Ok(None),
            _ => (),
        } }
    }
    fn parse_cnf_header(&mut self) -> Result<CnfHeaderStats, E> {
        let vars = match self.next()? {
            Some(DimacsLexeme::Number(num)) => match MaybeVariable::try_from(num) {
                Ok(var) => var,
                Err(num) => self.error_out_of_range_num_variables(num)?,
            },
            _ => self.error_expected_field("number of variables")?,
        };
        let clauses = match self.next()? {
            Some(DimacsLexeme::Number(num)) => match usize::try_from(num) {
                Ok(cls) => cls,
                Err(_) => self.error_out_of_range_num_clauses(num)?,
            },
            _ => self.error_expected_field("number of clauses")?,
        };
        Ok(CnfHeaderStats {
            variables: vars,
            clauses: clauses,
        })
    }
    fn parse_formula(&mut self) -> Result<Option<()>, E> {
        match self.peek() {
            Ok(Some(DimacsLexeme::Number(_))) => Ok(Some(())),
            Ok(None) => Ok(None),
            Err(_) => Err(self.next().unwrap_err()),
            _ => self.error_expected_field("clause literal, or EOF")?,
        }
    }
    fn parse_core(&mut self) -> Result<Option<ClauseIndex>, E> {
        Ok(match self.next()? {
            Some(DimacsLexeme::Letter(b'k')) => match self.next()? {
                Some(DimacsLexeme::Number(num)) => match ClauseIndex::try_from(num) {
                    Ok(id) => Some(id),
                    Err(num) => self.error_out_of_range_id(num)?,
                },
                _ => self.error_expected_field("clause identifier")?,
            },
            None => None,
            _ => self.error_expected_field("core marker '0x6B', or EOF")?,
        })
    }
    fn parse_proof(&mut self) -> Result<Option<AsrInstructionKind>, E> {
        Ok(match self.next()? {
            Some(DimacsLexeme::Letter(b'r')) => match self.next()? {
                Some(DimacsLexeme::Number(num)) => match ClauseIndex::try_from(num) {
                    Ok(id) => Some(AsrInstructionKind::Rup(id)),
                    Err(num) => self.error_out_of_range_id(num)?,
                },
                _ => self.error_expected_field("clause identifier")?,
            },
            Some(DimacsLexeme::Letter(b's')) => match self.next()? {
                Some(DimacsLexeme::Number(num)) => match ClauseIndex::try_from(num) {
                    Ok(id) => Some(AsrInstructionKind::Sr(id)),
                    Err(num) => self.error_out_of_range_id(num)?,
                },
                _ => self.error_expected_field("clause identifier")?,
            },
            Some(DimacsLexeme::Letter(b'd')) => Some(AsrInstructionKind::Del),
            None => None,
            _ => self.error_expected_field("instruction marker 'r', 's', 'd', or EOF")?
        })
    }
    fn parse_clause(&mut self) -> Result<Option<Literal>, E> {
        Ok(match self.next()? {
            Some(DimacsLexeme::Number(num)) => match Literal::try_from(num) {
                Ok(lit) => Some(lit),
                Err(0i64) => None,
                Err(num) => self.error_out_of_range_literal(num)?,
            },
            _ => self.error_expected_field("literal, or end-of-clause zero")?,
        })
    }
    fn parse_witness(&mut self) -> Result<Option<(Variable, Literal)>, E> {
        let var = match self.next()? {
            Some(DimacsLexeme::Number(num)) => match Variable::try_from(num) {
                Ok(var) => var,
                Err(0i64) => return Ok(None),
                Err(num) => self.error_out_of_range_variable(num)?,
            },
            _ => self.error_expected_field("variable, or end-of-witness zero")?,
        };
        let lit = match self.next()? {
            Some(DimacsLexeme::Number(num)) => match Literal::try_from(num) {
                Ok(lit) => lit,
                Err(num) => self.error_out_of_range_literal(num)?,
            },
            Some(DimacsLexeme::Letter(b't')) => Literal::Top,
            Some(DimacsLexeme::Letter(b'f')) => Literal::Bottom,
            _ => self.error_expected_field("literal, top 't', or bottom 'b'")?,
        };
        Ok(Some((var, lit)))
    }
    fn parse_chain(&mut self) -> Result<Option<ClauseIndex>, E> {
        Ok(match self.next()? {
            Some(DimacsLexeme::Number(num)) => match ClauseIndex::try_from(num) {
                Ok(lit) => Some(lit),
                Err(0i64) => None,
                Err(num) => self.error_out_of_range_id(num)?,
            },
            _ => self.error_expected_field("clause identifier, or end-of-chain zero")?,
        })
    }
    fn position(&self) -> &FilePosition {
        self.input.position()
    }
}

#[derive(Debug, PartialEq)]
enum VbeLexeme {
    Header([u8; 8]),
    Number(u64),
}
pub struct VbeParser<'a, E: Error + From<ParsingError> + From<io::Error>> {
    input: InputStream<'a, E>,
    cache: Result<Option<VbeLexeme>, E>,
    format: &'a str,
}
impl<'a, E: Error + From<ParsingError> + From<io::Error>> VbeParser<'a, E> {
    const LowercaseK: u64 = b'k' as u64;
    const LowercaseR: u64 = b'r' as u64;
    const LowercaseS: u64 = b's' as u64;
    const LowercaseD: u64 = b'd' as u64;
    pub fn new(is: InputStream<'a, E>, format: &'a str) -> VbeParser<'a, E> {
        let mut parser = VbeParser::<'a, E> {
            input: is,
            cache: Ok(None),
            format: format,
        };
        parser.cache = parser.read_number();
        parser
    }
    fn peek(&mut self) -> &Result<Option<VbeLexeme>, E> {
        &self.cache
    }
    fn next(&mut self) -> Result<Option<VbeLexeme>, E> {
        let mut read = self.read_number();
        mem::swap(&mut self.cache, &mut read);
        read
    }
    fn read_number(&mut self) -> Result<Option<VbeLexeme>, E> {
        let mut num: u64 = 0u64;
        let mut shift: u8 = 0u8;
        loop { match self.input.next()? {
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
                        break Ok(Some(VbeLexeme::Number(num)))
                    }
                } else {
                    self.error_out_of_range_number()?
                }
            },
            None => if shift == 0u8 {
                break Ok(None)
            } else {
                self.error_invalid_vbe_number()?
            },
        } }
    }
    fn read_header(&mut self) -> Result<Option<VbeLexeme>, E> {
        let mut hd = [0u8; 8];
        for i in 0usize..8usize {
            match self.input.next()? {
                Some(c) => hd[i] = c,
                None => self.error_invalid_vbe_header()?,
            }
        }
        Ok(Some(VbeLexeme::Header(hd)))
    }
    fn error_out_of_range_number<T>(&self) -> Result<T, E> {
        Err(E::from(ParsingError::OutOfRangeU64(Box::new(LexingError {
            pos: self.input.position().clone(),
            format: self.format.to_string(),
        }))))
    }
    fn error_invalid_vbe_number<T>(&self) -> Result<T, E> {
        Err(E::from(ParsingError::InvalidVbeNumber(Box::new(LexingError {
            pos: self.input.position().clone(),
            format: self.format.to_string(),
        }))))
    }
    fn error_invalid_vbe_header<T>(&self) -> Result<T, E> {
        Err(E::from(ParsingError::InvalidVbeHeader(Box::new(LexingError {
            pos: self.input.position().clone(),
            format: self.format.to_string(),
        }))))
    }
    fn error_expected_field<T>(&self, field: &str) -> Result<T, E> {
        Err(E::from(ParsingError::ExpectedField(Box::new(ExpectedField {
            field: field.to_string(),
            pos: self.input.position().clone(),
            format: self.format.to_string(),
        }))))
    }
    fn error_out_of_range_id<T>(&self, num: u64) -> Result<T, E> {
        Err(E::from(ParsingError::OutOfRangeField(Box::new(OutOfRangeField {
            found: format!("{}", num),
            range: format!("[{} .. {}]", 1i64, ClauseIndex::MaxValue),
            field: "clause identifier".to_string(),
            pos: self.input.position().clone(),
            format: self.format.to_string(),
        }))))
    }
    fn error_out_of_range_literal<T>(&self, num: u64) -> Result<T, E> {
        Err(E::from(ParsingError::OutOfRangeField(Box::new(OutOfRangeField {
            found: format!("{}", num),
            range: format!("[{} .. {}]", 2u64, u32::max_value()),
            field: "literal".to_string(),
            pos: self.input.position().clone(),
            format: self.format.to_string(),
        }))))
    }
    fn error_out_of_range_atom<T>(&self, num: u64) -> Result<T, E> {
        Err(E::from(ParsingError::OutOfRangeField(Box::new(OutOfRangeField {
            found: format!("{}", num),
            range: format!("[{} .. {}]", 0u64, u32::max_value()),
            field: "literal, top or bottom".to_string(),
            pos: self.input.position().clone(),
            format: self.format.to_string(),
        }))))
    }
    fn error_out_of_range_variable<T>(&self, num: u64) -> Result<T, E> {
        Err(E::from(ParsingError::OutOfRangeField(Box::new(OutOfRangeField {
            found: format!("{}", num),
            range: format!("[{} .. {}]", 1i64, Variable::MaxValue),
            field: "variable".to_string(),
            pos: self.input.position().clone(),
            format: self.format.to_string(),
        }))))
    }
    fn error_out_of_range_num_variables<T>(&self, num: u64) -> Result<T, E> {
        Err(E::from(ParsingError::OutOfRangeField(Box::new(OutOfRangeField {
            found: format!("{}", num),
            range: format!("[{} .. {}]", 0i64, Variable::MaxValue),
            field: "number of variables".to_string(),
            pos: self.input.position().clone(),
            format: self.format.to_string(),
        }))))
    }
    fn error_out_of_range_num_clauses<T>(&self, num: u64) -> Result<T, E> {
        let max = u64::try_from(usize::max_value()).unwrap_or(u64::max_value());
        Err(E::from(ParsingError::OutOfRangeField(Box::new(OutOfRangeField {
            found: format!("{}", num),
            range: format!("[{} .. {}]", 1i64, max),
            field: "number of clauses".to_string(),
            pos: self.input.position().clone(),
            format: self.format.to_string(),
        }))))
    }
}
impl<'a, E: Error + From<ParsingError> + From<io::Error>> AsrParser<E> for VbeParser<'a, E> {
    fn skip_to_header(&mut self) -> Result<Option<[u8; 8]>, E> {
        loop { match self.next()? {
            Some(VbeLexeme::Header(hd)) => break Ok(Some(hd)),
            None => break Ok(None),
            _ => (),
        } }
    }
    fn parse_cnf_header(&mut self) -> Result<CnfHeaderStats, E> {
        let vars = match self.next()? {
            Some(VbeLexeme::Number(num)) => match MaybeVariable::try_from(num) {
                Ok(var) => var,
                Err(num) => self.error_out_of_range_num_variables(num)?,
            },
            _ => self.error_expected_field("number of variables")?,
        };
        let clauses = match self.next()? {
            Some(VbeLexeme::Number(num)) => match usize::try_from(num) {
                Ok(cls) => cls,
                Err(_) => self.error_out_of_range_num_clauses(num)?,
            },
            _ => self.error_expected_field("number of clauses")?,
        };
        Ok(CnfHeaderStats {
            variables: vars,
            clauses: clauses,
        })
    }
    fn parse_formula(&mut self) -> Result<Option<()>, E> {
        match self.peek() {
            Ok(Some(VbeLexeme::Number(_))) => Ok(Some(())),
            Ok(None) => Ok(None),
            Err(_) => Err(self.next().unwrap_err()),
            _ => self.error_expected_field("clause literal, or EOF")?,
        }
    }
    fn parse_core(&mut self) -> Result<Option<ClauseIndex>, E> {
        Ok(match self.next()? {
            Some(VbeLexeme::Number(VbeParser::<'a, E>::LowercaseK)) => match self.next()? {
                Some(VbeLexeme::Number(num)) => match ClauseIndex::try_from(num) {
                    Ok(id) => Some(id),
                    Err(num) => self.error_out_of_range_id(num)?,
                },
                _ => self.error_expected_field("clause identifier")?,
            },
            None => None,
            _ => self.error_expected_field("core marker '0x6B', or EOF")?,
        })
    }
    fn parse_proof(&mut self) -> Result<Option<AsrInstructionKind>, E> {
        Ok(match self.next()? {
            Some(VbeLexeme::Number(VbeParser::<'a, E>::LowercaseR)) => match self.next()? {
                Some(VbeLexeme::Number(num)) => match ClauseIndex::try_from(num) {
                    Ok(id) => Some(AsrInstructionKind::Rup(id)),
                    Err(num) => self.error_out_of_range_id(num)?,
                },
                _ => self.error_expected_field("clause identifier")?,
            },
            Some(VbeLexeme::Number(VbeParser::<'a, E>::LowercaseS)) => match self.next()? {
                Some(VbeLexeme::Number(num)) => match ClauseIndex::try_from(num) {
                    Ok(id) => Some(AsrInstructionKind::Sr(id)),
                    Err(num) => self.error_out_of_range_id(num)?,
                },
                _ => self.error_expected_field("clause identifier")?,
            },
            Some(VbeLexeme::Number(VbeParser::<'a, E>::LowercaseD)) => Some(AsrInstructionKind::Del),
            None => None,
            _ => self.error_expected_field("instruction marker '0x72', '0x73', '0x64', or EOF")?
        })
    }
    fn parse_clause(&mut self) -> Result<Option<Literal>, E> {
        Ok(match self.next()? {
            Some(VbeLexeme::Number(num)) => match Literal::try_from(num) {
                Ok(lit) => Some(lit),
                Err(0u64) => None,
                Err(num) => self.error_out_of_range_literal(num)?,
            },
            _ => self.error_expected_field("literal, or end-of-clause zero")?,
        })
    }
    fn parse_witness(&mut self) -> Result<Option<(Variable, Literal)>, E> {
        let var = match self.next()? {
            Some(VbeLexeme::Number(num)) => match Variable::try_from(num) {
                Ok(var) => var,
                Err(0u64) => return Ok(None),
                Err(num) => self.error_out_of_range_variable(num)?,
            },
            _ => self.error_expected_field("variable, or end-of-witness zero")?,
        };
        let lit = match self.next()? {
            Some(VbeLexeme::Number(num)) => match Literal::try_from(num) {
                Ok(lit) => lit,
                Err(0u64) => Literal::Top,
                Err(1u64) => Literal::Bottom,
                Err(num) => self.error_out_of_range_atom(num)?,
            },
            _ => self.error_expected_field("literal, positive zero or negative zero")?,
        };
        Ok(Some((var, lit)))
    }
    fn parse_chain(&mut self) -> Result<Option<ClauseIndex>, E> {
        Ok(match self.next()? {
            Some(VbeLexeme::Number(num)) => match ClauseIndex::try_from(num) {
                Ok(lit) => Some(lit),
                Err(0u64) => None,
                Err(num) => self.error_out_of_range_id(num)?,
            },
            _ => self.error_expected_field("clause identifier, or end-of-chain zero")?,
        })
    }
    fn position(&self) -> &FilePosition {
        self.input.position()
    }
}

#[cfg(test)]
mod test {
	use std::{
        error::{Error},
        fmt::{self, Formatter, Display},
        io::{self},
        mem::{self},
        path::{Path},
	};
	use crate::{
        parser::{
            DimacsLexeme::{self, Header as DHeader, Number as DNumber, Letter as DLetter},
            VbeLexeme::{self, Header as VHeader, Number as VNumber},
            ParsingError, DimacsParser, VbeParser, LexingError,
        },
		input::{InputStream, CompressionFormat, FilePosition},
    };
    
    #[derive(Debug)]
    enum TestError {
        Input(io::Error),
        Parsing(ParsingError),
    }
    impl From<io::Error> for TestError {
        fn from(err: io::Error) -> TestError {
            TestError::Input(err)
        }
    }
    impl From<ParsingError> for TestError {
        fn from(err: ParsingError) -> TestError {
            TestError::Parsing(err)
        }
    }
    impl Error for TestError {
        fn source(&self) -> Option<&(dyn Error + 'static)> {
            match self {
                TestError::Input(err) => Some(err),
                TestError::Parsing(err) => Some(err),
            }
        }
    }
    impl Display for TestError {
        fn fmt(&self, f: &mut Formatter) -> fmt::Result {
            write!(f, "")
        }
    }

	fn check_dimacs_lexer(path: &Path, check: &Vec<DimacsLexeme>, err: Option<ParsingError>) {
		let is = InputStream::<'_>::open(path, CompressionFormat::Plain).expect("Could not open file");
		let mut parser = DimacsParser::<'_, TestError>::new(is, "CNF");
        let mut vec = Vec::<DimacsLexeme>::new();
        loop { match parser.next() {
            Ok(Some(x)) => vec.push(x),
            Err(e) => {
                assert!(err.is_some());
                match e {
                    TestError::Parsing(ps) => assert!(mem::discriminant(&ps) == mem::discriminant(&err.unwrap())),
                    TestError::Input(_) => assert!(false),
                }
                break
            },
            Ok(None) => {
                assert!(err.is_none());
                break
            },
        } }
		let mut it1 = check.iter();
		let mut it2 = vec.iter();
		loop {
			match (it1.next(), it2.next()) {
				(Some(x), Some(y)) => assert!(x == y),
				(None, None) => break,
				_ => assert!(false),
			}
		}
	}

	fn check_vbe_lexer(path: &Path, check: &Vec<VbeLexeme>, err: Option<ParsingError>) {
		let is = InputStream::<'_>::open(path, CompressionFormat::Plain).expect("Could not open file");
		let mut parser = VbeParser::<'_, TestError>::new(is, "CNF");
        let mut vec = Vec::<VbeLexeme>::new();
        loop { match parser.next() {
            Ok(Some(x)) => vec.push(x),
            Err(e) => {
                assert!(err.is_some());
                match e {
                    TestError::Parsing(ps) => assert!(mem::discriminant(&ps) == mem::discriminant(&err.unwrap())),
                    TestError::Input(_) => assert!(false),
                }
                break
            },
            Ok(None) => {
                assert!(err.is_none());
                break
            },
        } }
		let mut it1 = check.iter();
		let mut it2 = vec.iter();
		loop {
			match (it1.next(), it2.next()) {
				(Some(x), Some(y)) => {
                    assert!(x == y);
                },
				(None, None) => break,
				_ => assert!(false),
			}
		}
    }

    fn lexing_error() -> Box<LexingError> {
        Box::new(LexingError {
            pos: FilePosition::new(Path::new("")),
            format: "CNF".to_string(),
        })
    }

	#[test]
	fn test_dimacs_lexer() {
		let tests: Vec<(&str, Vec<DimacsLexeme>, Option<ParsingError>)> = vec![
			("test/dimacs_lexer/cnf0.txt", vec![
				DHeader([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]),
				DNumber(50i64), DNumber(80i64), DNumber(16i64), DNumber(17i64),
				DNumber(30i64), DNumber(0i64), DNumber(-17i64), DNumber(22i64),
				DNumber(30i64), DNumber(0i64), DNumber(-17i64), DNumber(-22i64),
				DNumber(30i64), DNumber(0i64), DNumber(16i64), DNumber(-30i64),
				DNumber(47i64), DNumber(0i64), DNumber(16i64), DNumber(-30i64),
				DNumber(-47i64), DNumber(0i64), DNumber(-16i64), DNumber(-21i64),
				DNumber(31i64), DNumber(0i64), DNumber(-16i64), DNumber(-21i64),
				DNumber(-31i64), DNumber(0i64), DNumber(-16i64), DNumber(21i64),
				DNumber(-28i64), DNumber(0i64)
			], None),
			("test/dimacs_lexer/cnf1.txt", vec![
				DHeader([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]),
				DNumber(50i64), DNumber(80i64), DNumber(16i64), DNumber(17i64),
				DNumber(30i64), DNumber(0i64), DNumber(-17i64), DNumber(22i64),
				DNumber(30i64), DNumber(0i64)
			], Some(ParsingError::InvalidNumber(lexing_error()))),
			("test/dimacs_lexer/cnf2.txt", vec![], Some(ParsingError::InvalidHeader(lexing_error()))),
			("test/dimacs_lexer/cnf3.txt", vec![], Some(ParsingError::OutOfRangeHeader(lexing_error()))),
			("test/dimacs_lexer/cnf4.txt", vec![
				DHeader([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]), DNumber(50i64), DNumber(9223372036854775807i64)
			], None),
			("test/dimacs_lexer/cnf5.txt", vec![
				DHeader([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]), DNumber(50i64)
			], Some(ParsingError::OutOfRangeI64(lexing_error()))),
			("test/dimacs_lexer/cnf6.txt", vec![
				DHeader([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]), DNumber(50i64), DNumber(-9223372036854775808i64)
			], None),
			("test/dimacs_lexer/cnf7.txt", vec![
				DHeader([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]), DNumber(50i64)
			], Some(ParsingError::OutOfRangeI64(lexing_error()))),
			("test/dimacs_lexer/cnf8.txt", vec![
				DHeader([b'l', b'e', b't', b't', b'e', b'r', b's', 0u8]),
				DNumber(50i64), DNumber(80i64), DNumber(16i64), DNumber(17i64),
				DNumber(30i64), DNumber(0i64), DNumber(-17i64), DLetter(b'x'),
				DNumber(3i64), DNumber(0i64), DNumber(-17i64), DLetter(b'b'),
				DLetter(b'A'), DNumber(0i64)
			], None),
			("test/dimacs_lexer/cnf9.txt", vec![
				DHeader([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]),
				DNumber(50i64), DNumber(80i64)
			], Some(ParsingError::InvalidLetter(lexing_error())))
		];
		for (path, check, err) in tests {
			check_dimacs_lexer(Path::new(path), &check, err)
		}
	}

	#[test]
	fn test_vbe_lexer() {
		let tests: Vec<(&str, Vec<VbeLexeme>, Option<ParsingError>)> = vec![
			("test/vbe_lexer/vbe0.bin",
                vec![VNumber(142848914u64), VNumber(85u64), VNumber(55u64), VNumber(104u64), VNumber(1u64), VNumber(36u64), VNumber(12595012903u64),
                VNumber(10279u64), VNumber(19u64), VNumber(67u64), VNumber(62028944u64), VNumber(113u64), VNumber(86u64), VNumber(37u64), VNumber(29888669399u64)],
				None
			),
			("test/vbe_lexer/vbe1.bin",
				vec![VNumber(142848914u64), VNumber(85u64), VNumber(55u64), VNumber(104u64), VNumber(1u64), VNumber(36u64), VNumber(12595012903u64),
                VNumber(10279u64), VNumber(19u64), VNumber(67u64), VNumber(62028944u64), VNumber(113u64), VNumber(86u64), VNumber(37u64), VNumber(29888669399u64), VNumber(18446744073709551615u64)],
                None),
            ("test/vbe_lexer/vbe2.bin",
				vec![VNumber(142848914u64), VNumber(85u64), VNumber(55u64), VNumber(104u64), VNumber(1u64), VNumber(36u64), VNumber(12595012903u64),
                VNumber(10279u64), VNumber(19u64), VNumber(67u64), VNumber(62028944u64), VNumber(113u64), VNumber(86u64), VNumber(37u64), VNumber(29888669399u64)],
				Some(ParsingError::OutOfRangeU64(lexing_error()))),
			("test/vbe_lexer/vbe3.bin",
                vec![VNumber(142848914u64), VNumber(85u64), VNumber(55u64), VNumber(104u64), VNumber(1u64), VNumber(36u64), VNumber(12595012903u64),
                VNumber(10279u64), VNumber(19u64), VNumber(67u64), VNumber(62028944u64), VNumber(113u64), VNumber(86u64), VNumber(37u64), VNumber(29888669399u64)],
				Some(ParsingError::InvalidVbeNumber(lexing_error()))),
			("test/vbe_lexer/vbe4.bin",
                vec![VNumber(142848914u64), VNumber(85u64), VNumber(55u64), VNumber(104u64), VNumber(1u64), VNumber(36u64), VNumber(12595012903u64),
                VNumber(10279u64), VNumber(19u64), VNumber(67u64), VNumber(62028944u64), VNumber(113u64), VNumber(86u64), VNumber(37u64), VNumber(29888669399u64),
                VHeader([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8])], None),
            ("test/vbe_lexer/vbe5.bin",
                vec![VNumber(142848914u64), VNumber(85u64), VNumber(55u64), VNumber(104u64), VNumber(1u64), VNumber(36u64), VNumber(12595012903u64),
                VNumber(10279u64), VNumber(19u64), VNumber(67u64), VNumber(62028944u64), VNumber(113u64), VNumber(86u64), VNumber(37u64), VNumber(29888669399u64),
                VHeader([b'a', b'b', b'c', b'd', b'e', b'f', b'g', b'h']), VNumber(1u64)], None),
            ("test/vbe_lexer/vbe6.bin",
                vec![VNumber(142848914u64), VNumber(85u64), VNumber(55u64), VNumber(104u64), VNumber(1u64), VNumber(36u64), VNumber(12595012903u64),
                VNumber(10279u64), VNumber(19u64), VNumber(67u64), VNumber(62028944u64), VNumber(113u64), VNumber(86u64), VNumber(37u64), VNumber(29888669399u64)],
                Some(ParsingError::InvalidVbeHeader(lexing_error()))),
		];
		for (path, check, err) in tests {
			check_vbe_lexer(Path::new(path), &check, err)
		}
	}
}