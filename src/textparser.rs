use std::{
    convert::{TryFrom},
    io::{Write, Result as IoResult},
    mem::{self},
    path::{PathBuf, Path},
    char::{self},
};

use crate::{
    io::{InputReader, FilePosition, DeferredPosition},
    basic::{Literal, Variable, ClauseIndex},
};

#[derive(Copy, Clone, Debug)]
enum Lexeme {
    Number(i64),
    Letter(u8),
    Header([u8; 8]),
    Quote,
}

#[derive(PartialEq)]
pub enum PrepParsedInstructionKind {
    Core,
    Rup,
    Wsr,
    Del,
}

pub struct PrepParsedInstruction {
    kind: PrepParsedInstructionKind,
    id: ClauseIndex,
    ofp: FilePosition,
    dfp: Option<FilePosition>,
}
impl PrepParsedInstruction {
    #[inline(always)]
    fn new(kind: PrepParsedInstructionKind, id: ClauseIndex, ofp: FilePosition, dfp: Option<FilePosition>) -> PrepParsedInstruction {
        PrepParsedInstruction {
            kind: kind,
            id: id,
            ofp: ofp,
            dfp: dfp,
        }
    }
    #[inline(always)]
    pub fn kind(&self) -> &PrepParsedInstructionKind {
        &self.kind
    }
    #[inline(always)]
    pub fn index(&self) -> &ClauseIndex {
        &self.id
    }
    pub fn position(&self, original: &Path, deferred: &Path) -> DeferredPosition {
        DeferredPosition::with_paths(self.ofp, self.dfp, original, deferred)
    }
    pub fn default_position(&self) -> FilePosition {
        if let Some(fp) = self.dfp {
            fp
        } else {
            self.ofp
        }
    }
}

pub struct TextAsrParser<'a> {
    input: InputReader<'a>,
    position: FilePosition,
    cache: Option<Lexeme>,
    quote: String,
}
impl<'a> TextAsrParser<'a> {
    pub fn new(input: InputReader<'a>) -> TextAsrParser<'a> {
        let pos = input.position();
        let mut parser = TextAsrParser::<'a> {
            input: input,
            position: pos,
            cache: None,
            quote: String::new(),
        };
        parser.consume();
        parser
    }
    pub fn parse_clause<'b>(&'b mut self) -> TextClauseParser<'a, 'b> {
        TextClauseParser::<'a, 'b> { parser: Some(self) }
    }
    pub fn parse_witness<'b>(&'b mut self) -> TextWitnessParser<'a, 'b> {
        TextWitnessParser::<'a, 'b> { parser: Some(self) }
    }
    pub fn parse_chain<'b>(&'b mut self) -> TextChainParser<'a, 'b> {
        TextChainParser::<'a, 'b> { parser: Some(self) }
    }
    pub fn parse_multichain<'b>(&'b mut self,id: ClauseIndex) -> TextMultichainParser<'a, 'b> {
        TextMultichainParser::<'a, 'b> { parser: Some(self), id: Some(id) }
    }
    pub fn parse_cnf_header(&mut self) -> (FilePosition, u32, u64) {
        let pos = match self.peek() {
            Some(Lexeme::Header([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8])) => self.position(),
            _ => self.expected_header("Expected 'p cnf' header."),
        };
        self.consume();
        let var = match self.peek_number_variables("Expected the number of variables in the formula.") {
            Ok(opt) => opt,
            Err(num) => self.out_of_range_number_variables(num),
        };
        self.consume();
        let cls = match self.peek_number_clauses("Expected the number of clauses in the formula.") {
            Ok(num) => num,
            Err(num) => self.out_of_range_number_clauses(num),
        };
        self.consume();
        (pos, var, cls)
    }
    pub fn parse_source_header(&mut self) -> Option<(FilePosition, PathBuf)> {
        match self.peek() {
            Some(Lexeme::Header([b's', b'o', b'u', b'r', b'c', b'e', 0u8, 0u8])) => Some(()),
            _ => None,
        }?;
        let pos = self.position();
        self.consume();
        match self.peek() {
            Some(Lexeme::Quote) => self.consume(),
            _ => self.expected_quote(),
        }
        let mut new_quote = String::new();
        mem::swap(&mut self.quote, &mut new_quote);
        Some((pos, PathBuf::from(new_quote)))
    }
    pub fn parse_count_header(&mut self, forced: bool) -> Option<(FilePosition, u64)> {
        let pos = match self.peek() {
            Some(Lexeme::Header([b'c', b'o', b'u', b'n', b't', 0u8, 0u8, 0u8])) => self.position(),
            _ if !forced => None?,
            _ => self.expected_header("Expected 'p count' header.")
        };
        self.consume();
        let num = match self.peek_number_instructions("Expected the number of core and proof instructions in the ASR proof.") {
            Ok(num) => num,
            Err(num) => self.out_of_range_number_clauses(num),
        };
        self.consume();
        Some((pos, num))
    }
    pub fn parse_core_header(&mut self) -> FilePosition {
        let pos = match self.peek() {
            Some(Lexeme::Header([b'c', b'o', b'r', b'e', 0u8, 0u8, 0u8, 0u8])) => self.position(),
            _ => self.expected_header("Expected 'p core' header."),
        };
        self.consume();
        pos
    }
    pub fn parse_proof_header(&mut self) -> FilePosition {
        let pos = match self.peek() {
            Some(Lexeme::Header([b'p', b'r', b'o', b'o', b'f', 0u8, 0u8, 0u8])) => self.position(),
            _ => self.expected_header("Expected 'p proof' header."),
        };
        self.consume();
        pos
    }
    pub fn parse_premise(&mut self) -> Option<FilePosition> {
        match self.peek() {
            Some(Lexeme::Number(_)) => Some(self.position()),
            None | Some(Lexeme::Header(_)) => None,
            _ => self.expected_instruction("Expected clause or EOF."),
        }
    }
    pub fn parse_instruction(&mut self, core: bool, proof: bool) -> Option<PrepParsedInstruction> {
        let kind = match self.peek() {
            Some(Lexeme::Letter(b'r')) if proof => Some(PrepParsedInstructionKind::Rup),
            Some(Lexeme::Letter(b'd')) if proof => Some(PrepParsedInstructionKind::Del),
            Some(Lexeme::Letter(b'w')) if proof => Some(PrepParsedInstructionKind::Wsr),
            Some(Lexeme::Letter(b'k')) if core => Some(PrepParsedInstructionKind::Core),
            Some(Lexeme::Header(_)) | None => None,
            _ => {
                let s = match (core, proof) {
                    (true, true) => "Expected RUP instruction 'r', or WSR instruction 'w', or deletion instruction 'd',
                        or core clause instruction 'k', or section header, or EOF.",
                    (true, false) => "Expected core clause instruction 'k', or section header, or EOF.",
                    (false, true) => "Expected RUP instruction 'r', or WSR instruction 'w', or deletion instruction 'd',
                        or section header, or EOF.",
                    (false, false) => "Expected section header, or EOF."
                };
                self.expected_instruction(s)
            }
        }?;
        let pos = self.position();
        self.consume();
        let id = self.peek_index("Expected clause identifier.").unwrap_or_else(|num| self.out_of_range_index(num));
        self.consume();
        let ln = self.parse_file_position();
        Some(PrepParsedInstruction::new(kind, id, pos, ln))
    }
    pub fn parse_file_position(&mut self) -> Option<FilePosition> {
        match self.peek() {
            Some(Lexeme::Letter(b'l')) => Some(()),
            _ => None,
        }?;
        self.consume();
        let ln = match self.peek() {
            Some(Lexeme::Number(num)) => Some(FilePosition::from(*num)),
            _ => self.expected_file_position(),
        };
        self.consume();
        ln
    }
    pub fn save_core_instruction<W: Write>(&mut self, id: ClauseIndex, fp: FilePosition, output: &mut W) -> IoResult<()> {
        write!(output, "k {} l {} ", id.text(), fp.text())?;
        let mut clause_ps = self.parse_clause();
        while let Some(lit) = clause_ps.next() {
            write!(output, "{} ", lit.text())?;
        }
        write!(output, "0\n")
    }
    pub fn save_rup_instruction<W: Write>(&mut self, id: ClauseIndex, fp: FilePosition, output: &mut W) -> IoResult<()> {
        write!(output, "r {} l {} ", id.text(), fp.text())?;
        {
            let mut clause_ps = self.parse_clause();
            while let Some(lit) = clause_ps.next() {
                write!(output, "{} ", lit.text())?;
            }
        }
        write!(output, "0 ")?;
        {
            let mut chain_ps = self.parse_chain();
            while let Some(cid) = chain_ps.next() {
                write!(output, "{} ", cid.text())?;
            }
        }
        write!(output, "0\n")
    }
    pub fn save_wsr_instruction<W: Write>(&mut self, id: ClauseIndex, fp: FilePosition, output: &mut W) -> IoResult<()> {
        write!(output, "w {} l {} ", id.text(), fp.text())?;
        {
            let mut clause_ps = self.parse_clause();
            while let Some(lit) = clause_ps.next() {
                write!(output, "{} ", lit.text())?;
            }
        }
        write!(output, "0 ")?;
        {
            let mut witness_ps = self.parse_witness();
            while let Some((var, lit)) = witness_ps.next() {
                write!(output, "{} {} ", var.text(), lit.text())?;
            }
        }
        write!(output, "0 ")?;
        {
            let mut mchain_ps = self.parse_multichain(id);
            while let Some((lid, pchain)) = mchain_ps.next() {
                if lid != id {
                    write!(output, "{} ", lid.text())?;
                }
                match pchain {
                    Some(mut chain_ps) => {
                        while let Some(cid) = chain_ps.next() {
                            write!(output, "{} ", cid.text())?;
                        }
                        write!(output, "0 ")?;
                    },
                    None => write!(output, "d ")?,
                }
            }
        }
        write!(output, "0\n")
    }
    pub fn save_del_instruction<W: Write>(&mut self, id: ClauseIndex, fp: FilePosition, output: &mut W) -> IoResult<()> {
        write!(output, "d {} l {}\n", id.text(), fp.text())
    }
    fn peek_number_variables(&mut self, expect: &'static str) -> Result<u32, i64> {
        match self.peek() {
            Some(Lexeme::Number(num)) if *num <= Variable::MaxValue => Ok(u32::try_from(*num).unwrap()),
            Some(Lexeme::Number(num)) => Err(*num),
            _ => self.expected_number_variables(expect),
        }
    }
    fn peek_number_clauses(&mut self, expect: &'static str) -> Result<u64, i64> {
        match self.peek() {
            Some(Lexeme::Number(num)) => u64::try_from(*num).map_err(|_| *num),
            _ => self.expected_number_clauses(expect),
        }
    }
    fn peek_number_instructions(&mut self, expect: &'static str) -> Result<u64, i64> {
        match self.peek() {
            Some(Lexeme::Number(num)) => u64::try_from(*num).map_err(|_| *num),
            _ => self.expected_number_instructions(expect),
        }
    }
    fn peek_literal(&mut self, expect: &'static str) -> Result<Literal, i64> {
        match self.peek() {
            Some(Lexeme::Number(num)) => Literal::try_from(*num),
            Some(Lexeme::Letter(b't')) => Ok(Literal::Top),
            Some(Lexeme::Letter(b'f')) => Ok(Literal::Bottom),
            _ => self.expected_literal(expect),
        }
    }
    fn peek_index(&mut self, expect: &'static str) -> Result<ClauseIndex, i64> {
        match self.peek() {
            Some(Lexeme::Number(num)) => ClauseIndex::try_from(*num),
            _ => self.expected_id(expect),
        }
    }
    fn peek_variable(&mut self, expect: &'static str) -> Result<(Variable, bool), i64> {
        match self.peek() {
            Some(Lexeme::Number(num)) if num >= &0i64 => Variable::try_from(*num).map(|var| (var, true)),
            Some(Lexeme::Number(num)) => Variable::try_from(-*num).map(|var| (var, false)),
            _ => self.expected_variable(expect),
        }
    }
    fn peek(&mut self) -> &Option<Lexeme> {
        &self.cache
    }
    fn consume(&mut self) {
        match self.ignore() {
            Some(c) if (c as char).is_ascii_digit() || c == b'-' => self.read_number(c),
            Some(b'p') => self.read_header(),
            Some(c) if (c as char).is_ascii_alphabetic() => self.read_letter(c),
            Some(b'"') => self.read_quote(),
            None => self.cache = None,
            Some(c) => self.invalid_input(c),
        }
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
            Some(res) => self.cache = Some(Lexeme::Number(res)),
            None => self.out_of_range_integer(),
        }
    }
    fn read_letter(&mut self, c: u8) {
        match self.input.next() {
            Some(d) if (d as char).is_whitespace() => self.cache = Some(Lexeme::Letter(c)),
            None => self.cache = Some(Lexeme::Letter(c)),
            _ => self.not_a_letter(),
        }
    }
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
        self.cache = Some(Lexeme::Header(header))
    }
    fn read_quote(&mut self) {
        let mut state: u32 = 3u32;
        loop {
            let i = self.input.next();
            match state {
                3u32 => match i {
                    Some(b'"') => break,
                    Some(b'\\') => state -= 1u32,
                    Some(c) => self.quote.push(c as char),
                    None => self.invalid_quote(),
                },
                2u32 => {
                    state = 3u32;
                    match i {
                        Some(b't') => self.quote.push('\t'),
                        Some(b'r') => self.quote.push('\r'),
                        Some(b'n') => self.quote.push('\n'),
                        Some(b'\'') => self.quote.push('\''),
                        Some(b'\"') => self.quote.push('\"'),
                        Some(b'\\') => self.quote.push('\\'),
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
                            self.quote.push(c);
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
        self.cache = Some(Lexeme::Quote);
    }
    fn invalid_input(&mut self, c: u8) -> ! {
        panick!("invalid input" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Could not recognize character '{}'.", c as char);
        })
    }
    fn invalid_escape(&mut self, s: Option<u8>) {
        panick!("invalid escape" @ self.position.with_path(self.input.path()), lock, {
            match s {
                Some(c) => append!(lock, "Invalid escaped character '\\{}'.", c as char),
                None => append!(lock, "Unfinished escaped character."),
            }
        })
    }
    fn invalid_unicode_escape(&mut self, s: Option<u32>) {
        panick!("invalid unicode escape" @ self.position.with_path(self.input.path()), lock, {
            match s {
                Some(c) => append!(lock, "Invalid escaped Unicode character with code {:#X}.", c),
                None => append!(lock, "Unfinished escaped Unicode character."),
            }
        })
    }
    fn invalid_quote(&mut self) {
        panick!("invalid quoted string" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Unclosed quoted string.");
        })
    }
    fn out_of_range_integer(&mut self) -> ! {
        panick!("out-of-range integer" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Parsed a number outside of the 64-bit signed range [{}..{}].", i64::min_value(), i64::max_value());
        })
    }
    fn out_of_range_header(&mut self) -> ! {
        panick!("out-of-range header" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Parsed a header longer than 8 characters.");
        })
    }
    fn not_a_number(&mut self) -> ! {
        panick!("invalid integer" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Could not parse an integer.");
        })
    }
    fn not_a_letter(&mut self) -> ! {
        panick!("invalid letter" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Could not parse a letter.");
        })
    }
    fn not_a_header(&mut self) -> ! {
        panick!("invalid header" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Could not parse a 'p' header.");
        })
    }
    fn expected_header(&mut self, expect: &'static str) -> ! {
        panick!("expected header" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "{}", expect);
        })
    }
    fn expected_quote(&mut self) -> ! {
        panick!("expected text literal" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "expected a text literal starting with '\"'.");
        })
    }
    fn expected_instruction(&mut self, expect: &'static str) -> ! {
        panick!("expected instruction" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "{}", expect);
        })
    }
    fn expected_file_position(&mut self) -> ! {
        panick!("expected file position" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Expected a file position.");
        })
    }
    fn expected_literal(&mut self, expect: &'static str) -> ! {
        panick!("expected literal" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "{}", expect);
        })
    }
    fn expected_variable(&mut self, expect: &'static str) -> ! {
        panick!("expected variable" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "{}", expect);
        })
    }
    fn expected_number_variables(&mut self, expect: &'static str) -> ! {
        panick!("expected number of variables" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "{}", expect);
        })
    }
    fn expected_number_clauses(&mut self, expect: &'static str) -> ! {
        panick!("expected number of clauses" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "{}", expect);
        })
    }
    fn expected_number_instructions(&mut self, expect: &'static str) -> ! {
        panick!("expected number of core and proof instructions" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "{}", expect);
        })
    }
    fn expected_id(&mut self, expect: &'static str) -> ! {
        panick!("expected clause identifier" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "{}", expect);
        })
    }
    fn expected_index_element(&mut self, expect: &'static str) -> ! {
        panick!("expected index element" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "{}", expect);
        })
    }
    fn out_of_range_literal(&mut self, num: i64) -> ! {
        panick!("out-of-range literal" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Parsed a literal {} outside of the range [{}..{}] U [{}..{}].", num, -Literal::MaxValue, -1i64, 1i64, Literal::MaxValue);
        })
    }
    fn out_of_range_variable(&mut self, num: i64) -> ! {
        panick!("out-of-range variable" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Parsed a variable {} outside of the range [{}..{}].", num, 1i64, Variable::MaxValue);
        })
    }
    fn out_of_range_index(&mut self, num: i64) -> ! {
        panick!("out-of-range clause identifier" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Parsed a clause identifier {} outside of the range [{}..{}].", num, 1i64, ClauseIndex::MaxValue);
        })
    }
    fn out_of_range_number_variables(&mut self, num: i64) -> ! {
        panick!("out-of-range number of variables" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Parsed a number of variables {} outside of the range [{}..{}].", num, 0i64, Variable::MaxValue);
        })
    }
    fn out_of_range_number_clauses(&mut self, num: i64) -> ! {
        panick!("out-of-range number of clauses" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Parsed a number of clauses {} outside of the range [{}..{}].", num, 0usize, u64::max_value());
        })
    }
    fn out_of_range_number_instructions(&mut self, num: i64) -> ! {
        panick!("out-of-range number of instructions" @ self.position.with_path(self.input.path()), lock, {
            append!(lock, "Parsed a number of core and proof instructions {} outside of the range [{}..{}].", num, 0usize, u64::max_value());
        })
    }
    pub fn position(&self) -> FilePosition {
        self.position.clone()
    }
    pub fn path(&self) -> &Path {
        self.input.path()
    }
}

#[repr(transparent)]
pub struct TextClauseParser<'a, 'b> {
    parser: Option<&'b mut TextAsrParser<'a>>,
}
impl<'a, 'b> TextClauseParser<'a, 'b> {
    pub fn next(&mut self) -> Option<Literal> {
        let ps = self.parser.take()?;
        let litopt = match ps.peek_literal("Expected literal or end-of-clause zero.") {
            Ok(lit) => Some(lit),
            Err(0i64) => None,
            Err(x) => ps.out_of_range_literal(x),
        };
        ps.consume();
        litopt?;
        self.parser = Some(ps);
        litopt
    }
}
impl<'a, 'b> Drop for TextClauseParser<'a, 'b> {
    fn drop(&mut self) {
        while let Some(_) = self.next() {
        }
    }
}

#[repr(transparent)]
pub struct TextWitnessParser<'a, 'b> {
    parser: Option<&'b mut TextAsrParser<'a>>,
}
impl<'a, 'b> TextWitnessParser<'a, 'b> {
    pub fn next(&mut self) -> Option<(Variable, Literal)> {
        let ps = self.parser.take()?;
        let pair = match ps.peek_variable("Expected variable or end-of-witness zero.") {
            Ok(pair) => Some(pair),
            Err(0i64) => None,
            Err(x) => ps.out_of_range_variable(x),
        };
        ps.consume();
        let (var, sign) = pair?;
        let lit = match ps.peek_literal("Expected mapping literal.") {
            Ok(lit) => if sign {
                lit
            } else {
                lit.complement()
            },
            Err(x) => ps.out_of_range_literal(x),
        };
        ps.consume();
        self.parser = Some(ps);
        Some((var, lit))
    }
}
impl<'a, 'b> Drop for TextWitnessParser<'a, 'b> {
    fn drop(&mut self) {
        while let Some(_) = self.next() {
        }
    }
}

#[repr(transparent)]
pub struct TextChainParser<'a, 'b> {
    parser: Option<&'b mut TextAsrParser<'a>>,
}
impl<'a, 'b> TextChainParser<'a, 'b> {
    pub fn next(&mut self) -> Option<ClauseIndex> {
        let ps = self.parser.take()?;
        let optid = match ps.peek_index("Expected clause identifier or end-of-chain zero.") {
            Ok(id) => Some(id),
            Err(0i64) => None,
            Err(x) => ps.out_of_range_index(x),
        };
        ps.consume();
        optid?;
        self.parser = Some(ps);
        optid
    }
}
impl<'a, 'b> Drop for TextChainParser<'a, 'b> {
    fn drop(&mut self) {
        while let Some(_) = self.next() {
        }
    }
}

pub struct TextMultichainParser<'a, 'b> {
    parser: Option<&'b mut TextAsrParser<'a>>,
    id: Option<ClauseIndex>,
}
impl<'a, 'b> TextMultichainParser<'a, 'b> {
    pub fn next<'c>(&'c mut self) -> Option<(ClauseIndex, Option<TextChainParser<'a, 'c>>)> where 'b: 'c {
        let ps = self.parser.take()?;
        let (lid, del) = if self.id.is_none() {
            let optlid = match ps.peek_index("Expected lateral clause identifier.") {
                Ok(id) => Some(id),
                Err(0i64) => None,
                Err(x) => ps.out_of_range_index(x),
            };
            ps.consume();
            let lid = optlid?;
            let del = if let Some(Lexeme::Letter(b'd')) = ps.peek() {
                ps.consume();
                true
            } else {
                false
            };
            (lid, del)
        } else {
            (self.id.take().unwrap(), false)
        };
        self.parser = Some(ps);
        Some((lid, if del {
            None
        } else {
            Some(TextChainParser::<'a, 'c> { parser: Some(&mut *self.parser.as_mut().unwrap()) })
        }))
    }
}
impl<'a, 'b> Drop for TextMultichainParser<'a, 'b> {
    fn drop(&mut self) {
        while let Some(_) = self.next() {
        }
    }
}

#[cfg(test)]
pub mod test {
    use std::{
        fs::{File},
        path::{Path},
        convert::{TryFrom},
        panic::{self},
    };
	use crate::{
        textparser::{TextAsrParser, PrepParsedInstructionKind},
        io::{InputReader, PrintedPanic},
        basic::{Literal, ClauseIndex, Variable},
    };
    
    fn parsing_test(path: &str) {
        let file = File::open(Path::new(path)).unwrap();
        let mut parser = TextAsrParser::new(InputReader::new(file, &Path::new(path), false));
        let (_, var, cls) = parser.parse_cnf_header();
        assert!(var == 50u32);
        assert!(cls == 80u64);
        assert!(parser.parse_premise().unwrap().offset() == Some(12u64));
        {
            let mut ps = parser.parse_clause();
            assert!(ps.next() == Some(Literal::try_from(-16i64).unwrap()));
            assert!(ps.next() == Some(Literal::try_from(32i64).unwrap()));
            assert!(ps.next() == Some(Literal::try_from(2147483647i64).unwrap()));
            assert!(ps.next() == Some(Literal::try_from(-2147483647i64).unwrap()));
            assert!(ps.next() == None);
        }
        assert!(parser.parse_premise().unwrap().offset() == Some(14u64));
        {
            let mut ps = parser.parse_clause();
            assert!(ps.next() == Some(Literal::try_from(180i64).unwrap()));
            assert!(ps.next() == Some(Literal::try_from(20i64).unwrap()));
            assert!(ps.next() == Some(Literal::try_from(-1i64).unwrap()));
            assert!(ps.next() == None);
        }
        assert!(parser.parse_premise().unwrap().offset() == Some(15u64));
        {
            let mut ps = parser.parse_clause();
            assert!(ps.next() == None);
        }
        assert!(parser.parse_premise().is_none());
        let (pos, buf) = parser.parse_source_header().unwrap();
        assert!(pos.offset() == Some(15u64));
        assert!(buf.to_str().unwrap() == "Unicode \"test\"\n\t\\\u{004C}\u{20CF}\u{292A}\u{1F780}\u{1F600}");
        // assert!(parser.parse_source_header().is_none());
        assert!(parser.parse_core_header().offset() == Some(15u64));
        let ins = parser.parse_instruction(true, false).unwrap();
        assert!(ins.index() == &ClauseIndex::try_from(1i64).unwrap());
        assert!(ins.kind() == &PrepParsedInstructionKind::Core);
        assert!(ins.ofp.offset() == Some(15u64));
        assert!(ins.dfp.is_none());
        {
            let mut ps = parser.parse_clause();
            assert!(ps.next() == Some(Literal::try_from(42i64).unwrap()));
            assert!(ps.next() == Some(Literal::try_from(-31i64).unwrap()));
            assert!(ps.next() == Some(Literal::Bottom));
            assert!(ps.next() == None);
        }
        let ins = parser.parse_instruction(true, false).unwrap();
        assert!(ins.index() == &ClauseIndex::try_from(38i64).unwrap());
        assert!(ins.kind() == &PrepParsedInstructionKind::Core);
        assert!(ins.ofp.offset() == Some(15u64));
        assert!(ins.dfp.unwrap().offset() == Some(5u64 >> 2));
        {
            let mut ps = parser.parse_clause();
            assert!(ps.next() == Some(Literal::Top));
            assert!(ps.next() == Some(Literal::try_from(18i64).unwrap()));
            assert!(ps.next() == Some(Literal::try_from(-1i64).unwrap()));
            assert!(ps.next() == None);  
        }
        assert!(parser.parse_instruction(true, false).is_none());
        assert!(parser.parse_proof_header().offset() == Some(15u64));
        let ins = parser.parse_instruction(false, true).unwrap();
        assert!(ins.index() == &ClauseIndex::try_from(23i64).unwrap());
        assert!(ins.kind() == &PrepParsedInstructionKind::Rup);
        assert!(ins.ofp.offset() == Some(16u64));
        assert!(ins.dfp.is_none());
        {
            let mut ps = parser.parse_clause();
            assert!(ps.next() == Some(Literal::try_from(1i64).unwrap()));
            assert!(ps.next() == Some(Literal::try_from(8i64).unwrap()));
            assert!(ps.next() == Some(Literal::try_from(-3i64).unwrap()));
            assert!(ps.next() == None);
        }
        {
            let mut ps = parser.parse_chain();
            assert!(ps.next() == Some(ClauseIndex::try_from(1i64).unwrap()));
            assert!(ps.next() == Some(ClauseIndex::try_from(2i64).unwrap()));
            assert!(ps.next() == Some(ClauseIndex::try_from(3i64).unwrap()));
            assert!(ps.next() == None);
        }
        let ins = parser.parse_instruction(false, true).unwrap();
        assert!(ins.index() == &ClauseIndex::try_from(4294967295i64).unwrap());
        assert!(ins.kind() == &PrepParsedInstructionKind::Wsr);
        assert!(ins.ofp.offset() == Some(16u64));
        assert!(ins.dfp.unwrap().offset() == Some(1996u64 >> 2));
        {
            let mut ps = parser.parse_witness();
            assert!(ps.next() == Some((Variable::try_from(2147483647i64).unwrap(), Literal::try_from(-11i64).unwrap())));
            assert!(ps.next() == Some((Variable::try_from(22i64).unwrap(), Literal::try_from(-22i64).unwrap())));
            assert!(ps.next() == Some((Variable::try_from(17i64).unwrap(), Literal::Bottom)));
            assert!(ps.next() == Some((Variable::try_from(23i64).unwrap(), Literal::Top)));
            assert!(ps.next() == None);
        }
        {
            let mut mc = parser.parse_multichain(ClauseIndex::try_from(4294967295i64).unwrap());
            {
                let (id, chain) = mc.next().unwrap();
                assert!(id == ClauseIndex::try_from(4294967295i64).unwrap());
                let mut ps = chain.unwrap();
                assert!(ps.next() == Some(ClauseIndex::try_from(1i64).unwrap()));
                assert!(ps.next() == Some(ClauseIndex::try_from(2i64).unwrap()));
                assert!(ps.next() == Some(ClauseIndex::try_from(3i64).unwrap()));
                assert!(ps.next() == None);
            }
            {
                let (id, chain) = mc.next().unwrap();
                assert!(id == ClauseIndex::try_from(32i64).unwrap());
                let mut ps = chain.unwrap();
                assert!(ps.next() == Some(ClauseIndex::try_from(64i64).unwrap()));
                assert!(ps.next() == Some(ClauseIndex::try_from(128i64).unwrap()));
                assert!(ps.next() == Some(ClauseIndex::try_from(256).unwrap()));
                assert!(ps.next() == None);
            }
            {
                let (id, chain) = mc.next().unwrap();
                assert!(id == ClauseIndex::try_from(58i64).unwrap());
                assert!(chain.is_none());
            }
            assert!(mc.next().is_none());
        }
        let (pos, count) = parser.parse_count_header(true).unwrap();
        assert!(pos.offset() == Some(17u64));
        assert!(count == 4096u64);
        let ins = parser.parse_instruction(false, true).unwrap();
        assert!(ins.index() == &ClauseIndex::try_from(18i64).unwrap());
        assert!(ins.kind() == &PrepParsedInstructionKind::Del);
        assert!(ins.ofp.offset() == Some(18u64));
        assert!(ins.dfp.is_none());
        let ins = parser.parse_instruction(false, true).unwrap();
        assert!(ins.index() == &ClauseIndex::try_from(23i64).unwrap());
        assert!(ins.kind() == &PrepParsedInstructionKind::Del);
        assert!(ins.ofp.offset() == Some(18u64));
        assert!(ins.dfp.is_none());
        let ins = parser.parse_instruction(false, true).unwrap();
        assert!(ins.index() == &ClauseIndex::try_from(19i64).unwrap());
        assert!(ins.kind() == &PrepParsedInstructionKind::Del);
        assert!(ins.ofp.offset() == Some(19u64));
        assert!(ins.dfp.is_none());
    }

    fn parsing_catch_panic(path: &str, expected: &str) -> Option<bool> {
        match panic::catch_unwind(|| parsing_test(path)) {
            Ok(()) => None,
            Err(pain) => {
                match pain.downcast::<PrintedPanic>() {
                    Ok(pp) => Some(pp.check(expected)),
                    Err(_) => panic!("unexpected error"),
                }
            }
        }
    }

	#[test]
	fn test_parser() {
        assert!(parsing_catch_panic("test/text_parser/text_asr_sample.txt", "") == None);
        assert!(parsing_catch_panic("test/text_parser/invalid_input.txt", "invalid input") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/oor_integer.txt", "out-of-range integer") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/oor_header.txt", "out-of-range header") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/invalid_integer.txt", "invalid integer") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/invalid_letter.txt", "invalid letter") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/invalid_header1.txt", "invalid header") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/invalid_header2.txt", "invalid header") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/invalid_string.txt", "invalid quoted string") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/expected_header1.txt", "expected header") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/expected_header2.txt", "expected header") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/expected_instruction1.txt", "expected instruction") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/expected_instruction2.txt", "expected instruction") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/expected_literal.txt", "expected literal") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/expected_variable.txt", "expected variable") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/expected_number_variables.txt", "expected number of variables") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/expected_number_clauses.txt", "expected number of clauses") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/expected_id.txt", "expected clause identifier") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/oor_literal1.txt", "out-of-range literal") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/oor_literal2.txt", "out-of-range literal") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/oor_variable1.txt", "out-of-range variable") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/oor_variable2.txt", "out-of-range variable") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/oor_index.txt", "out-of-range clause identifier") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/oor_number_variables.txt", "out-of-range number of variables") == Some(true));
    }
}