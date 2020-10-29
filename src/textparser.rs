use std::{
    convert::{TryFrom},
};

use crate::{
    io::{InputReader, FilePositionRef},
    basic::{Literal, Variable, ClauseIndex, MaybeVariable, InstructionNumber, InstructionNumberKind},
};

#[derive(Copy, Clone, Debug)]
enum Lexeme {
    Number(i64),
    Letter(u8),
    Header([u8; 8]),
}

#[derive(PartialEq, Eq)]
pub enum AsrParsedInstructionKind {
    Rup,
    Wsr,
    Del,
}

#[derive(PartialEq, Eq)]
pub struct AsrParsedInstruction {
    pub kind: AsrParsedInstructionKind,
    pub id: ClauseIndex,
    pub num: InstructionNumber,
}

pub struct TextAsrParser<'a> {
    input: InputReader<'a>,
    position: FilePositionRef<'a>,
    cache: Option<Lexeme>,
}
impl<'a> TextAsrParser<'a> {
    pub fn new(input: InputReader<'a>) -> TextAsrParser<'a> {
        let pos = input.position();
        let mut parser = TextAsrParser::<'a> {
            input: input,
            position: pos,
            cache: None,
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
    pub fn parse_cnf_header(&mut self) -> (FilePositionRef<'a>, MaybeVariable, usize) {
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
    pub fn parse_core_header(&mut self) -> FilePositionRef<'a> {
        let pos = match self.peek() {
            Some(Lexeme::Header([b'c', b'o', b'r', b'e', 0u8, 0u8, 0u8, 0u8])) => self.position(),
            _ => self.expected_header("Expected 'p core' header."),
        };
        self.consume();
        pos
    }
    pub fn parse_proof_header(&mut self) -> FilePositionRef<'a> {
        let pos = match self.peek() {
            Some(Lexeme::Header([b'p', b'r', b'o', b'o', b'f', 0u8, 0u8, 0u8])) => self.position(),
            _ => self.expected_header("Expected 'p proof' header."),
        };
        self.consume();
        pos
    }
    pub fn parse_premise(&mut self) -> Option<FilePositionRef<'a>> {
        match self.peek() {
            Some(Lexeme::Number(_)) => Some(self.position()),
            None | Some(Lexeme::Header(_)) => None,
            _ => self.expected_instruction("Expected clause or EOF."),
        }
    }
    pub fn parse_core(&mut self) -> Option<(ClauseIndex, InstructionNumber, FilePositionRef<'a>)> {
        match self.peek() {
            Some(Lexeme::Letter(b'k')) => {
                self.consume();
                let id = match self.peek_index("Expected clause identifier.") {
                    Ok(id) => id,
                    Err(num) => self.out_of_range_index(num),
                };
                let pos = self.position();
                self.consume();
                let num = self.read_instruction_number(InstructionNumberKind::Core);
                Some((id, num, pos))
            },
            Some(Lexeme::Header(_)) | None => None,
            _ => self.expected_instruction("Expected core instruction 'k', or section header, or EOF."),
        }
    }
    pub fn parse_instruction(&mut self) -> Option<(AsrParsedInstruction, FilePositionRef<'a>)> {
        let kind = match self.peek() {
            Some(Lexeme::Letter(b'r')) => AsrParsedInstructionKind::Rup,
            Some(Lexeme::Letter(b'w')) => AsrParsedInstructionKind::Wsr,
            Some(Lexeme::Letter(b'd')) => AsrParsedInstructionKind::Del,
            Some(Lexeme::Header(_)) | None => return None,
            _ => self.expected_instruction("Expected RUP instruction 'r', or WSR instruction 'w', or deletion instruction 'd', or section header, or EOF."),
        };
        let pos = self.position();
        self.consume();
        let id = self.peek_index("Expected clause identifier.").unwrap_or_else(|num| self.out_of_range_index(num));
        self.consume();
        let num = self.read_instruction_number(InstructionNumberKind::Proof);
        Some((AsrParsedInstruction {
            kind: kind,
            id: id,
            num: num,
        }, pos))
    }
    fn read_instruction_number(&mut self, kind: InstructionNumberKind) -> InstructionNumber {
        match self.peek() {
            Some(Lexeme::Letter(b'l')) => self.consume(),
            _ => return InstructionNumber::new(kind),
        }
        match self.peek() {
            Some(Lexeme::Number(num)) => match InstructionNumber::try_from((*num, kind)) {
                Ok(n) => {
                    self.consume();
                    n
                },
                Err(n) => self.out_of_range_instruction_number(n),
            },
            _ => self.expected_instruction_number(),
        }
    }
    fn peek_number_variables(&mut self, expect: &'static str) -> Result<MaybeVariable, i64> {
        match self.peek() {
            Some(Lexeme::Number(num)) => MaybeVariable::try_from(*num),
            _ => self.expected_number_variables(expect),
        }
    }
    fn peek_number_clauses(&mut self, expect: &'static str) -> Result<usize, i64> {
        match self.peek() {
            Some(Lexeme::Number(num)) => usize::try_from(*num).map_err(|_| *num),
            _ => self.expected_number_clauses(expect),
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
            _ => self.expected_index(expect),
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
    fn invalid_input(&mut self, c: u8) -> ! {
        panick!("invalid input" @ self.position, lock, {
            append!(lock, "Could not recognize character '{}'.", c);
        })
    }
    fn out_of_range_integer(&mut self) -> ! {
        panick!("out-of-range integer" @ self.position, lock, {
            append!(lock, "Parsed a number outside of the 64-bit signed range [{}..{}].", i64::min_value(), i64::max_value());
        })
    }
    fn out_of_range_header(&mut self) -> ! {
        panick!("out-of-range header" @ self.position, lock, {
            append!(lock, "Parsed a header longer than 8 characters.");
        })
    }
    fn not_a_number(&mut self) -> ! {
        panick!("invalid integer" @ self.position, lock, {
            append!(lock, "Could not parse an integer.");
        })
    }
    fn not_a_letter(&mut self) -> ! {
        panick!("invalid letter" @ self.position, lock, {
            append!(lock, "Could not parse a letter.");
        })
    }
    fn not_a_header(&mut self) -> ! {
        panick!("invalid header" @ self.position, lock, {
            append!(lock, "Could not parse a 'p' header.");
        })
    }
    fn expected_header(&mut self, expect: &'static str) -> ! {
        panick!("expected header" @ self.position, lock, {
            append!(lock, "{}", expect);
        })
    }
    fn expected_instruction(&mut self, expect: &'static str) -> ! {
        panick!("expected instruction" @ self.position, lock, {
            append!(lock, "{}", expect);
        })
    }
    fn expected_instruction_number(&mut self) -> ! {
        panick!("expected instruction number" @ self.position, lock, {
            append!(lock, "Expected the corresponding instruction number in the unprocessed ASR proof file.");
        })
    }
    fn expected_literal(&mut self, expect: &'static str) -> ! {
        panick!("expected literal" @ self.position, lock, {
            append!(lock, "{}", expect);
        })
    }
    fn expected_variable(&mut self, expect: &'static str) -> ! {
        panick!("expected variable" @ self.position, lock, {
            append!(lock, "{}", expect);
        })
    }
    fn expected_number_variables(&mut self, expect: &'static str) -> ! {
        panick!("expected number of variables" @ self.position, lock, {
            append!(lock, "{}", expect);
        })
    }
    fn expected_number_clauses(&mut self, expect: &'static str) -> ! {
        panick!("expected number of clauses" @ self.position, lock, {
            append!(lock, "{}", expect);
        })
    }
    fn expected_index(&mut self, expect: &'static str) -> ! {
        panick!("expected clause identifier" @ self.position, lock, {
            append!(lock, "{}", expect);
        })
    }
    fn out_of_range_instruction_number(&mut self, num: i64) -> ! {
        panick!("out-of-range instruction number" @ self.position, lock, {
            append!(lock, "Parsed an instruction number {} outside of the range [{}..{}].", num, 1i64, InstructionNumber::MaxValue);
        })
    }
    fn out_of_range_literal(&mut self, num: i64) -> ! {
        panick!("out-of-range literal" @ self.position, lock, {
            append!(lock, "Parsed a literal {} outside of the range [{}..{}] U [{}..{}].", num, -Literal::MaxValue, -1i64, 1i64, Literal::MaxValue);
        })
    }
    fn out_of_range_variable(&mut self, num: i64) -> ! {
        panick!("out-of-range variable" @ self.position, lock, {
            append!(lock, "Parsed a variable {} outside of the range [{}..{}].", num, 1i64, Variable::MaxValue);
        })
    }
    fn out_of_range_index(&mut self, num: i64) -> ! {
        panick!("out-of-range clause identifier" @ self.position, lock, {
            append!(lock, "Parsed a clause identifier {} outside of the range [{}..{}].", num, 1i64, ClauseIndex::MaxValue);
        })
    }
    fn out_of_range_number_variables(&mut self, num: i64) -> ! {
        panick!("out-of-range number of variables" @ self.position, lock, {
            append!(lock, "Parsed a number of variables {} outside of the range [{}..{}].", num, 0i64, Variable::MaxValue);
        })
    }
    fn out_of_range_number_clauses(&mut self, num: i64) -> ! {
        panick!("out-of-range number of clauses" @ self.position, lock, {
            append!(lock, "Parsed a number of clauses {} outside of the range [{}..{}].", num, 0usize, usize::max_value());
        })
    }
    pub fn position(&self) -> FilePositionRef<'a> {
        self.position.clone()
    }
}

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
        textparser::{TextAsrParser, AsrParsedInstruction, AsrParsedInstructionKind},
        io::{InputReader, PrintedPanic},
        basic::{MaybeVariable, Literal, ClauseIndex, Variable, InstructionNumber, InstructionNumberKind},
    };
    
    fn parsing_test(path: &str) {
        let file = File::open(Path::new(path)).unwrap();
        let mut parser = TextAsrParser::new(InputReader::new(file, &Path::new(path), false));
        let (_, var, cls) = parser.parse_cnf_header();
        assert!(var == MaybeVariable::try_from(50i64).unwrap());
        assert!(cls == 80usize);
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
        assert!(parser.parse_core_header().offset() == Some(15u64));
        let (id, num, pos) = parser.parse_core().unwrap();
        assert!(num == InstructionNumber::new(InstructionNumberKind::Core));
        assert!(id == ClauseIndex::try_from(1i64).unwrap());
        assert!(pos.offset() == Some(15u64));
        {
            let mut ps = parser.parse_clause();
            assert!(ps.next() == Some(Literal::try_from(42i64).unwrap()));
            assert!(ps.next() == Some(Literal::try_from(-31i64).unwrap()));
            assert!(ps.next() == Some(Literal::Bottom));
            assert!(ps.next() == None);
        }
        let (id, num, pos) = parser.parse_core().unwrap();
        assert!(num == InstructionNumber::new(InstructionNumberKind::Core));
        assert!(id == ClauseIndex::try_from(38i64).unwrap());
        assert!(pos.offset() == Some(15u64));
        {
            let mut ps = parser.parse_clause();
            assert!(ps.next() == Some(Literal::Top));
            assert!(ps.next() == Some(Literal::try_from(18i64).unwrap()));
            assert!(ps.next() == Some(Literal::try_from(-1i64).unwrap()));
            assert!(ps.next() == None);
        }
        assert!(parser.parse_core().is_none());
        assert!(parser.parse_proof_header().offset() == Some(15u64));
        let (ins, pos) = parser.parse_instruction().unwrap();
        assert!(ins == AsrParsedInstruction {
            kind: AsrParsedInstructionKind::Rup,
            id: ClauseIndex::try_from(23i64).unwrap(),
            num: InstructionNumber::new(InstructionNumberKind::Proof),
        });
        assert!(pos.offset() == Some(16u64));
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
        let (ins, pos) = parser.parse_instruction().unwrap();
        assert!(ins == AsrParsedInstruction {
            kind: AsrParsedInstructionKind::Wsr,
            id: ClauseIndex::try_from(4294967295i64).unwrap(),
            num: InstructionNumber::new(InstructionNumberKind::Proof),
        });
        assert!(pos.offset() == Some(16u64));
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
                let (id, mut chain) = mc.next().unwrap();
                assert!(id == ClauseIndex::try_from(32i64).unwrap());
                let mut ps = chain.unwrap();
                assert!(ps.next() == Some(ClauseIndex::try_from(64i64).unwrap()));
                assert!(ps.next() == Some(ClauseIndex::try_from(128i64).unwrap()));
                assert!(ps.next() == Some(ClauseIndex::try_from(256).unwrap()));
                assert!(ps.next() == None);
            }
            {
                let (id, mut chain) = mc.next().unwrap();
                assert!(id == ClauseIndex::try_from(58i64).unwrap());
                assert!(chain.is_none());
            }
            assert!(mc.next().is_none());
        }
        let (ins, pos) = parser.parse_instruction().unwrap();
        assert!(ins == AsrParsedInstruction {
            kind: AsrParsedInstructionKind::Del,
            id: ClauseIndex::try_from(18i64).unwrap(),
            num: InstructionNumber::new(InstructionNumberKind::Proof),
        });
        assert!(pos.offset() == Some(18u64));
        let (ins, pos) = parser.parse_instruction().unwrap();
        assert!(ins == AsrParsedInstruction {
            kind: AsrParsedInstructionKind::Del,
            id: ClauseIndex::try_from(23i64).unwrap(),
            num: InstructionNumber::new(InstructionNumberKind::Proof),
        });
        assert!(pos.offset() == Some(18u64));
        let (ins, pos) = parser.parse_instruction().unwrap();
        assert!(ins == AsrParsedInstruction {
            kind: AsrParsedInstructionKind::Del,
            id: ClauseIndex::try_from(19i64).unwrap(),
            num: InstructionNumber::new(InstructionNumberKind::Proof),
        });
        assert!(pos.offset() == Some(19u64));
        assert!(parser.parse_instruction().is_none());
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
        assert!(parsing_catch_panic("test/text_parser/expected_header.txt", "expected header") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/expected_instruction1.txt", "expected instruction") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/expected_instruction2.txt", "expected instruction") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/expected_literal.txt", "expected literal") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/expected_variable.txt", "expected variable") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/expected_number_variables.txt", "expected number of variables") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/expected_number_clauses.txt", "expected number of clauses") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/expected_index.txt", "expected clause identifier") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/oor_literal1.txt", "out-of-range literal") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/oor_literal2.txt", "out-of-range literal") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/oor_variable1.txt", "out-of-range variable") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/oor_variable2.txt", "out-of-range variable") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/oor_index1.txt", "out-of-range clause identifier") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/oor_index2.txt", "out-of-range clause identifier") == Some(true));
        assert!(parsing_catch_panic("test/text_parser/oor_number_variables.txt", "out-of-range number of variables") == Some(true));
    }
}