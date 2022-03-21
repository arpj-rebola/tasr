use std::{
    path::{PathBuf},
    intrinsics::{self},
};

use crate::{
    io::{FilePosition, DeferredFilePosition, FilePath},
    basic::{Literal, Variable, ClauseIndex},
    lexer::{AsrLexer, Lexeme},
};

#[derive(PartialEq)]
pub enum ParsedInstructionKind {
    Core,
    Rup,
    Wsr,
    Del,
}

pub struct ParsedInstruction {
    kind: ParsedInstructionKind,
    id: ClauseIndex,
    pos: DeferredFilePosition,
}
impl ParsedInstruction {
    #[inline(always)]
    fn new(kind: ParsedInstructionKind, id: ClauseIndex, pos: DeferredFilePosition) -> ParsedInstruction {
        ParsedInstruction {
            kind: kind,
            id: id,
            pos: pos,
        }
    }
    #[inline(always)]
    pub fn kind(&self) -> &ParsedInstructionKind {
        &self.kind
    }
    #[inline(always)]
    pub fn index(&self) -> &ClauseIndex {
        &self.id
    }
    pub fn position(&self) -> &DeferredFilePosition {
        &self.pos
    }
}

pub struct AsrParser<L: AsrLexer> {
    lexer: L,
    source: FilePath,
}
impl<L: AsrLexer> AsrParser<L> {
    pub fn new(lexer: L) -> AsrParser<L> {
        let mut p = AsrParser::<L> {
            lexer: lexer,
            source: FilePath::unknown(),
        };
        if let Some((_, src)) = p.parse_source_header() {
            p.source = FilePath::new(PathBuf::from(src));
        }
        p
    }
    pub fn parse_cnf_header(&mut self) -> (FilePosition, u32, u64) {
        let pos = match self.lexer.peek().as_this_header([b'c', b'n', b'f', 0u8, 0u8, 0u8, 0u8, 0u8]) {
            Ok(()) => self.lexer.position().clone(),
            Err(lx) => self.expected_cnf_header(lx),
        };
        self.lexer.consume();
        let var = self.lexer.peek().as_nvariables().unwrap_or_else(|lx| self.expected_number_variables(lx));
        self.lexer.consume();
        let cls = self.lexer.peek().as_nclauses().unwrap_or_else(|lx| self.expected_number_clauses(lx));
        self.lexer.consume();
        (pos, var, cls)
    }
    pub fn parse_core_header(&mut self) -> FilePosition {
        let pos = match self.lexer.peek().as_this_header([b'c', b'o', b'r', b'e', 0u8, 0u8, 0u8, 0u8]) {
            Ok(()) => self.lexer.position().clone(),
            Err(lx) => self.expected_core_header(lx),
        };
        self.lexer.consume();
        pos
    }
    pub fn parse_proof_header(&mut self) -> FilePosition {
        let pos = match self.lexer.peek().as_this_header([b'p', b'r', b'o', b'o', b'f', 0u8, 0u8, 0u8]) {
            Ok(()) => self.lexer.position().clone(),
            Err(lx) => self.expected_proof_header(lx),
        };
        self.lexer.consume();
        pos
    }
    pub fn parse_source_header(&mut self) -> Option<(FilePosition, String)> {
        let pos = match self.lexer.peek().as_this_header([b's', b'o', b'u', b'r', b'c', b'e', 0u8, 0u8]) {
            Ok(()) => self.lexer.position().clone(),
            Err(_) => None?,
        };
        self.lexer.consume();
        let src = match self.lexer.peek().as_quote() {
            Ok(bx) => *bx.clone(),
            Err(lx) => self.expected_source_header(lx),
        };
        self.lexer.consume();
        Some((pos, src))
    }
    pub fn parse_count_header(&mut self, forced: bool) -> Option<(FilePosition, u64)> {
        let pos = match self.lexer.peek().as_this_header([b'c', b'o', b'u', b'n', b't', 0u8, 0u8, 0u8]) {
            Ok(()) => self.lexer.position().clone(),
            Err(_) if !forced => None?,
            Err(lx) => self.expected_count_header(lx),
        };
        self.lexer.consume();
        let num = match self.lexer.peek().as_nclauses() {
            Ok(num) => num,
            Err(lx) => self.expected_number_instructions(lx),
        };
        self.lexer.consume();
        Some((pos, num))
    }
    pub fn parse_premise(&mut self) -> Option<FilePosition> {
        match self.lexer.peek() {
            Lexeme::Number(_) => Some(self.lexer.position().clone()),
            Lexeme::Eof => None,
            lx => self.expected_premise(lx),
        }
    }
    pub fn parse_instruction(&mut self, core: bool, proof: bool) -> Option<ParsedInstruction> {
        let kind = match self.lexer.peek() {
            Lexeme::Letter(b'r') if proof => Some(ParsedInstructionKind::Rup),
            Lexeme::Letter(b'd') if proof => Some(ParsedInstructionKind::Del),
            Lexeme::Letter(b'w') if proof => Some(ParsedInstructionKind::Wsr),
            Lexeme::Letter(b'k') if core => Some(ParsedInstructionKind::Core),
            Lexeme::Header(_) | Lexeme::Eof => None,
            lx => self.expected_instruction(core, proof, lx),
        }?;
        let pos = self.lexer.position().clone();
        self.lexer.consume();
        let id = self.lexer.peek().as_index().unwrap_or_else(|lx| self.expected_instruction_id(lx));
        self.lexer.consume();
        let dpos = if let Lexeme::Letter(b'l') = self.lexer.peek() {
            self.lexer.consume();
            let off = self.lexer.peek().as_file_offset().unwrap_or_else(|lx| self.expected_file_position(lx));
            self.lexer.consume();
            let def = {
                let mut def = FilePosition::new(pos.binary_file(), self.source.clone());
                def.set_offset(off);
                def
            };
            DeferredFilePosition::new_derived(pos, def)
        } else {
            DeferredFilePosition::new_origin(pos)
        };
        Some(ParsedInstruction::new(kind, id, dpos))
    }
    #[inline]
    pub fn parse_clause(&mut self) -> AsrClauseParser<'_, L> {
        AsrClauseParser::<L> {
            parser: &mut *self,
            locked: false,
        }
    }
    #[inline]
    pub fn parse_chain(&mut self) -> AsrChainParser<'_, L> {
        AsrChainParser::<L> {
            parser: &mut *self,
            locked: false,
        }
    }
    #[inline]
    pub fn parse_witness(&mut self) -> AsrWitnessParser<'_, L> {
        AsrWitnessParser::<L> {
            parser: &mut *self,
            locked: false,
        }
    }
    #[inline]
    pub fn parse_multichain(&mut self, id: ClauseIndex) -> AsrMultichainParser<'_, L> {
        AsrMultichainParser::<L> {
            parser: &mut *self,
            id: Some(id),
            locked: false,
        }
    }
    #[inline]
    pub fn position(&self) -> &FilePosition {
        self.lexer.position()
    }
    fn expected_cnf_header(&self, lx: &Lexeme) -> ! {
        panick!("expected header" @ self.lexer.position(), lock, {
            append!(lock, "Expected 'cnf' header, but found {} instead.", lx);
        })
    }
    fn expected_core_header(&self, lx: &Lexeme) -> ! {
        panick!("expected header" @ self.lexer.position(), lock, {
            append!(lock, "Expected 'core' header, but found {} instead.", lx);
        })
    }
    fn expected_proof_header(&self, lx: &Lexeme) -> ! {
        panick!("expected header" @ self.lexer.position(), lock, {
            append!(lock, "Expected 'proof' header, but found {} instead.", lx);
        })
    }
    fn expected_source_header(&self, lx: &Lexeme) -> ! {
        panick!("expected source string" @ self.lexer.position(), lock, {
            append!(lock, "Expected source string in 'source' header, but found {} instead.", lx);
        })
    }
    fn expected_count_header(&self, lx: &Lexeme) -> ! {
        panick!("expected count header" @ self.lexer.position(), lock, {
            append!(lock, "Expected 'count' header, but found {} instead.", lx);
        })
    }
    fn expected_number_variables(&self, lx: &Lexeme) -> ! {
        panick!("expected number of variables" @ self.lexer.position(), lock, {
            match lx {
                Lexeme::Number(num) => append!(lock, "Parsed a number of variables {} outside of the range [{}..{}]", num, 0i64, Variable::MaxValue),
                _ => append!(lock, "Expected the number of variables in the formula, but found {} instead.", lx),
            }
        })
    }
    fn expected_number_clauses(&self, lx: &Lexeme) -> ! {
        panick!("expected number of clauses" @ self.lexer.position(), lock, {
            match lx {
                Lexeme::Number(num) => append!(lock, "Parsed a number of clauses {} outside of the range [{}..{}].", num, 0usize, u64::max_value()),
                _ => append!(lock, "Expected the number of clauses in the formula, but found {} instead.", lx),
            }
        })
    }
    fn expected_number_instructions(&self, lx: &Lexeme) -> ! {
        panick!("expected number of instructions" @ self.lexer.position(), lock, {
            match lx {
                Lexeme::Number(num) => append!(lock, "Parsed a number of instructions {} outside of the range [{}..{}].", num, 0usize, u64::max_value()),
                _ => append!(lock, "Expected the number of instructions in the proof, but found {} instead.", lx),
            }
        })
    }
    fn expected_premise(&self, lx: &Lexeme) -> ! {
        panick!("expected premise clause" @ self.lexer.position(), lock, {
            append!(lock, "Expected a new premise clause or EOF, but found {} instead.", lx);
        })
    }
    fn expected_instruction(&self, core: bool, proof: bool, lx: &Lexeme) -> ! {
        panick!("expected proof instruction" @ self.lexer.position(), lock, {
            let msg = match (core, proof) {
                (true, true) => "Expected RUP instruction 'r', or WSR instruction 'w', or deletion instruction 'd',
                    or core clause instruction 'k', or section header, or EOF",
                (true, false) => "Expected core clause instruction 'k', or section header, or EOF",
                (false, true) => "Expected RUP instruction 'r', or WSR instruction 'w', or deletion instruction 'd',
                    or section header, or EOF",
                (false, false) => "Expected section header, or EOF"
            };
            append!(lock, "{}, but found {} instead.", msg, lx);
        })
    }
    fn expected_instruction_id(&self, lx: &Lexeme) -> ! {
        panick!("expected proof instruction identifier" @ self.lexer.position(), lock, {
            match lx {
                Lexeme::Number(num) => append!(lock, "Parsed a clause identifier {} outside of the range [{}..{}]", num, 1i64, ClauseIndex::MaxValue),
                _ => append!(lock, "Expected a clause identifier for a proof instruction, but found {} instead.", lx),
            }
        })
    }
    fn expected_file_position(&self, lx: &Lexeme) -> ! {
        panick!("expected proof instruction position" @ self.lexer.position(), lock, {
            append!(lock, "Expected a file position for a proof instruction, but found {} instead.", lx);
        })
    }
    fn expected_literal(&self, lx: &Lexeme) -> ! {
        panick!("expected literal" @ self.lexer.position(), lock, {
            match lx {
                Lexeme::Number(num) => append!(lock, "Parsed a literal {} outside of the range [{}..{}] U [{}..{}].", num, -Literal::MaxValue, -1i64, 1i64, Literal::MaxValue),
                _ => append!(lock, "Expected a literal, but found {} instead.", lx),
            }
        })
    }
    fn expected_variable(&self, lx: &Lexeme) -> ! {
        panick!("expected literal" @ self.lexer.position(), lock, {
            match lx {
                Lexeme::Number(num) => append!(lock, "Parsed a variable {} outside of the range [{}..{}].", num, 1i64, Variable::MaxValue),
                _ => append!(lock, "Expected a variable, but found {} instead.", lx),
            }
        })
    }
    fn expected_index(&self, lx: &Lexeme) -> ! {
        panick!("expected clause identifier" @ self.lexer.position(), lock, {
            match lx {
                Lexeme::Number(num) => append!(lock, "Parsed a clause identifier {} outside of the range [{}..{}].", num, 1i64, ClauseIndex::MaxValue),
                _ => append!(lock, "Expected a clause identifier, but found {} instead.", lx),
            }
        })
    }
}

pub struct AsrClauseParser<'a, L: AsrLexer> {
    parser: &'a mut AsrParser<L>,
    locked: bool,
}
impl<'a, L: AsrLexer> AsrClauseParser<'a, L> {
    pub fn next(&mut self) -> Option<Literal> {
        if intrinsics::unlikely(self.locked) {
            None
        } else {
            let litopt = match self.parser.lexer.peek().as_literal() {
                Ok(lit) => Some(lit),
                Err(Lexeme::Number(0i64)) => {
                    self.locked = true;
                    None
                },
                Err(lx) => {
                    self.locked = true;
                    self.parser.expected_literal(lx)
                },
            };
            self.parser.lexer.consume();
            litopt
        }
    }
}
impl<'a, L: AsrLexer> Drop for AsrClauseParser<'a, L> {
    fn drop(&mut self) {
        if intrinsics::unlikely(!self.locked) {
            while let Some(_) = self.next() {
            }
        }
    }
}

pub struct AsrChainParser<'a, L: AsrLexer> {
    parser: &'a mut AsrParser<L>,
    locked: bool,
}
impl<'a, L: AsrLexer> AsrChainParser<'a, L> {
    pub fn next(&mut self) -> Option<ClauseIndex> {
        if intrinsics::unlikely(self.locked) {
            None
        } else {
            let idopt = match self.parser.lexer.peek().as_index() {
                Ok(lit) => Some(lit),
                Err(Lexeme::Number(0i64)) => {
                    self.locked = true;
                    None
                },
                Err(lx) => {
                    self.locked = true;
                    self.parser.expected_index(lx);
                },
            };
            self.parser.lexer.consume();
            idopt
        }
    }
}
impl<'a, L: AsrLexer> Drop for AsrChainParser<'a, L> {
    fn drop(&mut self) {
        if intrinsics::unlikely(!self.locked) {
            while let Some(_) = self.next() {
            }
        }
    }
}

pub struct AsrWitnessParser<'a, L: AsrLexer> {
    parser: &'a mut AsrParser<L>,
    locked: bool,
}
impl<'a, L: AsrLexer> AsrWitnessParser<'a, L> {
    pub fn next(&mut self) -> Option<(Variable, Literal)> {
        if intrinsics::unlikely(self.locked) {
            None
        } else {
            let varopt = match self.parser.lexer.peek().as_variable() {
                Ok(lit) => Some(lit),
                Err(Lexeme::Number(0i64)) => {
                    self.locked = true;
                    None
                },
                Err(lx) => {
                    self.locked = true;
                    self.parser.expected_variable(lx)
                },
            };
            self.parser.lexer.consume();
            let var = varopt?;
            let lit = match self.parser.lexer.peek().as_literal() {
                Ok(lit) => lit,
                Err(lx) => {
                    self.locked = true;
                    self.parser.expected_literal(lx)
                },
            };
            self.parser.lexer.consume();
            Some((var, lit))
        }
    }
}
impl<'a, L: AsrLexer> Drop for AsrWitnessParser<'a, L> {
    fn drop(&mut self) {
        if intrinsics::unlikely(!self.locked) {
            while let Some(_) = self.next() {
            }
        }
    }
}

pub struct AsrMultichainParser<'a, L: AsrLexer> {
    parser: &'a mut AsrParser<L>,
    id: Option<ClauseIndex>,
    locked: bool,
}
impl<'a, L: AsrLexer> AsrMultichainParser<'a, L> {
    pub fn next(&mut self) -> Option<(ClauseIndex, Option<AsrChainParser<'_, L>>)> {
        if intrinsics::unlikely(self.locked) {
            None
        } else {
            let (lid, del) = if intrinsics::likely(self.id.is_none()) {
                let idopt = match self.parser.lexer.peek().as_index() {
                    Ok(lit) => Some(lit),
                    Err(Lexeme::Number(0i64)) => {
                        self.locked = true;
                        None
                    },
                    Err(lx) => {
                        self.locked = true;
                        self.parser.expected_index(lx);
                    },
                };
                self.parser.lexer.consume();
                let lid = idopt?;
                let del = if let &Lexeme::Letter(b'd') = self.parser.lexer.peek() {
                    self.parser.lexer.consume();
                    true
                } else {
                    false
                };
                (lid, del)
            } else {
                (self.id.take().unwrap(), false)
            };
            Some((lid, if del {
                None
            } else {
                Some(AsrChainParser::<'_, L> {
                    parser: self.parser,
                    locked: false,
                })
            }))
        }
    }
}
impl<'a, L: AsrLexer> Drop for AsrMultichainParser<'a, L> {
    fn drop(&mut self) {
        if intrinsics::unlikely(!self.locked) {
            while let Some(_) = self.next() {
            }
        }
    }
}