use std::{
    fmt::{Formatter, Result as FmtResult, Display},
};

use crate::{
    basic::{Variable, Literal, ClauseIndex},
};

pub struct DisplayLiteralPairCsv<'a>(pub &'a [(Literal, Literal)]);
impl<'a> Display for DisplayLiteralPairCsv<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let mut first = true;
        for (lit1, lit2) in self.0 {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{{{}, {}}}", lit1, lit2)?;
        }
        Ok(())
    }
}

pub struct DisplayLiteralCsv<'a>(pub &'a [Literal]);
impl<'a> Display for DisplayLiteralCsv<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let mut first = true;
        for lit in self.0 {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}", lit)?;
        }
        Ok(())
    }
}

pub struct DisplayClause<'a>(pub &'a [Literal]);
impl<'a> Display for DisplayClause<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let mut first = true;
        write!(f, "[")?;
        for lit in self.0 {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}", lit)?;
        }
        write!(f, "]")
    }
}

pub struct DisplaySubstitutionMappingCsv<'a>(pub &'a [(Variable, Literal)]);
impl<'a> Display for DisplaySubstitutionMappingCsv<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let mut first = true;
        for (var, lit) in self.0 {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{} -> {}", var, lit)?;
        }
        Ok(())
    }
}

pub struct DisplaySubstitutionMultimappingCsv<'a>(pub &'a [(Variable, Vec<Literal>)]);
impl<'a> Display for DisplaySubstitutionMultimappingCsv<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let mut first_multimapping = true;
        for (var, lits) in self.0 {
            if first_multimapping {
                first_multimapping = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{{")?;
            let mut first_mapping = true;
            for lit in lits {
                if first_mapping {
                    first_mapping = false;
                } else {
                    write!(f, ", ")?;
                }
                write!(f, "{} -> {}", var, lit)?;
            }
            write!(f, "}}")?;
        }
        Ok(())
    }
}

pub struct DisplaySubstitution<'a>(pub &'a [(Variable, Literal)]);
impl<'a> Display for DisplaySubstitution<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let mut first = true;
        write!(f, "[")?;
        for (var, lit) in self.0 {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{} -> {}", var, lit)?;
        }
        write!(f, "]")
    }
}

pub struct DisplayClauseIndexCsv<'a>(pub &'a [ClauseIndex]);
impl<'a> Display for DisplayClauseIndexCsv<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let mut first = true;
        for id in self.0 {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}", id)?;
        }
        Ok(())
    }
}

pub struct DisplayChain<'a>(pub &'a [ClauseIndex]);
impl<'a> Display for DisplayChain<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        let mut first = true;
        write!(f, "(")?;
        for id in self.0 {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}", id)?;
        }
        write!(f, ")")
    }
}