use std::{
    fmt::{Formatter, Result as FmtResult, Display},
};

use crate::{
    basic::{Variable, Literal, ClauseIndex},
};

use colored::{
	Colorize,
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

// pub struct CsvDisplay<T, I>(pub I) where
//     I: Iterator<Item = T>,
//     T: Display;
// impl<I: Iterator<Item = T>, T: Display> Display for CsvDisplay<T, I> {
//     fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        
//     }
// }




// pub struct CsvDisplayMap<I, T, S, F>(pub F, pub I) where
//     I: IntoIterator<Item = T> + Copy,
//     S: Display,
//     F: Fn(T) -> S + Copy;
// impl<I, T, S, F> Display for CsvDisplayMap<I, T, S, F> where
//     I: IntoIterator<Item = T> + Copy,
//     S: Display,
//     F: Fn(T) -> S + Copy
// {
//     fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
//         let mut first = true;
//         for lit in self.1.into_iter().map(self.0) {
//             if first {
//                 first = false;
//             } else {
//                 write!(f, ", ")?;
//             }
//             write!(f, "{}", lit)?;
//         }
//         Ok(())
//     } 
// }

// pub struct CsvDisplay<I, T>(pub I) where
//     I: IntoIterator<Item = T> + Copy,
//     T: Display;
// impl<I, T> Display for CsvDisplay<I, T> where
//     I: IntoIterator<Item = T> + Copy,
//     T: Display
// {
//     fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
//         let mut first = true;
//         for lit in self.0 {
//             if first {
//                 first = false;
//             } else {
//                 write!(f, ", ")?;
//             }
//             write!(f, "{}", lit)?;
//         }
//         Ok(())
//     } 
// }

// pub struct ClauseDisplay<'a, I>(pub I) where
//     I: IntoIterator<Item = &'a Literal> + Copy;
// impl<'a, I> Display for ClauseDisplay<'a, I> where
//     I: IntoIterator<Item = &'a Literal> + Copy
// {
//     fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
//         let mut it = self.0.into_iter().peekable();
//         write!(f, "[")?;
//         loop {
//             if let Some(lit) = it.next() {
//                 write!(f, "{}", lit)?;
//             }
//             if it.peek().is_some() {
//                 write!(f, ", ")?;
//             } else {
//                 break write!(f, "]");
//             }
//         }
//     }
// }

// pub struct ChainDisplay<'a, I>(pub I) where
//     I: IntoIterator<Item = &'a ClauseIndex> + Copy;
// impl<'a, I> Display for ChainDisplay<'a, I> where
//     I: IntoIterator<Item = &'a ClauseIndex> + Copy
// {
//     fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
//         let mut it = self.0.into_iter().peekable();
//         write!(f, "(")?;
//         loop {
//             if let Some(id) = it.next() {
//                 write!(f, "{}", id)?;
//             }
//             if it.peek().is_some() {
//                 write!(f, ", ")?;
//             } else {
//                 break write!(f, ")");
//             }
//         }
//     }
// }

// pub struct SubstitutionDisplay<'a, I>(pub I) where
//     I: IntoIterator<Item = &'a (Variable, Literal)> + Copy;
// impl<'a, I> Display for SubstitutionDisplay<'a, I> where
//     I: IntoIterator<Item = &'a (Variable, Literal)> + Copy
// {
//     fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
//         let mut first = true;
//         write!(f, "{{")?;
//         for (var, lit) in self.0 {
//             if first {
//                 first = false;
//             } else {
//                 write!(f, ", ")?;
//             }
//             write!(f, "{} -> {}", var, lit)?;
//         }
//         write!(f, "}}")
//     }
// }

// pub struct MappingDisplay<'a>(pub &'a (Variable, Literal));
// impl<'a> Display for MappingDisplay<'a> {
//     fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
//         write!(f, "{} -> {}", (self.0).0, (self.0).1)
//     }
// }

// pub struct StatsDisplay<'a>(pub &'a str);
// impl<'a> Display for StatsDisplay<'a> {
//     fn fmt(&self , f: &mut Formatter<'_>) -> FmtResult {
//         write!(f, "{}", self.0.blue().bold())
//     }
// }