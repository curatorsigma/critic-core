//! Tools for working with Morphology data in critic
#[derive(Debug, PartialEq)]
pub enum MorphDialectParseError {}

pub trait MorphDialect: core::fmt::Display + core::str::FromStr<Err = MorphDialectParseError> {}

