//! `critic-core` is the core crate defining Data Types and other language agnostic parts of critic
//! that are specialized to languages and used by other tools in the `critic` stack.
pub mod atg;
pub mod morph;
pub mod syntax;
pub mod anchor;

/// WIP
trait CriticDialect<Morph, Transcript>
where
    Morph: morph::MorphDialect,
    Transcript: atg::AtgDialect,
{
    /// Normalize a surface form of a word
    fn surface_normalize() {}
}

