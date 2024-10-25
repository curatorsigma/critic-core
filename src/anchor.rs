//! Tools for working with work-agnostic positional anchors (e.g. versification) in critic
use core::str::FromStr;

/// A dialect for Positional Anchors.
///
/// By example, a positional Anchor could look like the following.
/// Here we model a short work, which is a poem consisting of only two Stanzas.
/// The positional Anchors are the starts of the two Stanzas.
/// ```
/// use core::str::FromStr;
/// use critic_core::anchor::AnchorDialect;
/// #[derive(Debug, PartialEq)]
/// enum ParseStanzaError {
///     EmptyString,
///     TooManyChars,
///     NotInRange,
///     NotANumber,
/// }
/// impl core::fmt::Display for ParseStanzaError {
///     fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
///         match self {
///             Self::EmptyString => write!(f, "Empty String is no valid stanza number."),
///             Self::TooManyChars => write!(f, "More then one character is no valid stanza number."),
///             Self::NotANumber => write!(f, "The Argument is not a number."),
///             Self::NotInRange => write!(f, "The Argument is not either 1 or 2."),
///         }
///     }
/// }
/// impl std::error::Error for ParseStanzaError {}
/// 
/// #[derive(Debug, PartialEq)]
/// enum Stanza {
///     One,
///     Two,
/// }
/// impl core::fmt::Display for Stanza {
///     fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
///         match self {
///             Self::One => {
///                 write!(f, "1")
///             }
///             Self::Two => {
///                 write!(f, "2")
///             }
///         }
///     }
/// }
/// impl FromStr for Stanza {
///     type Err = ParseStanzaError;
///     fn from_str(s: &str) -> Result<Self, Self::Err> {
///         if s.is_empty() {
///             Err(ParseStanzaError::EmptyString)
///         } else if s.len() >= 2 {
///             Err(ParseStanzaError::TooManyChars)
///         } else {
///             let nr = s.parse::<u8>().map_err(|_| ParseStanzaError::NotANumber)?;
///             match nr {
///                 1 => Ok(Stanza::One),
///                 2 => Ok(Stanza::Two),
///                 _ => Err(ParseStanzaError::NotInRange),
///             }
///         }
///     }
/// }
/// impl AnchorDialect for Stanza {
///     type ParseError = ParseStanzaError;
/// }
/// ```
///
/// A more interesting example for an [AnchorDialect] could be a versification scheme for a
/// classical work.
pub trait AnchorDialect: FromStr<Err = Self::ParseError> + core::fmt::Display + core::fmt::Debug + PartialEq {
    type ParseError: std::error::Error + PartialEq;
}

