//! The core tools for working with in critic
//!
//! For a definition of ATG, see `ATG_Definition.md`
use std::marker::PhantomData;
use crate::anchor::AnchorDialect;

#[cfg(test)]
mod test;

/// [ControlPointDefinition] defines the control points (as unicode scalar values) which have
/// special semantics in an `ATG` dialect.
///
/// See the `ATG_Definition.md` for a technical definition of ATG.
///
/// For a simple example of a [ControlPointDefinition], consider:
/// ```
/// use critic_core::atg::{ControlPointDefinition, AtgDialect};
/// const EXAMPLE_CONTROL_POINTS: ControlPointDefinition = ControlPointDefinition {
///     escape: '\\',
///     start_param: '(',
///     stop_param: ')',
///     illegible: '^',
///     lacuna: '~',
///     anchor: '§',
///     format_break: '/',
///     correction: '&',
///     non_semantic: "\t\n",
///     comment: '#',
/// };
/// ```
#[derive(Debug, PartialEq)]
pub struct ControlPointDefinition {
    /// This character is used to escape Unicode points that are to difficult (or impossible) to
    /// type.
    ///
    /// BEFORE ANY OTHER PROCESSING, the following process happens:
    /// (1) Every occurance of escape char followed by
    ///     - 6 if possible, or
    ///     - 4 if possible, or
    ///     - 2 if possible (in this order)
    /// of occurances of hex digits `0-9abcdefABCDEF`, irrespecitve of case, are replaced by the
    /// corresponding unicode character.
    ///
    /// In addition, whenever an escape is encountered during parsing at any other point:
    /// - if the following char is not a control point, parsing fails.
    /// - if the following char is a control point, it looses ALL semantic value and is treated as
    ///   any other non-control point char in its position.
    ///
    /// Try to be very conservative with the use of the escape character.
    /// Whenever possible, prefer to actually type out the unicode points of your language, and use
    /// control points that you do not need in either parameters or the transcribed language.
    /// On the other hand, when you use Unicode PUA, you will likely need to make use of the
    /// escape.
    pub escape: char,
    /// The character introducing a parameter definition
    ///
    /// Whenever another control point requires `parameters`, they are appended to that control
    /// point enclosed in start_param and [ControlPointDefinition::stop_param].
    ///
    /// For languages that do not use '(' (the opening paranthesis), we suggest using it. If '('
    /// needs to be used as part of the stream, chose another unicode point that is
    /// - easy to type (these will be used very frequently in ATG transcript files)
    /// - looks like or has semantics like paranthesis
    pub start_param: char,
    /// The character ending a parameter definition
    ///
    /// This should be naturally oposite to [ControlPointDefinition::start_param], e.g. ')'
    pub stop_param: char,
    /// A character introducing a section of text that is illegible (but not missing entirely, see
    /// lacuna for that use case)
    ///
    /// Takes one or two parameter:
    /// - the probable number of damaged characters
    /// - a probable reconstruction (a best-effort guess at the characters, whether from
    ///   orthography or vid). MAY be ommitted
    pub illegible: char,
    /// A character introducing a lacuna
    ///
    /// The lucuna may be supplied, which uses the same introducing character.
    /// Single missing characters are lacunae and marked with this character. Characters that are
    /// visible but illegible are marked [ControlPointDefinition::illegible] instead.
    ///
    /// Takes one or two parameter:
    /// - the probable number of damaged characters
    /// - a probable reconstruction (a best-effort guess at the characters, whether from
    ///   orthography or vid). MAY be ommitted
    pub lacuna: char,
    /// A character introducing a positional anchor
    ///
    /// A positional anchor here is some position in the text that is well-known or otherwise easy
    /// to find. Examples include:
    /// - verse numbers for biblical texts
    /// - chapter breaks, section breaks, stanza, ...
    ///
    /// The positional anchoring system MUST be uniform across all texts of one work, independent
    /// of Dialect.
    ///
    /// Try to avoid using line numbers whenever possible. Everything will still work, but the resulting Data
    /// will often be easier to understand when anchor appear at sentence breaks (semantic breaks, not
    /// formatting breaks in MSS)
    ///
    /// Takes one argument:
    /// - the anchor.
    pub anchor: char,
    /// A character introducing a format break of the MS
    ///
    /// Takes one parameter, either (as literal strings):
    /// - 'line': linebreak. Text continues on the next line.
    /// - 'column': columnbreak. Text continues on the next column.
    ///     - inherently also breaks the line
    /// - 'page': pagebreak. Text continues on the next page.
    ///     - inherently also breaks the column
    /// - 'paragraph': paragraph break. Text continues ofter some vertical whitespace in the same
    ///   column.
    ///     - inherently also breaks the line
    pub format_break: char,
    /// A character introducing a scribal correction
    ///
    /// The different corrections follow this character as parameters.
    /// The semantics are defined by manuscript (included in the meta information).
    /// - commonly, we write the corrections in this order:
    ///     - original, uncorrected
    ///     - original, corrected
    ///     - first external corrected
    ///     - second external corrected
    ///     - ...
    pub correction: char,
    /// Any number of chars that have no semantic in this ATG.
    ///
    /// They may be used for purposes of formatting. Consider making \n and \r non-semantic here.
    pub non_semantic: &'static str,
    /// A character introducing a comment
    ///
    /// Not part of the transcription, only for later editors of the same ATG data.
    pub comment: char,
}
impl ControlPointDefinition {
    /// True IFF c is a true control point
    fn is_control_point(&self, c: &char) -> bool {
        [
            self.escape,
            self.start_param,
            self.stop_param,
            self.correction,
            self.illegible,
            self.lacuna,
            self.correction,
            self.anchor,
            self.format_break,
            self.comment,
        ]
        .contains(c) || self.is_non_semantic(c)
    }

    /// True iff c is a non-semantic character
    fn is_non_semantic(&self, c: &char) -> bool {
        self.non_semantic.contains(*c)
    }
}

/// An [AtgDialect] contains all the information defining ATG for a specific language.
///
/// Not part of this structure, but part of an ATG dialect:
/// - a definition for Unicode PUA (if used)
/// - a way to render the unicode stream (TODO: what exactly do we require here?)
///
/// For a simple example of an [AtgDialect], consider:
/// ```
/// use critic_core::atg::{ControlPointDefinition, AtgDialect};
/// const EXAMPLE_CONTROL_POINTS: ControlPointDefinition = ControlPointDefinition {
///     escape: '\\',
///     start_param: '(',
///     stop_param: ')',
///     illegible: '^',
///     lacuna: '~',
///     anchor: '§',
///     format_break: '/',
///     correction: '&',
///     non_semantic: "\t\n",
///     comment: '#',
/// };
/// 
/// struct ExampleAtgDialect {}
/// impl AtgDialect for ExampleAtgDialect {
///     const NATIVE_POINTS: &'static str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ,.'";
///     const ATG_CONTROL_POINTS: ControlPointDefinition = EXAMPLE_CONTROL_POINTS;
///     const WORD_DIVISOR: char = ' ';
/// }
/// ```
pub trait AtgDialect {
    /// The unicode points allowed in this ATG dialect
    ///
    /// These code points are exactly those points which may appear in the language stream itself,
    /// excluding any control characters
    const NATIVE_POINTS: &'static str;

    const ATG_CONTROL_POINTS: ControlPointDefinition;

    /// A character used as semantic whitespace for word division
    ///
    /// - Use ' ' (the literal space) whenever that is possible.
    /// - Consider using another character if the language has few semantic word breaks and you
    ///   want to use ' ' to make reading the ATG file easier
    ///
    /// Consecutive occurances of word_divisor are compacted into one semantically relevant occurance.
    const WORD_DIVISOR: char;

    fn is_control_point(c: &char) -> bool {
        Self::ATG_CONTROL_POINTS.is_control_point(c)
    }

    fn is_non_semantic(c: &char)-> bool {
        Self::ATG_CONTROL_POINTS.is_non_semantic(c)
    }
}

/// The Errors that can occur while parsing a string as ATG
///
/// This type contains the location of the encountered problem.
#[derive(Debug, PartialEq)]
pub struct AtgParseError<A>
    where A: AnchorDialect,
{
    /// Location at which the problem was encountered (byte-offset, NOT Unicode)
    location: usize,
    /// The problem that occured
    reason: AtgParseErrorReason<A>,
}
impl<A> core::fmt::Display for AtgParseError<A> where A: AnchorDialect {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        write!(f, "{} at {}", self.reason, self.location)
    }
}
impl<A> std::error::Error for AtgParseError<A> where A: AnchorDialect {}
impl<A> AtgParseError<A> where A: AnchorDialect {
    /// Create an [AtgParseError]
    pub fn new(location: usize, reason: AtgParseErrorReason<A>) -> Self {
        Self {
            location, reason
        }
    }

    /// Add an offset to the existing location
    pub fn offset_location(self, offset: usize) -> Self {
        Self::new(self.location + offset, self.reason)
    }
}

/// The Errors that can occur while parsing a string as ATG
///
/// This type does not contain the location of the encountered problem.
#[derive(Debug, PartialEq, Eq)]
pub enum AtgParseErrorReason<A>
    where A: AnchorDialect,
{
    /// An escape character was used but not followed by 2,4, or 6 hexdigits defining a unicode
    /// scalar value or a control char
    EscapeMalformed(String),
    /// The start of a parameter sequence was expected, but not encountered
    MissingParameterStart,
    /// A parameter was required to contain a length, but was not parsable as a number
    LengthNotANumber(String),
    /// A string was required to be native, but contained non-native characters
    NotNative(String),
    /// An error occured while parsing an Anchor
    Anchor(A::ParseError),
    /// A format break was encountered, but it was not one of the known Format breaks.
    UnknownFormatBreak(String),
    /// EOF was encountered while a parameter still needed to be closed
    EOF(char),
}
impl<A> core::fmt::Display for AtgParseErrorReason::<A>
    where A: AnchorDialect,
{
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::EscapeMalformed(x) => {
                write!(f, "{x} cannot be used as escape sequence")
            }
            Self::MissingParameterStart => {
                write!(f, "A parameter was expected but not found.")
            }
            Self::LengthNotANumber(x) => {
                write!(f, "{x} is not parsable as a length value.")
            }
            Self::NotNative(x) => {
                write!(f, "{x} is not a native string of the used dialect.")
            }
            Self::Anchor(x) => {
                write!(f, "There was a problem parsing an anchor: {x}.")
            }
            Self::UnknownFormatBreak(x) => {
                write!(f, "{x} is not a format break ('line', 'column', 'paragraph', 'page).")
            }
            Self::EOF(x) => {
                write!(f, "EOF was encountered while waiting for {x}")
            }
        }
    }
}
impl<A> std::error::Error for AtgParseErrorReason::<A> where A: AnchorDialect {}

/// if the first character of s is the escape, try to read one character as an escape
///
/// Err value: the string that should have been an escape sequence but is not
///
/// Return:
/// - the final character
/// - the remaining string
/// - the index at which the remaining string starts in the input
fn escape_one_if_required<D>(s: &str) -> Result<(char, &str, usize), String>
    where D: AtgDialect,
{
    let Some(c) = s.chars().nth(0) else {
        return Err(s.to_owned());
    };

    if c != D::ATG_CONTROL_POINTS.escape {
        // we need to get the index of the next character start
        let idx = match s.char_indices().nth(1) {
            Some(x) => x.0,
            None => 1_usize,
        };
        return Ok((c, &s[idx..], idx));
    };

    if let Some(next) = s.chars().nth(1) {
        // if the next char is a control point: escape it
        if D::is_control_point(&next) {
            // we need to get the index of the next character start
            // we are skipping the escape and the next control point that is escaped
            let idx = match s.char_indices().nth(2) {
                Some(x) => x.0,
                None => 2_usize,
            };
            Ok((next, &s[idx..], idx))
        }
        // the next char is not a control point
        // try to get the next six characters as hexdigits and parse them as unicode point
        else if s.len() >= 7 && s.chars().skip(1).take(6).all(|x| x.is_ascii_hexdigit()) {
            let parsed = u32::from_str_radix(&s[1..7], 16).map_err(|_| s[0..7].to_owned())?;
            Ok((core::char::from_u32(parsed).ok_or(&s[0..7].to_owned())?, &s[7..], 7))
        }
        // try 4 hex digits
        else if s.len() >= 5 && s.chars().skip(1).take(4).all(|x| x.is_ascii_hexdigit()) {
            let parsed = u32::from_str_radix(&s[1..5], 16).map_err(|_| s[0..5].to_owned())?;
            Ok((core::char::from_u32(parsed).ok_or(&s[0..5].to_owned())?, &s[5..], 5))
        }
        // try 2 hex digits
        else if s.len() >= 3 && s.chars().skip(1).take(2).all(|x| x.is_ascii_hexdigit()) {
            let parsed = u32::from_str_radix(&s[1..3], 16).map_err(|_| s[0..3].to_owned())?;
            Ok((core::char::from_u32(parsed).ok_or(&s[0..3].to_owned())?, &s[3..], 3))
        } else {
            Err(s.to_owned())
        }
    } else {
        return Err(s.to_owned());
    }
}

/// Eat characters (escaping them as we go) until
/// - an error occurs while escaping
/// - the next occurance of `c` is encountered
/// - EOF is encountered (this is considered an Error)
fn escape_until_next<A, D>(c: char, s: &str) -> Result<(String, &str, usize), AtgParseError<A>>
    where D: AtgDialect,
          A: AnchorDialect,
{
    let mut res = String::new();
    let mut remainder = s;
    let mut current;
    let mut offset = 0_usize;
    let mut single_escape_offset;
    loop {
        if remainder.is_empty() {
            return Err(AtgParseError::<A>::new(offset, AtgParseErrorReason::EOF(c)));
        };
        (current, remainder, single_escape_offset) = escape_one_if_required::<D>(remainder).map_err(|x| AtgParseError::new(offset, AtgParseErrorReason::EscapeMalformed(x)))?;
        offset = offset + single_escape_offset;
        if current == c {
            return Ok((res, remainder, offset));
        }
        res.push(current);
    }
}

/// Collect all characters until the next control point.
///
/// The remainder WILL be part of the remainder.
///
/// The return values are:
/// - The string excluding the next control point
/// - Some(the control point) or None, if the input ended
/// - the remaining, unparsed input, INCLUDING the control point
fn escape_until_control_point<A, D>(s: &str) -> Result<(String, Option<char>, &str, usize), AtgParseError<A>>
    where D: AtgDialect,
          A: AnchorDialect,
{
    let mut res = String::new();
    let mut remainder = s;
    let mut current;
    // the offset of the next char in the input string (in bytes)
    let mut offset = 0_usize;
    let mut single_escape_offset;
    let mut new_remainder;
    loop {
        if remainder.is_empty() {
            return Ok((res, None, remainder, 0));
        };
        (current, new_remainder, single_escape_offset) = escape_one_if_required::<D>(remainder).map_err(|x| AtgParseError::new(offset, AtgParseErrorReason::EscapeMalformed(x)))?;
        offset = offset + single_escape_offset;
        if D::is_control_point(&current) {
            return Ok((res, Some(current), remainder, offset));
        }
        remainder = new_remainder;
        res.push(current);
    };
}

/// Parse a single parameter.
///
/// The first character in the input needs to be the parameter start control point.
fn collect_parameter<A, D>(s: &str) -> Result<(String, &str, usize), AtgParseError::<A>>
    where D: AtgDialect,
          A: AnchorDialect,
{
    let (first, remainder, _) = escape_one_if_required::<D>(s).map_err(|x| AtgParseError::new(0, AtgParseErrorReason::EscapeMalformed(x)))?;
    if first != D::ATG_CONTROL_POINTS.start_param {
        return Err(AtgParseError::new(0, AtgParseErrorReason::MissingParameterStart));
    };

    escape_until_next::<A, D>(D::ATG_CONTROL_POINTS.stop_param, remainder).map_err(|x| x.offset_location(1))
}

/// Parse a single parameter which needs to be native.
///
/// The first character in the input needs to be the parameter start control point.
fn collect_native_parameter<A, D>(s: &str) -> Result<(String, &str, usize), AtgParseError::<A>>
    where D: AtgDialect,
          A: AnchorDialect,
{
    if s.is_empty() {
        return Err(AtgParseError::new(0, AtgParseErrorReason::MissingParameterStart));
    };

    let (parameter, remainder, offset) = collect_parameter::<A, D>(s)?;
    for (idx, c) in parameter.char_indices() {
        if !D::NATIVE_POINTS.contains(c) {
            return Err(AtgParseError::new(idx, AtgParseErrorReason::NotNative(parameter)));
        };
    };
    Ok((parameter, remainder, offset))
}

/// Struct containing a single text in ATG format
#[derive(Debug)]
pub struct Text<A>
where
    A: AnchorDialect,
{
    parts: Vec<Part<A>>,
}
impl<A> Text<A>
where
    A: AnchorDialect,
{
    pub fn render<D>(&self) -> String
    where
        D: AtgDialect,
    {
        let mut res = String::new();
        for part in &self.parts {
            res.push_str(&part.render::<D>());
        }
        return res;
    }

    /// parse a string into an ATG text.
    pub fn parse<D>(s: &str) -> Result<Self, AtgParseError::<A>>
    where
        D: AtgDialect,
    {
        let (first_part, mut remainder) = Part::<A>::parse::<D>(s)?;
        let mut parts = vec![first_part];
        while !remainder.is_empty() {
            let (next_part, next_remainder) = Part::<A>::parse::<D>(remainder)?;
            parts.push(next_part);
            remainder = next_remainder;
        }
        Ok(Text::<A> { parts })
    }
}

#[derive(Debug, PartialEq)]
enum Part<A>
where
    A: AnchorDialect,
{
    Native(String),
    Illegible(Uncertain<Illegible>),
    Lacuna(Uncertain<Lacuna>),
    Correction(Correction),
    FormatBreak(FormatBreak),
    Anchor(A),
}
impl<A> Part<A>
where
    A: AnchorDialect,
{
    fn render<D>(&self) -> String
    where
        D: AtgDialect,
    {
        match self {
            Self::Native(x) => x.to_owned(),
            Self::Illegible(x) => x.render::<D>(),
            Self::Lacuna(x) => x.render::<D>(),
            Self::Correction(x) => x.render::<D>(),
            Self::FormatBreak(x) => x.render::<D>(),
            Self::Anchor(x) => {
                format!(
                    "{}{}{x}{}",
                    D::ATG_CONTROL_POINTS.anchor,
                    D::ATG_CONTROL_POINTS.start_param,
                    D::ATG_CONTROL_POINTS.stop_param
                )
            }
        }
    }

    fn parse_anchor<D>(s: &str) -> Result<(A, &str), AtgParseError::<A>>
        where D: AtgDialect,
    {
        // get one parameter
        let (anchor_string, remainder, _) = collect_parameter::<A, D>(s)?;
        let anchor = anchor_string.parse::<A>().map_err(|x| AtgParseError::new(1, AtgParseErrorReason::Anchor(x)))?;
        Ok((anchor, remainder))
    }

    fn parse_comment<D>(s: &str) -> Result<(usize, &str), AtgParseError::<A>>
        where
            D: AtgDialect,
    {
        let (comment, remainder, _) = collect_parameter::<A, D>(s)?;
        Ok((comment.len(), remainder))
    }

    /// read all the characters until the next non-comment non-escape control sequence
    ///
    /// ignores comments
    fn parse_native<D>(s: &str) -> Result<(Self, &str), AtgParseError::<A>>
        where D: AtgDialect,
    {
        if s.is_empty() {
            return Ok((Part::Native("".to_owned()), ""));
        }
        let (mut res, mut maybe_ctrl, mut remainder, _) = escape_until_control_point::<A, D>(s)?;
        let mut next_res: String;
        let mut offset = 0_usize;
        let mut add_offset: usize;
        loop {
            // early return if the last iteration found EOF instead of a control point
            let Some(ctrl_point) = maybe_ctrl else {
                return Ok((Part::Native(res), ""));
            };
            // if the control point is a comment, parse the comment and ignore it
            if ctrl_point == D::ATG_CONTROL_POINTS.comment {
                (_, remainder) = Self::parse_comment::<D>(remainder)?;
                (next_res, maybe_ctrl, remainder, add_offset) = escape_until_control_point::<A, D>(remainder).map_err(|x| x.offset_location(offset))?;
                offset += add_offset;
                res.push_str(&next_res);
            // if the control point is nonsemantic, completely ignore it (effectively a single
            // character comment)
            } else if D::is_non_semantic(&ctrl_point) {
                (next_res, maybe_ctrl, remainder, add_offset) = escape_until_control_point::<A, D>(remainder).map_err(|x| x.offset_location(offset))?;
                offset += add_offset;
                res.push_str(&next_res);
            } else {
                return Ok((Part::<A>::Native(res), remainder));
            };
        };
    }

    /// Parse a string as a single ATG Part
    fn parse<D>(s: &str) -> Result<(Self, &str), AtgParseError::<A>>
    where
        D: AtgDialect,
    {
        if s.is_empty() {
            return Ok((Part::<A>::Native("".to_owned()), s));
        };
        // escape the first character if required
        let (c, remainder, _) = escape_one_if_required::<D>(s).map_err(|x| AtgParseError::new(0, AtgParseErrorReason::EscapeMalformed(x)))?;

        // check what we have to parse
        if c == D::ATG_CONTROL_POINTS.illegible {
            let (illeg, remainder) = Uncertain::<Illegible>::parse::<A, D>(remainder).map_err(|x| x.offset_location(1))?;
            Ok((Part::<A>::Illegible(illeg), remainder))
        } else if c == D::ATG_CONTROL_POINTS.lacuna {
            let (lacuna, remainder) = Uncertain::<Lacuna>::parse::<A, D>(remainder).map_err(|x| x.offset_location(1))?;
            Ok((Part::<A>::Lacuna(lacuna), remainder))
        } else if c == D::ATG_CONTROL_POINTS.anchor {
            let (anchor, remainder) = Self::parse_anchor::<D>(remainder).map_err(|x| x.offset_location(1))?;
            Ok((Part::<A>::Anchor(anchor), remainder))
        } else if c == D::ATG_CONTROL_POINTS.format_break {
            let (format_break, remainder) = FormatBreak::parse::<A, D>(remainder).map_err(|x| x.offset_location(1))?;
            Ok((Part::<A>::FormatBreak(format_break), remainder))
        } else if c == D::ATG_CONTROL_POINTS.correction {
            let (correction, remainder) = Correction::parse::<A, D>(remainder).map_err(|x| x.offset_location(1))?;
            Ok((Part::<A>::Correction(correction), remainder))
        } else if c == D::ATG_CONTROL_POINTS.comment {
            let (comment_length, remainder) = Self::parse_comment::<D>(remainder).map_err(|x| x.offset_location(1))?;
            Self::parse_native::<D>(remainder).map_err(|x| x.offset_location(comment_length))
        } else {
            Self::parse_native::<D>(s)
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Illegible;
#[derive(Debug, PartialEq)]
pub struct Lacuna;
/// Reason for some part of the text to be uncertain
pub trait UncertainReason {}
impl UncertainReason for Illegible {}
impl UncertainReason for Lacuna {}

/// Any part of the text that is uncertain
#[derive(Debug, PartialEq)]
pub struct Uncertain<T>
where
    T: UncertainReason,
{
    len: u8,
    proposal: Option<String>,
    _x: PhantomData<T>,
}
impl<T> Uncertain<T>
where
    T: UncertainReason,
{
    pub fn new(len: u8, proposal: Option<String>) -> Self {
        Self {
            len,
            proposal,
            _x: PhantomData::<T>,
        }
    }

    /// Parse a sequence of parameters as an uncertain passage
    ///
    /// The caller made sure that this input is preceeded by the uncertain code point (of either
    /// illegible or lacuna)
    pub fn parse<A, D>(s: &str) -> Result<(Self, &str), AtgParseError::<A>>
        where A: AnchorDialect,
              D: AtgDialect,
    {
        let (first, remainder, _) = escape_one_if_required::<D>(s).map_err(|x| AtgParseError::new(0, AtgParseErrorReason::EscapeMalformed(x)))?;
        if first != D::ATG_CONTROL_POINTS.start_param {
            return Err(AtgParseError::new(0, AtgParseErrorReason::MissingParameterStart));
        };
        // collect until the next stop_param
        let (first_param, remainder, first_param_offset) = escape_until_next::<A, D>(D::ATG_CONTROL_POINTS.stop_param, remainder).map_err(|x| x.offset_location(1))?;
        // make sure this is a number
        let uncertain_len = first_param.parse::<u8>().map_err(|_| AtgParseError::new(1, AtgParseErrorReason::LengthNotANumber(first_param)))?;
        if remainder.is_empty() {
            return Ok((Uncertain::<T>::new(uncertain_len, None), remainder));
        };

        let (proposal, remainder, _) = match collect_native_parameter::<A, D>(remainder).map_err(|x| x.offset_location(first_param_offset + 1)) {
            Ok(x) => x,
            Err(AtgParseError { location: _, reason: AtgParseErrorReason::MissingParameterStart }) => {
                return Ok((Uncertain::<T>::new(uncertain_len, None), remainder));
            },
            Err(x) => { return Err(x) },
        };
        // make sure every char until the next stop_param is native (or escaped native)
        Ok((Uncertain::<T>::new(uncertain_len, if proposal.is_empty() { None } else { Some(proposal) }), remainder))
    }
}
impl Uncertain<Illegible> {
    fn render<D>(&self) -> String
    where
        D: AtgDialect,
    {
        let start_param = D::ATG_CONTROL_POINTS.start_param;
        let stop_param = D::ATG_CONTROL_POINTS.stop_param;
        let illegible = D::ATG_CONTROL_POINTS.illegible;
        match &self.proposal {
            None => {
                format!("{illegible}{start_param}{}{stop_param}", self.len)
            }
            Some(proposal) => {
                format!(
                    "{illegible}{start_param}{}{stop_param}{start_param}{}{stop_param}",
                    self.len, proposal
                )
            }
        }
    }

}
impl Uncertain<Lacuna> {
    fn render<D>(&self) -> String
    where
        D: AtgDialect,
    {
        let start_param = D::ATG_CONTROL_POINTS.start_param;
        let stop_param = D::ATG_CONTROL_POINTS.stop_param;
        let lacuna = D::ATG_CONTROL_POINTS.lacuna;
        match &self.proposal {
            None => {
                format!("{lacuna}{start_param}{}{stop_param}", self.len)
            }
            Some(proposal) => {
                format!(
                    "{lacuna}{start_param}{}{stop_param}{start_param}{}{stop_param}",
                    self.len, proposal
                )
            }
        }
    }
}

/// Anything that is present, but potentially not legible.
#[derive(Debug, PartialEq)]
pub enum Present {
    Native(String),
    Illegible(Uncertain<Illegible>),
}
impl Present {
    fn render<D>(&self) -> String
    where
        D: AtgDialect,
    {
        match self {
            Self::Native(x) => x.to_owned(),
            Self::Illegible(x) => x.render::<D>(),
        }
    }
}

/// Defines a part of a text that was corrected
#[derive(Debug, PartialEq)]
struct Correction {
    /// The different versions of this corrected passage.
    ///
    /// Order specifies, which version belongs to which hand. This is defined per Manuscript.
    versions: Vec<Present>,
}
impl Correction {
    fn render<D>(&self) -> String
    where
        D: AtgDialect,
    {
        let mut res = String::new();
        res.push(D::ATG_CONTROL_POINTS.correction);
        for version in &self.versions {
            res.push_str(&format!(
                "{}{}{}",
                D::ATG_CONTROL_POINTS.start_param,
                version.render::<D>(),
                D::ATG_CONTROL_POINTS.stop_param
            ));
        }
        res
    }

    /// Parse a sequence of parameters as a correction
    ///
    /// The caller has stripped the correction control point already
    fn parse<A, D>(s: &str) -> Result<(Self, &str), AtgParseError<A>>
        where A: AnchorDialect,
              D: AtgDialect,
    {
        let mut versions = Vec::<Present>::new();
        let mut offset = 0_usize;
        let mut add_offset;

        let (mut param, mut remainder, _) = collect_native_parameter::<A, D>(s)?;
        versions.push(Present::Native(param));
        loop {
            (param, remainder, add_offset) = match collect_native_parameter::<A, D>(remainder) {
                Ok((x, y, z)) => {
                    (x, y, z)
                },
                Err(AtgParseError { reason: AtgParseErrorReason::MissingParameterStart, location: _}) => {
                    return Ok((Correction { versions }, remainder))
                },
                Err(x) => {
                    return Err(x.offset_location(offset));
                },
            };
            offset += add_offset;
            versions.push(Present::Native(param));
        };
    }
}

/// Information about a format break
#[derive(Debug, PartialEq)]
pub enum FormatBreak {
    Line,
    Column,
    Paragraph,
    Page,
}
impl FormatBreak {
    fn render<D>(&self) -> String
    where
        D: AtgDialect,
    {
        let format_break = D::ATG_CONTROL_POINTS.format_break;
        let start_param = D::ATG_CONTROL_POINTS.start_param;
        let stop_param = D::ATG_CONTROL_POINTS.stop_param;
        match self {
            Self::Line => format!("{format_break}{start_param}line{stop_param}"),
            Self::Column => format!("{format_break}{start_param}column{stop_param}"),
            Self::Paragraph => format!("{format_break}{start_param}paragraph{stop_param}"),
            Self::Page => format!("{format_break}{start_param}page{stop_param}"),
        }
    }

    fn parse<A, D>(s: &str) -> Result<(Self, &str), AtgParseError<A>>
        where A: AnchorDialect,
              D: AtgDialect,
    {
        let (parameter, remainder, _) = collect_parameter::<A, D>(s)?;
        match parameter.as_str() {
            "line" => {
                Ok((FormatBreak::Line, remainder))
            }
            "paragraph" => {
                Ok((FormatBreak::Paragraph, remainder))
            }
            "column" => {
                Ok((FormatBreak::Column, remainder))
            }
            "page" => {
                Ok((FormatBreak::Page, remainder))
            }
            _ => {
                Err(AtgParseError::new(1, AtgParseErrorReason::UnknownFormatBreak(parameter)))
            }
        }
    }
}
