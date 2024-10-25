//! An example ATG-Dialect
use std::str::FromStr;

use super::{AnchorDialect, AtgDialect, ControlPointDefinition};

const EXAMPLE_CONTROL_POINTS: ControlPointDefinition = ControlPointDefinition {
    escape: '\\',
    start_param: '(',
    stop_param: ')',
    illegible: '^',
    lacuna: '~',
    anchor: '§',
    format_break: '/',
    correction: '&',
    non_semantic: "\t\n",
    comment: '#',
};

struct ExampleAtgDialect {}
impl AtgDialect for ExampleAtgDialect {
    const NATIVE_POINTS: &'static str = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ,.'";
    const ATG_CONTROL_POINTS: ControlPointDefinition = EXAMPLE_CONTROL_POINTS;
    const WORD_DIVISOR: char = ' ';
}

#[derive(Debug, PartialEq)]
enum ParseStanzaError {
    EmptyString,
    TooManyChars,
    NotInRange,
    NotANumber,
}
impl core::fmt::Display for ParseStanzaError {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::EmptyString => write!(f, "Empty String is no valid stanza number."),
            Self::TooManyChars => write!(f, "More then one character is no valid stanza number."),
            Self::NotANumber => write!(f, "The Argument is not a number."),
            Self::NotInRange => write!(f, "The Argument is not either 1 or 2."),
        }
    }
}
impl std::error::Error for ParseStanzaError {}

#[derive(Debug, PartialEq)]
enum Stanza {
    One,
    Two,
}
impl core::fmt::Display for Stanza {
    fn fmt(&self, f: &mut core::fmt::Formatter) -> core::fmt::Result {
        match self {
            Self::One => {
                write!(f, "1")
            }
            Self::Two => {
                write!(f, "2")
            }
        }
    }
}
impl FromStr for Stanza {
    type Err = ParseStanzaError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.is_empty() {
            Err(ParseStanzaError::EmptyString)
        } else if s.len() >= 2 {
            Err(ParseStanzaError::TooManyChars)
        } else {
            let nr = s.parse::<u8>().map_err(|_| ParseStanzaError::NotANumber)?;
            match nr {
                1 => Ok(Stanza::One),
                2 => Ok(Stanza::Two),
                _ => Err(ParseStanzaError::NotInRange),
            }
        }
    }
}
impl AnchorDialect for Stanza {
    type ParseError = ParseStanzaError;
}

use crate::atg::{
    escape_one_if_required, escape_until_control_point, AtgParseError, AtgParseErrorReason, Correction, FormatBreak, Illegible, Lacuna, Part, Present, Text, Uncertain
};

#[test]
fn known_good_stanza() {
    let stanza = "1".parse::<Stanza>().unwrap();
    assert_eq!(stanza, Stanza::One);

    let stanza = "2".parse::<Stanza>().unwrap();
    assert_eq!(stanza, Stanza::Two);
}

#[test]
fn render_text() {
    let res = "§(1)\
    In twilight's glow, the sha^(2)(do)ws dance,/(line)\
    Whispers of dre^(1)(a)ms in a fleeting trance,/(line)\
    Stars awaken^(1)(,) the night unfurls,/(line)\
    A canvas painted with secret pearls./(paragraph)\
    §(2)\
    The moonlight &(illuminates)(bathes) the quiet stream,/(line)\
    Where echoes linger of a distant dream,/(line)\
    Soft breezes carry the ta~(8)ld,/(line)\
    Of hearts entwined and lo~(8)d./(page)\
    ";
    let mut text = Vec::<Part<Stanza>>::new();
    text.push(Part::<Stanza>::Anchor(Stanza::One));
    text.push(Part::<Stanza>::Native(
        "In twilight's glow, the sha".to_owned(),
    ));
    text.push(Part::<Stanza>::Illegible(Uncertain::<Illegible>::new(
        2,
        Some("do".to_owned()),
    )));
    text.push(Part::<Stanza>::Native("ws dance,".to_owned()));
    text.push(Part::<Stanza>::FormatBreak(FormatBreak::Line));

    text.push(Part::<Stanza>::Native("Whispers of dre".to_owned()));
    text.push(Part::<Stanza>::Illegible(Uncertain::<Illegible>::new(
        1,
        Some("a".to_owned()),
    )));
    text.push(Part::<Stanza>::Native(
        "ms in a fleeting trance,".to_owned(),
    ));
    text.push(Part::<Stanza>::FormatBreak(FormatBreak::Line));

    text.push(Part::<Stanza>::Native("Stars awaken".to_owned()));
    text.push(Part::<Stanza>::Illegible(Uncertain::<Illegible>::new(
        1,
        Some(",".to_owned()),
    )));
    text.push(Part::<Stanza>::Native(" the night unfurls,".to_owned()));
    text.push(Part::<Stanza>::FormatBreak(FormatBreak::Line));

    text.push(Part::<Stanza>::Native(
        "A canvas painted with secret pearls.".to_owned(),
    ));
    text.push(Part::<Stanza>::FormatBreak(FormatBreak::Paragraph));
    text.push(Part::<Stanza>::Anchor(Stanza::Two));

    text.push(Part::<Stanza>::Native("The moonlight ".to_owned()));
    text.push(Part::<Stanza>::Correction(Correction {
        versions: vec![
            Present::Native("illuminates".to_owned()),
            Present::Native("bathes".to_owned()),
        ],
    }));
    text.push(Part::<Stanza>::Native(" the quiet stream,".to_owned()));
    text.push(Part::<Stanza>::FormatBreak(FormatBreak::Line));

    text.push(Part::<Stanza>::Native(
        "Where echoes linger of a distant dream,".to_owned(),
    ));
    text.push(Part::<Stanza>::FormatBreak(FormatBreak::Line));

    text.push(Part::<Stanza>::Native(
        "Soft breezes carry the ta".to_owned(),
    ));
    text.push(Part::<Stanza>::Lacuna(Uncertain::<Lacuna>::new(8, None)));
    text.push(Part::<Stanza>::Native("ld,".to_owned()));
    text.push(Part::<Stanza>::FormatBreak(FormatBreak::Line));

    text.push(Part::<Stanza>::Native(
        "Of hearts entwined and lo".to_owned(),
    ));
    text.push(Part::<Stanza>::Lacuna(Uncertain::<Lacuna>::new(8, None)));
    text.push(Part::<Stanza>::Native("d.".to_owned()));
    text.push(Part::<Stanza>::FormatBreak(FormatBreak::Page));
    assert_eq!(
        Text::<Stanza> { parts: text }.render::<ExampleAtgDialect>(),
        res
    );
}

#[test]
fn render_part() {
    let native = Part::<Stanza>::Native("native".to_owned());
    assert_eq!(native.render::<ExampleAtgDialect>(), "native");
    let illeg =
        Part::<Stanza>::Illegible(Uncertain::<Illegible>::new(2, Some("?s".to_owned())));
    assert_eq!(illeg.render::<ExampleAtgDialect>(), "^(2)(?s)");
    let lacuna = Part::<Stanza>::Lacuna(Uncertain::<Lacuna>::new(4, None));
    assert_eq!(lacuna.render::<ExampleAtgDialect>(), "~(4)");
    let p1 = Present::Native("some text or smth...".to_owned());
    let p2 = Present::Illegible(Uncertain::<Illegible>::new(2, Some("os".to_owned())));
    let correction = Part::<Stanza>::Correction(Correction {
        versions: vec![p1, p2],
    });
    assert_eq!(
        correction.render::<ExampleAtgDialect>(),
        "&(some text or smth...)(^(2)(os))"
    );
    let format_break = Part::<Stanza>::FormatBreak(FormatBreak::Column);
    assert_eq!(format_break.render::<ExampleAtgDialect>(), "/(column)");
    let anchor = Part::<Stanza>::Anchor(Stanza::Two);
    assert_eq!(anchor.render::<ExampleAtgDialect>(), "§(2)");
}

#[test]
fn render_correction() {
    let p1 = Present::Native("some text or smth...".to_owned());
    let illeg = Present::Illegible(Uncertain::<Illegible>::new(2, Some("os".to_owned())));
    let correction = Correction {
        versions: vec![p1, illeg],
    };
    assert_eq!(
        correction.render::<ExampleAtgDialect>(),
        "&(some text or smth...)(^(2)(os))"
    );
}

#[test]
fn render_present() {
    let p1 = Present::Native("some text or smth...".to_owned());
    assert_eq!(
        p1.render::<ExampleAtgDialect>(),
        "some text or smth...".to_owned()
    );
    let illeg = Present::Illegible(Uncertain::<Illegible>::new(2, Some("os".to_owned())));
    assert_eq!(illeg.render::<ExampleAtgDialect>(), "^(2)(os)");
}

#[test]
fn render_uncertain_illegible() {
    let illeg = Uncertain::<Illegible>::new(2, Some("os".to_owned()));
    assert_eq!(illeg.render::<ExampleAtgDialect>(), "^(2)(os)");
}

#[test]
fn render_uncertain_lacuna() {
    let lacuna = Uncertain::<Lacuna>::new(2, Some("os".to_owned()));
    assert_eq!(lacuna.render::<ExampleAtgDialect>(), "~(2)(os)");
}

#[test]
fn render_format_break() {
    assert_eq!(FormatBreak::Line.render::<ExampleAtgDialect>(), "/(line)");
    assert_eq!(
        FormatBreak::Column.render::<ExampleAtgDialect>(),
        "/(column)"
    );
    assert_eq!(
        FormatBreak::Paragraph.render::<ExampleAtgDialect>(),
        "/(paragraph)"
    );
    assert_eq!(FormatBreak::Page.render::<ExampleAtgDialect>(), "/(page)");
}

#[test]
fn test_escape_until_control_point() {
    let input = "asd(";
    let parsed = escape_until_control_point::<Stanza, ExampleAtgDialect>(input).unwrap();
    assert_eq!(parsed, ("asd".to_owned(), Some('('), "(", 4));

    let input = "asd^(1)(c)";
    let parsed = escape_until_control_point::<Stanza, ExampleAtgDialect>(input).unwrap();
    assert_eq!(parsed, ("asd".to_owned(), Some('^'), "^(1)(c)", 4));

    let input = "a\\41d(";
    let parsed = escape_until_control_point::<Stanza, ExampleAtgDialect>(input).unwrap();
    assert_eq!(parsed, ("aAd".to_owned(), Some('('), "(", 6));
}

#[test]
fn parse_part() {
    let native = "abcdef";
    let parsed_native = Part::<Stanza>::parse::<ExampleAtgDialect>(native).unwrap();
    assert_eq!(parsed_native, (Part::<Stanza>::Native("abcdef".to_owned()), ""));

    let illeg = "^(3)(abc)";
    let parsed_illeg = Part::<Stanza>::parse::<ExampleAtgDialect>(illeg).unwrap();
    assert_eq!(parsed_illeg, (Part::<Stanza>::Illegible(Uncertain::<Illegible>::new(3, Some("abc".to_owned()))), ""));
    let illeg = "^(2)";
    let parsed_illeg = Part::<Stanza>::parse::<ExampleAtgDialect>(illeg).unwrap();
    assert_eq!(parsed_illeg, (Part::<Stanza>::Illegible(Uncertain::<Illegible>::new(2, None)), ""));
    let illeg = "^(2)()";
    let parsed_illeg = Part::<Stanza>::parse::<ExampleAtgDialect>(illeg).unwrap();
    assert_eq!(parsed_illeg, (Part::<Stanza>::Illegible(Uncertain::<Illegible>::new(2, None)), ""));

    let lacuna = "~(3)(abc)";
    let parsed_lacuna = Part::<Stanza>::parse::<ExampleAtgDialect>(lacuna).unwrap();
    assert_eq!(parsed_lacuna, (Part::<Stanza>::Lacuna(Uncertain::<Lacuna>::new(3, Some("abc".to_owned()))), ""));
    let lacuna = "~(2)some";
    let parsed_lacuna = Part::<Stanza>::parse::<ExampleAtgDialect>(lacuna).unwrap();
    assert_eq!(parsed_lacuna, (Part::<Stanza>::Lacuna(Uncertain::<Lacuna>::new(2, None)), "some"));
    let lacuna = "~(2)()";
    let parsed_lacuna = Part::<Stanza>::parse::<ExampleAtgDialect>(lacuna).unwrap();
    assert_eq!(parsed_lacuna, (Part::<Stanza>::Lacuna(Uncertain::<Lacuna>::new(2, None)), ""));

    let input = "/(line)";
    let parsed = Part::<Stanza>::parse::<ExampleAtgDialect>(input);
    assert_eq!(parsed, Ok((Part::<Stanza>::FormatBreak(FormatBreak::Line), "")));

    let input = "#(comment)";
    let parsed = Part::<Stanza>::parse::<ExampleAtgDialect>(input);
    assert_eq!(parsed, Ok((Part::<Stanza>::Native("".to_owned()), "")));

    let input = "&(optiona)(optionb)";
    let parsed = Part::<Stanza>::parse::<ExampleAtgDialect>(input);
    assert_eq!(parsed, Ok((Part::<Stanza>::Correction(Correction { versions: vec![Present::Native("optiona".to_owned()), Present::Native("optionb".to_owned())] }), "")));

    let input = "&(optiona)no option";
    let parsed = Part::<Stanza>::parse::<ExampleAtgDialect>(input);
    assert_eq!(parsed, Ok((Part::<Stanza>::Correction(Correction { versions: vec![Present::Native("optiona".to_owned())] }), "no option")));
}

#[test]
fn parse_native() {
    let native = "a";
    let (parsed_native, _) = Part::<Stanza>::parse_native::<ExampleAtgDialect>(native).unwrap();
    assert_eq!(parsed_native, Part::<Stanza>::Native(native.to_owned()));

    let native = "a^(1)(b)";
    let (parsed_native, remainder) = Part::<Stanza>::parse_native::<ExampleAtgDialect>(native).unwrap();
    assert_eq!(parsed_native, Part::<Stanza>::Native("a".to_owned()));
    assert_eq!(remainder, "^(1)(b)");
}

#[test]
fn test_escape_one_if_required() {
    let input = "a";
    let (char, remainder, offset) = escape_one_if_required::<ExampleAtgDialect>(input).unwrap();
    assert_eq!(char, 'a');
    assert_eq!(remainder, "");
    assert_eq!(offset, 1);

    let input = "^";
    let (char, remainder, offset) = escape_one_if_required::<ExampleAtgDialect>(input).unwrap();
    assert_eq!(char, '^');
    assert_eq!(remainder, "");
    assert_eq!(offset, 1);

    let input = "\\000041somestuff";
    let (char, remainder, offset) = escape_one_if_required::<ExampleAtgDialect>(input).unwrap();
    assert_eq!(char, '\u{41}');
    assert_eq!(remainder, "somestuff");
    assert_eq!(offset, 7);

    let input = "";
    let res = escape_one_if_required::<ExampleAtgDialect>(input);
    assert_eq!(res, Err("".to_owned()));
}

#[test]
fn test_parse_anchor(){
    let input = "(1)asdf";
    let parsed = Part::<Stanza>::parse_anchor::<ExampleAtgDialect>(input).unwrap();
    assert_eq!(parsed, (Stanza::One,  "asdf"))
}

#[test]
fn parse_uncertain() {
    let input = "(2)(abc)";
    let parsed = Uncertain::<Illegible>::parse::<Stanza, ExampleAtgDialect>(&input).unwrap();
    assert_eq!(parsed, (Uncertain::<Illegible>::new(2, Some("abc".to_owned())), ""));

    let input = "(2)(\\g)";
    let parsed = Uncertain::<Illegible>::parse::<Stanza, ExampleAtgDialect>(&input);
    assert_eq!(parsed, Err(AtgParseError::new(4, AtgParseErrorReason::EscapeMalformed("\\g)".to_owned()))))
}

#[test]
fn parse_render() {
    let input = "^(1)(a)";
    let parsed = Text::<Stanza>::parse::<ExampleAtgDialect>(input).unwrap();
    assert_eq!(parsed.render::<ExampleAtgDialect>(), input);

    let res = "§(1)\
    lo~(8)d.\
    ";
    let parsed = Text::<Stanza>::parse::<ExampleAtgDialect>(res).unwrap();
    assert_eq!(parsed.render::<ExampleAtgDialect>(), res);

    let res = "§(1)\
    In twilight's glow, the sha^(2)(do)ws dance,/(line)\
    Whispers of dre^(1)(a)ms in a fleeting trance,/(line)\
    Stars awaken^(1)(,) the night unfurls,/(line)\
    A canvas painted with secret pearls./(paragraph)\
    §(2)\
    The moonlight &(illuminates)(bathes) the quiet stream,/(line)\
    Where echoes linger of a distant dream,/(line)\
    Soft breezes carry the ta~(8)ld,/(line)\
    Of hearts entwined and lo~(8)d./(page)\
    ";
    let parsed = Text::<Stanza>::parse::<ExampleAtgDialect>(res).unwrap();
    assert_eq!(parsed.render::<ExampleAtgDialect>(), res);
}
