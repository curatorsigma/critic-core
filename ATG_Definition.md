# Abstract Transcription Grammar - Definition

This document defines the Abstract Transcription Grammar (hereafter `ATG`) in technical terms.
For a more practical introduction, see [TODO].

# General encoding
`ATG` is defined as a stream of [Unicode Scalar Values](https://www.unicode.org/glossary/#unicode_scalar_value).
Whenever `ATG` is stored digitally, it MUST be encoded as UTF-8.

# Code Point Order
Whenever this document refers to the order of code points in `ATG`, `preceeding` or `following` another code point, the logical (in-memory when encoded) order is refered to.
Languages which are Bidirection or RTL must use the appropriate Unicode Control Characters when required.

# What does ATG represent?
A single ATG structure represent the text as transcribed from a single text (a manuscript, scroll, codex, shard of pottery, ...; synonymously called medium hereafter).
It contains
- The surface text as present on the given medium
- Hints to broken characters, lacunae and scribal corrections
- Optionally: Anchors in some numbering scheme for the work
- Optionally: Word divisions, even when the written language has no word divisions

# ATG Dialects
An ATG Dialect contains the following definitions:
- characters that are part of the language transcribed
- characters used as the various control points

# Miscellaneous other required definitions
## per Work (Corpus)
Anchors may be defined to ease hyperlinking and collation. Any format may be used.

## per Text (Manuscript)
The number of correctors needs to be given (or can be determined automatically by the longest sequence of parameters part of a correction)

# EBNF
The control points defined separately for each dialect are part of this EBNF.
The allowable native points (points of the language transcribed) form the class `NATIVE` in this EBNF.
The format for positional anchors is defined in the dialect and part of this EBNF as `ANCHOR`.
`NUM_OF_CORRECTORS_HANDS` is an integer, which is defined for each text separately (it records the number of correcting hands, including multiple corrections by the same hand, that have been acting on that text)

```
control := escape | start_param | stop_param | illegible | lacuna | anchor | format_break | correction | divisor | non_semantic | comment
non-control := <ANY UNICODE POINT EXCEPT control>
DIGIT := 0-9
HEXDIGIT := DIGIT | a-f | A-F
char := escape HEXDIGIT{2}[HEXDIGIT{2}[HEXDIGIT{2}]] | non-control | escape control
NUMBER := DIGIT+
illegible_def := illegible start_param NUMBER stop_param [start_param NATIVE* stop_param]
lacuna_def := lacuna start_param NUMBER stop_param [start_param NATIVE* stop_param]
anchor_def := anchor start_param ANCHOR stop_param
format_break_def := format_break ("line" | "column" | "paragraph" | "page")
correction_def := correction (start_param (NATIVE* | illegible_def) stop_param){1,NUM_OF_CORRECTORS_HANDS}
comment_def := comment start_param char* stop_param
text := <ANY NUMBER IN ANY ORDER OF: NATIVE* | illegible_def | lacuna_def | correction_def | format_break_def | anchor_def | non_semantic>
```

# Example ATG Dialect EBNF
Consider this example Dialect of ATG, used to transcribed english language (not including all characters that might be useful here for brevity).
As a work, we consider a poem with 2 stanzas which form the anchors.

```
NATIVE := a-z | A-Z | , | . | '
ANCHOR := 1 | 2
escape := \
start_param := (
stop_param := )
illegible := ^
lacuna := ~
anchor := §
format_break := /
correction := &
word_divisor := " "
non_semantic := "\t" | "\n"
comment := "#"
```

Then to transcribe this poem (made up with chatgpt):
```
In twilight's glow, the sha??ws dance,
Whispers of dre?ms in a fleeting trance,
Stars awaken? the night unfurls,
A canvas painted with secret pearls.

The moonlight (originally read 'illuminates'. erased and overwritten with 'bathes') the quiet stream,
Where echoes linger of a distant dream,
Soft breezes carry the ta--------ld,
Of hearts entwined and lo--------d.
```

a correct representation of ATG in the above dialect is (line breaks are \n):
```
§(1)
In twilight's glow, the sha^(2)(do)ws dance,/(line)
Whispers of dre^(1)(a)ms in a fleeting trance,/(line)
Stars awaken^(1)(,) the night unfurls,/(line)
A canvas painted with secret pearls./(paragraph)

§(2)
The moonlight &(illuminates)(bathes) the quiet stream,/(line)
Where echoes linger of a distant dream,/(line)

#(potentially l. "carry the tales of old"??)
Soft breezes carry the ta~(8)ld,/(line)

Of hearts entwined and lo~(8)d./(page)
```
The last `/(page)` may be ommitted or written.

