/// Generic S-expression parser
// TODO: Get this to the point it can parse R6RS Scheme.
use crate::{
    file,
    symbol::{self, Symbol},
};
use rug::{Complete, Integer};
use std::{
    fmt,
    fs::File,
    io::{self, BufRead, BufReader, Read},
    iter::{self, Peekable},
    mem,
    str::{from_utf8, from_utf8_unchecked, FromStr, Utf8Error},
};
use thiserror::Error;

pub enum Expr {
    // TODO: should String also use Symbol?
    // will it work with string concatenations?
    // will we even need to do string concatenations?
    Boolean(bool), // #t
    // TODO: extend numeric tower
    Number(Integer),       // 1234
    Character(char),       // '💩'
    String(Symbol),        // "hello"
    Symbol(Symbol),        // hello
    Pair(Box<[Expr; 2]>),  // (...)
    Vector(Box<[Expr]>),   // #("one" '2' 3)
    ByteVector(Box<[u8]>), // #vu8(1 2 3)
    EmptyList,             // ()
}

// TODO: (syntax) error handling

// length in bytes of the largest token we want to peek
const MAX_LOOKAHEAD: usize = b"#vu8(".len();
type PeekableFile<T> = file::PeekableFile<T, MAX_LOOKAHEAD>;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("error reading input")]
    IO(#[from] io::Error),
    #[error("invalid UTF-8")]
    Utf8(#[from] Utf8Error),
    #[error("unexpected character '{}'{}", .0, Pretty(.1))]
    UnexpectedCharacter(char, &'static [char]),
    // TODO: ParseError::IO(io::Error::from(io::ErrorKind::UnexpectedEof)) or custom variant?
    #[error("unexpected EOF")]
    UnexpectedEOF,
}

struct Pretty<'a>(&'a [char]);

impl<'a> fmt::Display for Pretty<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            [] => Ok(()),
            [item] => {
                write!(f, "(expected '{}')", item)
            }
            [first, last] => {
                write!(f, "(expected '{}' or '{}')", first, last)
            }
            [first, rest @ .., last] => {
                write!(f, "(expected one of '{}'", first)?;
                for c in rest {
                    write!(f, ", '{}'", c)?;
                }
                write!(f, ", or '{}')", last)
            }
        }
    }
}

/*// this way we can do bytes.next()?
impl From<Option<u8>> for ParseError {
    fn from(option: Option<u8>) -> ParseError {
        assert_eq!(option, None); // TODO
        ParseError::UnexpectedEOF
    }
}*/

impl Expr {
    pub fn parse_pair<T: BufRead>(bytes: &mut PeekableFile<T>) -> Result<Self, ParseError> {
        Ok(if bytes.take_if_eq(b")")? {
            Self::EmptyList
        } else {
            Self::Pair(Box::new([Self::parse(bytes)?, Self::parse_pair(bytes)?]))
        })
    }

    pub fn parse<T: BufRead>(bytes: &mut PeekableFile<T>) -> Result<Self, ParseError> {
        // TODO: would a lookup table be faster than match?

        // TODO: we might want to handle non-ASCII whitespace as well
        while let Some(()) =
            bytes.try_take_char(|c| if c.is_whitespace() { Some(()) } else { None })??
        {}

        // yeet comments
        if bytes.take_if_eq(b";")? {
            // TODO: there are other newline chars we should handle
            // consume a comment spanning from a ; to the next newline
            while let Some(()) =
                bytes.try_take_char(|c| if dbg!(c) != '\n' { Some(()) } else { None })??
            {
            }
            // consume the newline itself
            bytes.try_take_char(Option::Some)??;
        }

        bytes.take_if(|[c]| { dbg!(*c as char); false });

        if bytes.take_if_eq(b"\\(")? {
            Ok(Self::Pair(Box::new([
                Self::Symbol(*symbol::QUOTE),
                Self::parse(bytes)?,
            ])))
        } else if bytes.take_if_eq(b"`(")? {
            Ok(Self::Pair(Box::new([
                Self::Symbol(*symbol::QUASIQUOTE),
                Self::parse(bytes)?,
            ])))
        } else if bytes.take_if_eq(b",(")? {
            Ok(Self::Pair(Box::new([
                Self::Symbol(*symbol::UNQUOTE),
                Self::parse(bytes)?,
            ])))
        } else if bytes.take_if_eq(b",@(")? {
            Ok(Self::Pair(Box::new([
                Self::Symbol(*symbol::UNQUOTE_SPLICING),
                Self::parse(bytes)?,
            ])))
        } else if bytes.take_if_eq(b"#\'(")? {
            Ok(Self::Pair(Box::new([
                Self::Symbol(*symbol::SYNTAX),
                Self::parse(bytes)?,
            ])))
        } else if bytes.take_if_eq(b"#`(")? {
            Ok(Self::Pair(Box::new([
                Self::Symbol(*symbol::QUASISYNTAX),
                Self::parse(bytes)?,
            ])))
        } else if bytes.take_if_eq(b"#,(")? {
            Ok(Self::Pair(Box::new([
                Self::Symbol(*symbol::UNSYNTAX),
                Self::parse(bytes)?,
            ])))
        } else if bytes.take_if_eq(b"#,@(")? {
            Ok(Self::Pair(Box::new([
                Self::Symbol(*symbol::UNSYNTAX_SPLICING),
                Self::parse(bytes)?,
            ])))
        } else if bytes.take_if_eq(b"(")? {
            Self::parse_pair(bytes)
        } else if bytes.take_if_eq(b"#(")? {
            Ok(Self::Vector({
                let mut v = Vec::new();
                while bytes.take_char_if(char::is_whitespace)?? {
                    v.push(Self::parse(bytes)?);
                }

                // TODO: better way than assert!
                // expect(bytes, b')')?;
                assert!(bytes.take_if_eq(b")")?);

                v.into_boxed_slice()
            }))
        } else if let Some(c) = bytes.try_take_char(Option::Some)?? {
            // TODO: populate expected characters
            // should this whole if chain be a macro?
            Err(ParseError::UnexpectedCharacter(c, &[]))
        } else {
            Err(ParseError::UnexpectedEOF)
        }

        /*    //[b'(', b')'] =>
            [b'"', ..] => Ok(Self::String(Symbol::from({
                let bytes = bytes
                    .skip(1)
                    .take_while(|r| r.as_ref().map_or(true, |c| *c != b'"'))
                    .collect::<Result<Vec<u8>, io::Error>>()?;

                String::from_utf8(bytes).unwrap().into_boxed_str()
            }))),
            [b'\'', ..] => Ok(Self::Character({
                assert_eq!(bytes.next().transpose()?, Some(b'\'')); // consume opening '

                let c = bytes.take_char()??;

                assert_eq!(bytes.next().transpose()?, Some(b'\'')); // consume closing '

                c
            })),
            [b'+' | b'-' | b'0'..=b'9', ..] => Ok(Self::Number({
                // TODO: this could theoretically be implemented in a streaming manner
                let bytes = iter::from_fn(|| {
                    bytes.try_take(|[b]| (b as char).is_ascii_whitespace() && b != b')')
                })
                .collect::<Result<Result<Vec<u8>, io::Error>, Utf8Error>>()??;

                Integer::parse(bytes).expect("weird int").complete()
            })),
            _ => Ok(Self::Symbol(Symbol::from({
                // TODO
                let v = iter::from_fn(|| {
                    bytes.try_take_char(|c| !(c.is_whitespace() || c == ')'))
                        .map(|r| r.transpose()).transpose()
                })
                .collect::<Result<Result<String, io::Error>, Utf8Error>>()??.into_boxed_str()
            }))),
        }*/
    }

    /*pub fn parse_all<'a>(i: impl IntoIterator<Item = u8> + 'a) -> impl Iterator<Item = Self> + 'a {
        todo!()
        /*let mut i = i.into_iter();
        iter::from_fn(move || match Self::parse(&mut i) {
            Ok(Expr::Empty) => None,
            //Err((Expr::Empty, _)) => None,
            Ok(expr) => Some(expr),
            Err((expr, _)) => Some(expr),
        })*/
    }*/
}

impl FromStr for Expr {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, ParseError> {
        // TODO: assert no trailing chars?
        Self::parse(&mut s.as_bytes().into())
    }
}

fn print_vector(
    prefix: &str,
    i: impl IntoIterator<Item = impl fmt::Display>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    let mut i = i.into_iter();

    write!(f, "{}(", prefix)?;
    if let Some(first) = i.next() {
        write!(f, "{}", first)?;
        for elem in i {
            write!(f, " {}", elem)?;
        }
    }
    write!(f, ")")
}

// TODO: QuickCheck end-to-end test that print(parse(expr)) == expr for all well-formed expr
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Boolean(true) => write!(f, "#t"),
            Self::Boolean(false) => write!(f, "#f"),
            Self::Number(c) => write!(f, "{}", c),
            Self::Character(c) => write!(f, "\'{}\'", c),
            Self::String(s) => write!(f, "\"{}\"", s),
            Self::Symbol(s) => write!(f, "{}", s),
            // TODO: "re-sugar" lists when printing
            Self::Pair(b) => write!(f, "({} {})", b[0], b[1]),
            Self::Vector(v) => print_vector("#(", v.into_iter(), f),
            Self::ByteVector(bv) => print_vector("#vu8(", bv.into_iter(), f),
            Self::EmptyList => write!(f, "()"),
        }
    }
}
