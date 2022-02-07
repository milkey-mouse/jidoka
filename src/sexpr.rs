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
}

// this way we can do bytes.next()?
impl From<Option<u8>> for ParseError {
    fn from(option: Option<u8>) -> ParseError {
        assert_eq!(option, None);
        ParseError::IO(io::Error::from(io::ErrorKind::UnexpectedEof))
    }
}

impl Expr {
    fn eat_char<T: BufRead>(bytes: &mut PeekableFile<T>) -> Result<char, ParseError> {
        // eat_char needs to see at least 4 chars
        // to be able to peek any UTF-8 code point
        const _: () = assert!(MAX_LOOKAHEAD >= 4);

        let peek = bytes.peek()?;
        match from_utf8(peek) {
            // there will definitely be a char to unwrap in >=4 bytes
            // of valid UTF-8 text: any code point fits within our buf
            Ok(s) => Ok(s.chars().next().unwrap()),
            Err(e) => match e.valid_up_to() {
                0 => Err(ParseError::Utf8(e)),
                // SAFETY: valid_up_to() tells us this string
                // is valid UTF-8 up to the index it returns
                i => {
                    let str = unsafe { from_utf8_unchecked(&peek[..i]) };
                    if let Some(c) = str.chars().next() {
                        bytes.consume(i);
                        Ok(c)
                    } else {
                        Err(ParseError::Utf8(e))
                    }
                }
            },
        }
    }

    fn parse<T: BufRead>(bytes: &mut PeekableFile<T>) -> Result<Self, ParseError> {
        // TODO: would a lookup table be faster than match?

        // TODO: we might want to handle non-ASCII whitespace as well
        while let Some(_) = bytes.next_if(|c| (c as char).is_whitespace()).transpose()? {}

        match bytes.peek()? {
            [] => Err(ParseError::IO(io::Error::from(
                io::ErrorKind::UnexpectedEof,
            ))),
            [b'\'', b'(', ..] => {
                bytes.consume(2);
                Ok(Self::Pair(Box::new([
                    Self::Symbol(*symbol::QUOTE),
                    Self::parse(bytes)?,
                ])))
            }
            [b'`', b'(', ..] => {
                bytes.consume(2);
                Ok(Self::Pair(Box::new([
                    Self::Symbol(*symbol::QUASIQUOTE),
                    Self::parse(bytes)?,
                ])))
            }
            [b',', b'(', ..] => {
                bytes.consume(2);
                Ok(Self::Pair(Box::new([
                    Self::Symbol(*symbol::UNQUOTE),
                    Self::parse(bytes)?,
                ])))
            }
            [b',', b'@', b'(', ..] => {
                bytes.consume(3);
                Ok(Self::Pair(Box::new([
                    Self::Symbol(*symbol::UNQUOTE_SPLICING),
                    Self::parse(bytes)?,
                ])))
            }
            [b'#', b'\'', b'(', ..] => {
                bytes.consume(3);
                Ok(Self::Pair(Box::new([
                    Self::Symbol(*symbol::SYNTAX),
                    Self::parse(bytes)?,
                ])))
            }
            [b'#', b'`', b'(', ..] => {
                bytes.consume(3);
                Ok(Self::Pair(Box::new([
                    Self::Symbol(*symbol::QUASISYNTAX),
                    Self::parse(bytes)?,
                ])))
            }
            [b'#', b',', b'(', ..] => {
                bytes.consume(3);
                Ok(Self::Pair(Box::new([
                    Self::Symbol(*symbol::UNSYNTAX),
                    Self::parse(bytes)?,
                ])))
            }
            [b'#', b',', b'@', b'(', ..] => {
                bytes.consume(4);
                Ok(Self::Pair(Box::new([
                    Self::Symbol(*symbol::UNSYNTAX_SPLICING),
                    Self::parse(bytes)?,
                ])))
            }
            //[b'(', b')'] =>
            [b'(', ..] => Ok(Self::Vector({
                assert_eq!(bytes.next().transpose()?, Some(b'(')); // consume opening (

                let mut v = Vec::with_capacity(2); // average expr has two children(?) TODO
                while let Some(_) = bytes.next_if(|c| (c as char).is_whitespace()).transpose()? {
                    v.push(Self::parse(bytes)?);
                }

                assert_eq!(bytes.next().transpose()?, Some(b')')); // consume closing )

                v.into_boxed_slice()
            })),
            [b'"', ..] => Ok(Self::String(Symbol::from({
                let bytes = bytes
                    .skip(1)
                    .take_while(|r| r.as_ref().map_or(true, |c| *c != b'"'))
                    .collect::<Result<Vec<u8>, io::Error>>()?;

                String::from_utf8(bytes).unwrap().into_boxed_str()
            }))),
            [b'\'', ..] => Ok(Self::Character({
                assert_eq!(bytes.next().transpose()?, Some(b'\'')); // consume opening '

                let c = Self::eat_char(bytes).expect("char literal is not valid UTF-8");

                assert_eq!(bytes.next().transpose()?, Some(b'\'')); // consume closing '

                c
            })),
            [b'+' | b'-' | b'0'..=b'9', ..] => Ok(Self::Number({
                // TODO: this could theoretically be implemented in a streaming manner
                let bytes = iter::from_fn(|| {
                    bytes.next_if(|b| !(b as char).is_ascii_whitespace() && b != b')')
                })
                .collect::<Result<Vec<u8>, io::Error>>()?;

                Integer::parse(bytes).expect("weird int").complete()
            })),
            _ => Ok(Self::Symbol(Symbol::from({
                let bytes = iter::from_fn(|| {
                    bytes.next_if(|c| !(c as char).is_ascii_whitespace() && c != b')')
                })
                .collect::<Result<Vec<u8>, io::Error>>()?;

                String::from_utf8(bytes).unwrap().into_boxed_str()
            }))),
        }
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
    v: impl IntoIterator<Item = impl fmt::Display>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    let mut v = v.into_iter();

    write!(f, "{}(", prefix)?;
    if let Some(first) = v.next() {
        write!(f, "{}", first)?;
        for elem in v {
            write!(f, "{}", elem)?;
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
            Self::Vector(v) => print_vector("#", v.into_iter(), f),
            Self::ByteVector(bv) => print_vector("#vu8", bv.into_iter(), f),
            Self::EmptyList => write!(f, "()"),
        }
    }
}
