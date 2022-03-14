/// Generic S-expression parser
// TODO: Get this to the point it can parse R7RS-small Scheme.
use crate::{
    symbol::{self, Symbol},
    utf8,
};
use lighter::lighter;
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
    //Boolean(bool), // #t
    // TODO: extend numeric tower
    //Number(Integer),       // 1234
    Character(char),       // '💩'
    String(Symbol),        // "hello"
    Symbol(Symbol),        // hello
    Pair(Box<[Expr; 2]>),  // (...)
    Vector(Box<[Expr]>),   // #("one" '2' 3)
    //ByteVector(Box<[u8]>), // #vu8(1 2 3)
    EmptyList,             // ()
}

// TODO: (syntax) error handling

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
    fn parse_block_comment<I: Iterator<Item = io::Result<u8>>>(
        bytes: &mut Peekable<I>,
    ) -> Result<bool, ParseError> {
        loop {
            return lighter! { match bytes.by_ref() {
                // end our block comment with |#
                Prefix("|#") => Ok(false),
                // block comments may be nested
                Prefix("#|") => Self::parse_block_comment(bytes),
                // otherwise consume a character
                // TODO: throw error if EOF
                _ => match bytes.next() {
                    Some(Ok(b')')) => Ok(true),
                    Some(Ok(_)) => continue,
                    Some(Err(e)) => Err(ParseError::IO(e)),
                    None => Err(ParseError::UnexpectedEOF),
                }
            }}?;
        }
    }

    fn parse_pair<I: Iterator<Item = io::Result<u8>>>(
        bytes: &mut Peekable<I>,
    ) -> Result<Self, ParseError> {
        match bytes.peek() {
            Some(Ok(b')')) => {
                bytes.next();
                Ok(Self::EmptyList)
            }
            Some(Ok(_)) => Ok(Self::Pair(Box::new([
                Self::parse(bytes)?,
                Self::parse_pair(bytes)?,
            ]))),
            Some(Err(_)) => match bytes.next() {
                Some(Err(e)) => Err(ParseError::IO(e)),
                _ => unreachable!(),
            },
            None => Err(ParseError::UnexpectedEOF),
        }
    }

    pub fn parse<I: Iterator<Item = io::Result<u8>>>(
        bytes: &mut Peekable<I>,
    ) -> Result<Self, ParseError> {
        // ignore whitespace and single-line comments
        /*while let Some(()) = bytes.try_take_char(|c| {
            if c.is_whitespace() || c == '\n' {
                Some(())
            } else {
                None
            }
        })?? {} TODO comments*/

        // TODO: ignore multiline comments

        // TODO: remove this line
        /*bytes.take_if(|[c]| {
            dbg!(*c as char);
            false
        })?;*/

        loop {
            return lighter! { match bytes.by_ref() {
                // ignore whitespace
                // (match all characters in the Unicode White_Space class)
                Prefix('\u{0009}') | Prefix('\u{000a}') | Prefix('\u{000b}')
                | Prefix('\u{000c}') | Prefix('\u{000d}') | Prefix('\u{0020}')
                | Prefix('\u{0085}') | Prefix('\u{00a0}') | Prefix('\u{1680}')
                | Prefix('\u{2000}') | Prefix('\u{2001}') | Prefix('\u{2002}')
                | Prefix('\u{2003}') | Prefix('\u{2004}') | Prefix('\u{2005}')
                | Prefix('\u{2006}') | Prefix('\u{2007}') | Prefix('\u{2008}')
                | Prefix('\u{2009}') | Prefix('\u{200a}') | Prefix('\u{2028}')
                | Prefix('\u{2029}') | Prefix('\u{202f}') | Prefix('\u{205f}')
                | Prefix('\u{3000}') => continue,

                // single-line comments
                Prefix(';') => {
                    // ignore all remaining characters in the line
                    loop {
                        lighter! { match bytes.by_ref() {
                            Prefix('\n') | Prefix('\r') => break,
                            _ => continue,
                        }}?;
                    }
                    continue;
                }

                // block comments (may be nested)
                Prefix("#|") => {
                    Self::parse_block_comment(bytes)?;
                    continue;
                }

                // booleans
                // TODO: this is really annoying, can we create a nested macro
                // for "symbol followed by space or close paren"?
                /*Prefix("#t\u{0009}") | Prefix("#t\u{000a}") | Prefix("#t\u{000b}") |
                Prefix("#t\u{000c}") | Prefix("#t\u{000d}") | Prefix("#t\u{0020}") |
                Prefix("#t\u{0085}") | Prefix("#t\u{00a0}") | Prefix("#t\u{1680}") |
                Prefix("#t\u{2000}") | Prefix("#t\u{2001}") | Prefix("#t\u{2002}") |
                Prefix("#t\u{2003}") | Prefix("#t\u{2004}") | Prefix("#t\u{2005}") |
                Prefix("#t\u{2006}") | Prefix("#t\u{2007}") | Prefix("#t\u{2008}") |
                Prefix("#t\u{2009}") | Prefix("#t\u{200a}") | Prefix("#t\u{2028}") |
                Prefix("#t\u{2029}") | Prefix("#t\u{202f}") | Prefix("#t\u{205f}") |
                Prefix("#t\u{3000}") => continue, */
                //Prefix("#t") => Ok(Self::Boolean(true)), // TODO

                /*Prefix("#f\u{0009}") | Prefix("#f\u{000a}") | Prefix("#f\u{000b}") |
                Prefix("#f\u{000c}") | Prefix("#f\u{000d}") | Prefix("#f\u{0020}") |
                Prefix("#f\u{0085}") | Prefix("#f\u{00a0}") | Prefix("#f\u{1680}") |
                Prefix("#f\u{2000}") | Prefix("#f\u{2001}") | Prefix("#f\u{2002}") |
                Prefix("#f\u{2003}") | Prefix("#f\u{2004}") | Prefix("#f\u{2005}") |
                Prefix("#f\u{2006}") | Prefix("#f\u{2007}") | Prefix("#f\u{2008}") |
                Prefix("#f\u{2009}") | Prefix("#f\u{200a}") | Prefix("#f\u{2028}") |
                Prefix("#f\u{2029}") | Prefix("#f\u{202f}") | Prefix("#f\u{205f}") |
                Prefix("#f\u{3000}") => continue, */
                //Prefix("#f") => Ok(Self::Boolean(false)),

                // numbers
                // TODO

                // chars
                Prefix("'") => {
                    let c = utf8::next_code_point(bytes);
                    match bytes.next() {
                        Some(b'\'') => Ok(Self::Char(c)),
                        Some(c) => Err(ParseError::UnexpectedCharacter(c, &['\''])),
                        None => Err(ParseError::UnexpectedEOF),
                    }
                }

                // strings
                Prefix('"') => Ok(Self::String(Symbol::from(std::str::from_utf8(
                    bytes
                        .take_while(|b| matches!(b, Ok(b'"')))
                        .collect::<io::Result<Vec<u8>>>()?
                        .as_slice(),
                )?))),

                // special forms
                Prefix("\\(") => Ok(Self::Pair(Box::new([
                    Self::Symbol(*symbol::QUOTE),
                    Self::parse(bytes)?,
                ]))),
                Prefix("`(") => Ok(Self::Pair(Box::new([
                    Self::Symbol(*symbol::QUASIQUOTE),
                    Self::parse(bytes)?,
                ]))),
                Prefix(",(") => Ok(Self::Pair(Box::new([
                    Self::Symbol(*symbol::UNQUOTE),
                    Self::parse(bytes)?,
                ]))),
                Prefix(",@(") => Ok(Self::Pair(Box::new([
                    Self::Symbol(*symbol::UNQUOTE_SPLICING),
                    Self::parse(bytes)?,
                ]))),
                Prefix("#'(") => Ok(Self::Pair(Box::new([
                    Self::Symbol(*symbol::SYNTAX),
                    Self::parse(bytes)?,
                ]))),
                Prefix("#`(") => Ok(Self::Pair(Box::new([
                    Self::Symbol(*symbol::QUASISYNTAX),
                    Self::parse(bytes)?,
                ]))),
                Prefix("#,(") => Ok(Self::Pair(Box::new([
                    Self::Symbol(*symbol::UNSYNTAX),
                    Self::parse(bytes)?,
                ]))),
                Prefix("#,@(") => Ok(Self::Pair(Box::new([
                    Self::Symbol(*symbol::UNSYNTAX_SPLICING),
                    Self::parse(bytes)?,
                ]))),

                // pairs or cons lists
                Prefix('(') => Self::parse_pair(bytes),

                // vectors: #("one" '2' 3)
                Prefix("#(") => Ok(Self::Vector({
                    // TODO
                    let mut v = Vec::new();

                    while bytes.peek() != Some(&b')') {
                        v.push(Self::parse(bytes)?);
                        // TODO: expect at least one whitespace character between items?
                    }

                    v.into_boxed_slice()
                    todo!()
                })),

                // TODO bytevectors

                // symbols/identifiers
                Prefix('|') => Ok(Self::Symbol(Symbol::from(std::str::from_utf8(
                    bytes
                        .take_while(|b| matches!(b, Ok(b'|')))
                        .collect::<io::Result<Vec<u8>>>()?
                        .as_slice(),
                )?))),
                Prefix(peeked) => {
                    let mut buf = peeked.into_iter().collect::<Vec<u8>>();

                    // add characters to our buf until we see either whitespace
                    // or a close-paren
                    // TODO: this is inelegant
                    loop {
                        match bytes.peek() {
                            Some(Ok(b')')) => break,
                            Some(Ok(_)) => lighter! {
                                match bytes.by_ref() {
                                    // TODO: use the real algorithm (unicode xid?
                                    // what do real lisps do?) for finding the end/
                                    // span of an identifier
                                    Prefix('\u{0009}') | Prefix('\u{000a}') | Prefix('\u{000b}') |
                                    Prefix('\u{000c}') | Prefix('\u{000d}') | Prefix('\u{0020}') |
                                    Prefix('\u{0085}') | Prefix('\u{00a0}') | Prefix('\u{1680}') |
                                    Prefix('\u{2000}') | Prefix('\u{2001}') | Prefix('\u{2002}') |
                                    Prefix('\u{2003}') | Prefix('\u{2004}') | Prefix('\u{2005}') |
                                    Prefix('\u{2006}') | Prefix('\u{2007}') | Prefix('\u{2008}') |
                                    Prefix('\u{2009}') | Prefix('\u{200a}') | Prefix('\u{2028}') |
                                    Prefix('\u{2029}') | Prefix('\u{202f}') | Prefix('\u{205f}') |
                                    Prefix('\u{3000}') => break,
                                    Prefix(s) => buf.extend(s),
                            }}?,
                            Some(Err(_)) => match bytes.next() {
                                Some(Err(e)) => return Err(ParseError::IO(e)),
                                _ => unreachable!(),
                            },
                            None => return Err(ParseError::UnexpectedEOF),
                        }
                    }

                    if buf.is_empty() {
                        Err(ParseError::UnexpectedEOF)
                    } else {
                        Ok(Self::Symbol(Symbol::from(std::str::from_utf8(&buf)?)))
                    }
                }

                c => panic!(
                    "unexpected character '{}' before {}",
                    std::str::from_utf8(&c).unwrap(),
                    std::str::from_utf8(&bytes.map(Result::unwrap).collect::<Vec<u8>>()).unwrap()
                ),
                //} else if let Some(s) = bytes.take_while
                /*} else if let Some(c) = bytes.try_take_char(Option::Some)?? {
                    // TODO: populate expected characters
                    // should this whole if chain be a macro?
                    Err(ParseError::UnexpectedCharacter(c, &[]))
                } else {
                    Err(ParseError::UnexpectedEOF)
                }*/
            }}?;
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
        Self::parse(&mut s.bytes().map(Result::Ok).peekable())
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
            //Self::Boolean(true) => write!(f, "#t"),
            //Self::Boolean(false) => write!(f, "#f"),
            //Self::Number(c) => write!(f, "{}", c),
            Self::Character(c) => write!(f, "\'{}\'", c),
            Self::String(s) => write!(f, "\"{}\"", s),
            Self::Symbol(s) => write!(f, "{}", s),
            // TODO: "re-sugar" lists when printing
            Self::Pair(b) => write!(f, "({} {})", b[0], b[1]),
            Self::Vector(v) => print_vector("#(", v.into_iter(), f),
            //Self::ByteVector(bv) => print_vector("#vu8(", bv.into_iter(), f),
            Self::EmptyList => write!(f, "()"),
        }
    }
}
