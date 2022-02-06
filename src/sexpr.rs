/// Generic S-expression parser
use rug::{Complete, Integer};
use std::{
    fmt,
    fs::File,
    io::{self, BufReader, Read},
    iter::{self, Peekable},
    mem,
    str::{from_utf8, from_utf8_unchecked, FromStr, Utf8Error},
};

pub enum Expr {
    List(Box<[Expr]>),   // (...)
    Symbol(egg::Symbol), // hello
    // TODO: should String also use egg::Symbol?
    // will it work with string concatenations?
    // will we even need to do string concatenations?
    //String(Box<str>),    // "hello"
    String(egg::Symbol), // "hello"
    Char(char),          // '0'
    Num(Integer),        // true
    Empty,               // empty expression, only used internally
                         // (TODO: should this just be Option<Expr>?)
}

/*impl Language for Expr {
    #[inline(always)]
    fn matches(&self, other: &Self) -> bool {
        mem::discriminant(self) == mem::discriminant(other)
    }

    fn children(&self) -> &[Id] {
        match self {
            Self::List(c) => c.as_ref(),
            _ => &[]
        }
    }

    fn children_mut(&mut self) -> &mut [Id] {
        match self {
            Self::List(c) => c.as_mut(),
            _ => &mut []
        }
    }
}*/

// TODO: QuickCheck end-to-end test that print(parse(expr)) == expr for all well-formed expr
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::List(l) => {
                write!(f, "(")?;
                if let Some((first, rest)) = l.split_first() {
                    write!(f, "{}", first)?;
                    for elem in rest {
                        write!(f, " {}", elem)?;
                    }
                }
                write!(f, ")")
            }
            Self::Symbol(s) => write!(f, "{}", s),
            Self::String(s) => write!(f, "\"{}\"", s),
            Self::Char(c) => write!(f, "\'{}\'", c),
            Self::Num(c) => write!(f, "{}", c),
            Self::Empty => Ok(()),
        }
    }
}

// TODO: (syntax) error handling

impl Expr {
    fn eat_char(bytes: &mut impl Iterator<Item = u8>) -> Result<char, Option<Utf8Error>> {
        let mut s = [0u8; 4];
        for i in 0..s.len() {
            s[i] = bytes.next().ok_or(None)?;
            match from_utf8(&s[..=i]) {
                Ok(s) => return Ok(s.chars().next().unwrap()),
                Err(e) if e.error_len() == None => continue,
                Err(e) => return Err(Some(e)),
            }
        }

        // TODO
        unreachable!();
    }

    fn _parse<I: Iterator<Item = u8>>(bytes: &mut Peekable<I>) -> Self {
        // TODO: we might want to handle non-ASCII whitespace as well
        while let Some(_) = bytes.next_if(|&c| (c as char).is_ascii_whitespace()) {}
        match bytes.peek() {
            None => Expr::Empty, // only used when asked to parse an empty string
            Some(b'(') => Self::List({
                assert_eq!(bytes.next(), Some(b'(')); // consume opening (

                let mut v = Vec::with_capacity(2); // average expr has two children(?) TODO
                while bytes.peek() != Some(&b')') && bytes.peek() != None {
                    v.push(Self::_parse(bytes));
                }

                assert_eq!(bytes.next(), Some(b')')); // consume closing )

                v.into_boxed_slice()
            }),
            Some(b'"') => Self::String(egg::Symbol::from({
                let bytes = bytes
                    .skip(1)
                    .take_while(|&c| c != b'"')
                    .collect::<Vec<u8>>();

                String::from_utf8(bytes).unwrap().into_boxed_str()
            })),
            Some(b'\'') => Self::Char({
                assert_eq!(bytes.next(), Some(b'\'')); // consume opening '

                let c = Self::eat_char(bytes).expect("char literal is not valid UTF-8");

                assert_eq!(bytes.next(), Some(b'\'')); // consume closing '

                c
            }),
            Some(b'+' | b'-' | b'0'..=b'0') => Self::Num({
                // TODO: this could theoretically be implemented in a streaming manner
                let bytes = iter::from_fn(|| {
                    bytes.next_if(|&c| !(c as char).is_ascii_whitespace() && c != b')')
                })
                .collect::<Vec<u8>>();

                Integer::parse(bytes).expect("weird int").complete()
            }),
            Some(_) => Self::Symbol(egg::Symbol::from({
                let bytes = iter::from_fn(|| {
                    bytes.next_if(|&c| !(c as char).is_ascii_whitespace() && c != b')')
                })
                .collect::<Vec<u8>>();
                String::from_utf8(bytes).unwrap().into_boxed_str()
            })),
        }
    }

    pub fn parse(i: impl IntoIterator<Item = u8>) -> Result<Self, (Self, usize)> {
        // keep track of position in string so we can give a
        // location if Self::parse throws an error, and give
        // back the remaining slice of the string that hasn't
        // been parsed if there are trailing characters
        let mut bytes_with_indices = i.into_iter().enumerate();
        let expr = {
            let bytes = bytes_with_indices.by_ref().map(|(_, v)| v);

            let mut in_comment = false;
            let mut bytes_without_comments = bytes
                .filter(move |c| match c {
                    // filter out comments by ignoring characters
                    // between a ';' and the next newline after it
                    b';' => mem::replace(&mut in_comment, true),
                    b'\n' => mem::replace(&mut in_comment, false),
                    _ => !in_comment,
                })
                .peekable();

            // TODO: throw and handle real errors, adding index context
            Self::_parse(&mut bytes_without_comments)
        };

        match bytes_with_indices.next() {
            None => Ok(expr),
            Some((i, _)) => Err((expr, i)),
        }
    }

    pub fn parse_all<'a>(i: impl IntoIterator<Item = u8> + 'a) -> impl Iterator<Item = Self> + 'a {
        let mut i = i.into_iter();
        iter::from_fn(move || match Self::parse(&mut i) {
            Ok(Expr::Empty) => None,
            //Err((Expr::Empty, _)) => None,
            Ok(expr) => Some(expr),
            Err((expr, _)) => Some(expr),
        })
    }

    pub fn parse_bytes_with_remainder(b: &[u8]) -> (Self, &[u8]) {
        match Self::parse(b.iter().copied()) {
            Ok(expr) => (expr, &[]),
            Err((expr, i)) => (expr, &b[i..]),
        }
    }

    pub fn parse_with_remainder(s: &str) -> (Self, &str) {
        let (expr, remainder) = Self::parse_bytes_with_remainder(s.as_bytes());

        // SAFETY: the full string is known to be valid UTF-8
        // (as we called str::as_bytes() to get it), and this
        // index we're slicing at will be immediately after a
        // complete ASCII character that we just parsed
        (expr, unsafe { from_utf8_unchecked(remainder) })
    }
}

impl FromStr for Expr {
    type Err = (Self, usize); // TODO: real errors

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // TODO: assert no trailing chars?
        Self::parse(s.as_bytes().iter().copied())
    }
}
