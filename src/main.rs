use std::{
    fmt, fs, io,
    iter::{self, Peekable},
    str::FromStr,
};

#[derive(Debug)]
enum Expr {
    List(Box<[Expr]>),   // (...)
    Symbol(egg::Symbol), // hello
    // TODO: should Quoted also use egg::Symbol?
    // will it work with string concatenations?
    // will we even need to do string concatenations?
    String(String),      // "hello"
    Char(char),          // '0'
}

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
        }
    }
}

// TODO: (syntax) error handling

impl Expr {
    fn parse<I: Iterator<Item = char>>(s: &mut Peekable<I>) -> Self {
        while let Some(_) = s.next_if(|c| c.is_whitespace()) {}
        match s.peek() {
            // TODO: does the None case mean unbalanced parentheses?
            //None => Self::List(Box::new([])),
            None => panic!("unbalanced parens/unexpected EOF"),
            Some('(') => Self::List({
                assert_eq!(s.next(), Some('(')); // consume opening )

                let mut v = Vec::with_capacity(2); // average expr has two children(?)
                while s.peek() != Some(&')') && s.peek() != None {
                    v.push(Self::parse(s));
                }

                assert_eq!(s.next(), Some(')')); // consume closing )

                v.into_boxed_slice()
            }),
            Some('"') => Self::String(s.skip(1).filter(|c| *c != '"').collect()),
            Some('\'') => Self::Char({
                assert_eq!(s.next(), Some('\'')); // consume opening '

                let c = s.next().unwrap();

                assert_eq!(s.next(), Some('\'')); // consume closing '

                c
            }),
            Some(_) => Self::Symbol(egg::Symbol::from(
                iter::from_fn(|| s.next_if(|c| !c.is_whitespace() && *c != ')'))
                    .collect::<String>(),
            )),
        }
    }
}

impl FromStr for Expr {
    type Err = (); // TODO: real errors

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut chars = s
            .lines()
            .flat_map(|s| s.split_once(';').map_or(s, |p| p.0).chars())
            .peekable();

        let parsed = Self::parse(&mut chars);
        assert_eq!(chars.next(), None); // trailing parens?
        Ok(parsed)
    }
}

fn main() -> Result<(), io::Error> {
    println!(
        "{}",
        fs::read_to_string("uniform-ctxts.jidoka")?
            .parse::<Expr>()
            .unwrap()
    );
    Ok(())
}
