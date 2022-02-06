use crate::sexpr;
use egg::{define_language, Id, Symbol};
use rug::Integer;

/*define_language! {
    // TODO: more creative name for language
    pub enum Language {
        Bool(bool),
        Num(Integer),

        "fun" = Fun(Id, Id),
        Symbol(Symbol),
        Apply(Symbol, Box<[Id]>),
    }
}*/

/*pub enum Expr {
    List(Box<[Expr]>),   // (...)
    Symbol(egg::Symbol), // hello
    // TODO: should String also use egg::Symbol?
    // will it work with string concatenations?
    // will we even need to do string concatenations?
    String(Box<str>), // "hello"
    Char(char),       // '0'
    Num(Integer),     // true
    Empty, // empty expression, only used internally
           // (TODO: should this just be Option<Expr>?)
}*/

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
