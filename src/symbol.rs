// TODO: copy Symbol so this can be its own lib not dependent on egg
pub use egg::Symbol;
use once_cell::sync::Lazy;

pub static QUOTE: Lazy<Symbol> = Lazy::new(|| Symbol::from("quote"));
pub static QUASIQUOTE: Lazy<Symbol> = Lazy::new(|| Symbol::from("quasiquote"));
pub static UNQUOTE: Lazy<Symbol> = Lazy::new(|| Symbol::from("unquote"));
pub static UNQUOTE_SPLICING: Lazy<Symbol> = Lazy::new(|| Symbol::from("unquote-splicing"));
pub static SYNTAX: Lazy<Symbol> = Lazy::new(|| Symbol::from("syntax"));
pub static QUASISYNTAX: Lazy<Symbol> = Lazy::new(|| Symbol::from("quasisyntax"));
pub static UNSYNTAX: Lazy<Symbol> = Lazy::new(|| Symbol::from("unsyntax"));
pub static UNSYNTAX_SPLICING: Lazy<Symbol> = Lazy::new(|| Symbol::from("unsyntax-splicing"));

pub static DEFINE: Lazy<Symbol> = Lazy::new(|| Symbol::from("define"));
pub static LET: Lazy<Symbol> = Lazy::new(|| Symbol::from("let"));
pub static FUN: Lazy<Symbol> = Lazy::new(|| Symbol::from("fun"));
pub static IF: Lazy<Symbol> = Lazy::new(|| Symbol::from("if"));
