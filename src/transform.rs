use crate::sexpr::Expr;
use once_cell::sync::Lazy;
use rug::Integer;
use std::iter;

static DEFINE: Lazy<egg::Symbol> = Lazy::new(|| egg::Symbol::from("define"));
static LET: Lazy<egg::Symbol> = Lazy::new(|| egg::Symbol::from("let"));
static FUN: Lazy<egg::Symbol> = Lazy::new(|| egg::Symbol::from("fun"));
static IF: Lazy<egg::Symbol> = Lazy::new(|| egg::Symbol::from("if"));

// intermediate form
enum Parsed {
    Define(egg::Symbol, Box<Parsed>), // (define name value) (use name)
    Let(egg::Symbol, Box<Parsed>, Box<Parsed>), // (let name value (use name))

    Lambda(Box<[egg::Symbol]>, Box<Parsed>), // (fun (a b c) (use a b c))
    Apply(Box<Parsed>, Box<[Parsed]>),       // (use a b c)

    If(Box<Parsed>, Box<Parsed>, Box<Parsed>), // (if 1 "true" "false")

    // TODO: should String also use egg::Symbol?
    // will it work with string concatenations?
    // will we even need to do string concatenations?
    String(egg::Symbol), // "hello"
    Char(char),          // '💩'
    Num(Integer),        // 1234
}

enum Desugared {
    // Option<egg::Symbol>::None, Option<Box<Desugared>>::None = ()
    // () is the unit value, for functions which take no argument(s)
    Lambda(Option<egg::Symbol>, Box<Desugared>), // (fun c (fun b (fun a (fun () (use a b c)))))
    Apply(Box<Desugared>, Option<Box<Desugared>>), // ((((use ()) a) b) c)

    // if is strictly speaking unnecessary as it too
    // can be desugared, but I think it's worth keeping
    If(Box<Desugared>, Option<Box<Desugared>>, Option<Box<Desugared>>), // (if 1 "true" "false")

    String(egg::Symbol), // "hello"
    Char(char),          // '💩'
    Num(Integer),        // 1234
}

//fn parse_expr
/*iter::from_fn(|| match exprs.next() {
    Define
})
exprs.next_if()
for expr in exprs {
    if let Expr::List(list) = expr {
        let (define, args) = l.split_first();
        if define == DEFINE {
            if let [Expr::Symbol(name), value] == args {
                Expr::List(Box::new([
                    Expr::List
                ]))
            } else {
                panic!("define has wrong args");
            }
        }
        if let [Expr::Symbol(define), Expr::b, c] = &*l {
            if define == DEFINE {

            }
        }
    }
}*/

// TODO: everywhere possible, do Parsed -> AsRef<Parsed>

fn desugar(parsed: Parsed) -> Option<Desugared> {
    desugar_iter(parsed, &mut iter::empty())
}

// mod splenda; // desugaring
// mod ivory;   // maintaining hygiene

fn desugar_iter(parsed: Parsed, rest: &mut impl Iterator<Item = Parsed>) -> Option<Desugared> {
    // TODO: how to keep variable hygiene?
    // what is the lambda-calculus version of Ivory™ soap?
    Some(match parsed {
        // (define var X) (use var) -> ((fun var (use var)) X)
        Parsed::Define(name, val) => Desugared::Apply(
            Box::new(Desugared::Lambda(
                Some(name),
                Box::new(desugar_iter(rest.next()?, rest)?),
            )),
            // TODO: change order so we don't consume too many Parsed's if desugar(val) returns None
            desugar(*val).map(Box::new),
        ),
        // (let var X (use var)) -> ((fun var (use var)) X)
        Parsed::Let(name, val, body) => Desugared::Apply(
            Box::new(Desugared::Lambda(Some(name), Box::new(desugar(*body)?))),
            // TODO: change order so we don't consume too many Parsed's if desugar(val) returns None
            desugar(*val).map(Box::new),
        ),

        // (fun (a b c) (use a b c)) -> (fun c (fun b (fun a (fun () (use a b c)))))
        // (Vec::from(box) = workaround for https://github.com/rust-lang/rust/issues/59878)
        Parsed::Lambda(args, body) => Vec::from(args).into_iter().fold(
            Desugared::Lambda(None, Box::new(desugar(*body)?)),
            |f, arg| Desugared::Lambda(Some(arg), Box::new(f)),
        ),
        // (use a b c) -> ((((use ()) a) b) c)
        Parsed::Apply(body, args) => Vec::from(args).into_iter().fold(
            Desugared::Apply(Box::new(desugar(*body)?), None),
            // TODO: don't unwrap desugar(arg), throw a real error.
            // if we panic here, it means someone has done e.g.
            // (print (define foo "bar")), where an argument doesn't
            // evaluate to a value. perhaps we should handle this the
            // Rust way and reify the unit type/value  so that any lone,
            // non-value-returning expression actually returns unit ()
            |f, arg| Desugared::Apply(Box::new(f), Some(Box::new(desugar(arg).unwrap()))),
        ),

        Parsed::If(cond, if_t, if_f) => Desugared::If(
            Box::new(desugar(*cond)?),
            desugar(*if_t).map(Box::new),
            desugar(*if_f).map(Box::new),
        ),

        Parsed::String(s) => Desugared::String(s),
        Parsed::Char(c) => Desugared::Char(c),
        Parsed::Num(n) => Desugared::Num(n),
    })
}
