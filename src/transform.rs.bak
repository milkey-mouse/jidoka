use crate::{sexpr::Expr, symbol::Symbol};
use once_cell::sync::Lazy;
use rug::Integer;
use std::iter;

// intermediate form
enum Parsed {
    Define(Symbol, Box<Parsed>),           // (define name value) (use name)
    Let(Symbol, Box<Parsed>, Box<Parsed>), // (let name value (use name))

    Lambda(Box<[Symbol]>, Box<Parsed>), // (fun (a b c) (use a b c))
    Apply(Box<Parsed>, Box<[Parsed]>),  // (use a b c)

    If(Box<Parsed>, Box<Parsed>, Box<Parsed>), // (if 1 "true" "false")

    // TODO: should String also use Symbol?
    // will it work with string concatenations?
    // will we even need to do string concatenations?
    String(Symbol), // "hello"
    Char(char),     // '💩'
    Num(Integer),   // 1234
}

fn parse(expr: Expr) -> Option<Parsed> {
    if let Expr::List(l) = expr {
        Some(match *l {
            [Expr::Symbol(define), Expr::Symbol(name), value] if define == *DEFINE => {
                Parsed::Define(name, Box::new(parse(value)?))
            }
            [Expr::Symbol(_let), Expr::Symbol(name), value, body] if _let == *LET => {
                Parsed::Let(name, Box::new(parse(value)?), Box::new(parse(body)?))
            }
            [Expr::Symbol(fun), Expr::List(args), body @ ..] if fun == *FUN => {
                // TODO: error handling
                let vars = args
                    .map(|arg| {
                        if let Expr::Symbol(s) = arg {
                            Some(s)
                        } else {
                            None
                        }
                    })
                    .collect::<Option<Box<[Symbol]>>>()?;

                Parsed::Lambda(vars, Box::new(parse(body)?))
            }
        })
    } else {
        // TODO: real error
        None
    }
}

enum Desugared {
    // Option<Symbol>::None, Option<Box<Desugared>>::None = ()
    // () is the unit value, for functions which take no argument(s)
    Lambda(Option<Symbol>, Box<Desugared>), // (fun c (fun b (fun a (fun () (use a b c)))))
    Apply(Box<Desugared>, Option<Box<Desugared>>), // ((((use ()) a) b) c)

    // if is strictly speaking unnecessary as it too
    // can be desugared, but I think it's worth keeping
    If(
        Box<Desugared>,
        Option<Box<Desugared>>,
        Option<Box<Desugared>>,
    ), // (if 1 "true" "false")

    String(Symbol), // "hello"
    Char(char),     // '💩'
    Num(Integer),   // 1234
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
