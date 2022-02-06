use crate::sexpr::Expr;
use once_cell::sync::Lazy;

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
    If(Box<Desugared>, Box<Desugared>, Box<Desugared>), // (if 1 "true" "false")

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

fn desugar(parsed: Parsed) -> Option<Desugared> {
    desugar_iter(parsed, iter::empty())
}

fn desugar_lambda(args: &[egg::Symbol], body: Parsed) -> Option<Desugared> {
    if let [rest @ .., last] = *args.split_last() {
        Desugared::Lambda(Some(last), Box::new(desugar_lambda(rest, body)?))
    } else {
        Desugared::Lambda(None, Box::new(desugar(body)?))
    }
}

/*fn desugar_apply(body: Parsed, args: &[Parsed]) -> Option<Desugared> {
    if let [rest @ .., last] = *args {
        Some(
            Desugared::Apply(
                Box::new( desugar_apply(body, &rest)? ),
                Some( Box::new(desugar(last)?) )
            )
        )
    } else {
        Some(
            Desugared::Apply(
                Box::new( desugar(body)? ),
                None
            )
        )
    }
}*/

fn desugar_iter(parsed: Parsed, rest: &mut impl Iterator<Item = Parsed>) -> Option<Desugared> {
    // TODO: how to keep variable hygiene?
    // what is the lambda-calculus version of Ivory™ soap?
    match parsed {
        // (define var X) (use var) -> ((fun var (use var)) X)
        Parsed::Define(name, val) => Some(Desugared::Apply(
            Box::new(Desugared::Lambda(name, desugar_iter(rest.next()?, rest))),
            // TODO: change order so we don't consume too many Parsed's if desugar(val) returns None
            desugar(val).map(Box::new),
        )),
        // (let var X (use var)) -> ((fun var (use var)) X)
        Parsed::Let(name, val, body) => Some(Desugared::Apply(
            Box::new(Desugared::Lambda(name, desugar(body)?)),
            // TODO: change order so we don't consume too many Parsed's if desugar(val) returns None
            desugar(val).map(Box::new),
        )),
        // (fun (a b c) (use a b c)) -> (fun c (fun b (fun a (fun () (use a b c)))))
        Parsed::Lambda(args, body) => Some(Box::new(desugar_lambda(args, body))),
        // (use a b c) -> ((((use ()) a) b) c)
        Parsed::Apply(body, args) => Some(Box::new(desugar_apply(body, args))),
        Parsed::String(s) => Some(Desugared::String(s)),
        Parsed::Char(c) => Some(Desugared::Char(c)),
        Parsed::Num(n) => Some(Desugared::Num(n)),
    }
}
