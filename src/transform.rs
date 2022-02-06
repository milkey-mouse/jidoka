use once_cell::sync::Lazy;
use crate::sexpr::Expr;

// intermediate form
pub enum Expr {
    Define((egg::Symbol, Expr)),
    Lambda(List())
}

static DEFINE: Lazy<egg::Symbol> = Lazy::new(|| egg::Symbol::from("define"));

fn desugar_define(exprs: impl IntoIterator<Item = Expr>) -> Expr {
    // Desugars (define var X) (use var) into ((fun (var) (use var)) X)
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
    }

    unimplemented!();
}
