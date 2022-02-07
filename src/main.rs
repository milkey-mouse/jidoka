use std::io::{self};

mod file;
//mod language;
mod sexpr;
mod symbol;
//mod transform;

fn main() -> Result<(), io::Error> {
    /*let files = ["uniform-ctxts.jidoka", "syntax.jidoka"];
    let exprs = files
        .into_iter()
        .flat_map(|f| {
            sexpr::Expr::parse_all(
                // TODO: no unwrap, properly handle I/O error
                BufReader::new(File::open(f).unwrap())
                    .bytes()
                    .map(Result::unwrap),
            )
        })
        .collect::<Vec<_>>();
    for expr in exprs {
        println!("{}", expr);
    }*/

    Ok(())
}
