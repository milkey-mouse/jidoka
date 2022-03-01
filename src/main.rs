use std::{
    fs::File,
    io::{self, BufReader, Read},
};

//mod file;
//mod language;
mod sexpr;
mod symbol;
mod utf8;
//mod transform;

fn main() -> Result<(), sexpr::ParseError> {
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
    let mut f = BufReader::new(File::open("uniform-ctxts.jidoka")?)
        .bytes()
        .map(Result::unwrap) // TODO
        .peekable();

    println!("{}", sexpr::Expr::parse(&mut f)?);

    Ok(())
}
