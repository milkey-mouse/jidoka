use egg::*;
use fxhash::FxHashSet as HashSet;
use std::{
    env, fs,
    io::{Error, ErrorKind, Result},
    path::Path,
};

define_language! {
    enum Lambda {
        Bool(bool),
        Num(i32),

        "var" = Var(Id),

        "+" = Add([Id; 2]),
        "=" = Eq([Id; 2]),

        "app" = App([Id; 2]),
        "lam" = Lambda([Id; 2]),
        "let" = Let([Id; 3]),
        "fix" = Fix([Id; 2]),
        //"list" = List(Box<[Id]>),
        "cons" = Cons([Id; 2]),

        "if" = If([Id; 3]),

        "samp" = Samp(Id),

        Symbol(egg::Symbol),
    }
}

impl Lambda {
    fn num(&self) -> Option<i32> {
        match self {
            Lambda::Num(n) => Some(*n),
            _ => None,
        }
    }
}

type EGraph = egg::EGraph<Lambda, LambdaAnalysis>;

#[derive(Default)]
struct LambdaAnalysis;

#[derive(Debug)]
struct Data {
    free: HashSet<Id>,
    constant: Option<(Lambda, PatternAst<Lambda>)>,
}

fn eval(egraph: &EGraph, enode: &Lambda) -> Option<(Lambda, PatternAst<Lambda>)> {
    let x = |i: &Id| egraph[*i].data.constant.as_ref().map(|c| &c.0);
    match enode {
        Lambda::Num(n) => Some((enode.clone(), format!("{}", n).parse().unwrap())),
        Lambda::Bool(b) => Some((enode.clone(), format!("{}", b).parse().unwrap())),
        Lambda::Add([a, b]) => Some((
            Lambda::Num(x(a)?.num()? + x(b)?.num()?),
            format!("(+ {} {})", x(a)?, x(b)?).parse().unwrap(),
        )),
        Lambda::Eq([a, b]) => Some((
            Lambda::Bool(x(a)? == x(b)?),
            format!("(= {} {})", x(a)?, x(b)?).parse().unwrap(),
        )),
        _ => None,
    }
}

impl Analysis<Lambda> for LambdaAnalysis {
    type Data = Data;
    fn merge(&mut self, to: &mut Data, from: Data) -> DidMerge {
        let before_len = to.free.len();
        // to.free.extend(from.free);
        to.free.retain(|i| from.free.contains(i));
        if to.constant.is_none() && from.constant.is_some() {
            to.constant = from.constant;
            DidMerge(true, to.free.len() != from.free.len())
        } else {
            DidMerge(before_len != to.free.len(), true)
        }
    }

    fn make(egraph: &EGraph, enode: &Lambda) -> Data {
        let f = |i: &Id| egraph[*i].data.free.iter().cloned();
        let mut free = HashSet::default();
        match enode {
            Lambda::Var(v) => {
                free.insert(*v);
            }
            Lambda::Let([v, a, b]) => {
                free.extend(f(b));
                free.remove(v);
                free.extend(f(a));
            }
            Lambda::Lambda([v, a]) | Lambda::Fix([v, a]) => {
                free.extend(f(a));
                free.remove(v);
            }
            _ => enode.for_each(|c| free.extend(&egraph[c].data.free)),
        }
        let constant = eval(egraph, enode);
        Data { constant, free }
    }

    fn modify(egraph: &mut EGraph, id: Id) {
        if let Some(c) = egraph[id].data.constant.clone() {
            if egraph.are_explanations_enabled() {
                egraph.union_instantiations(
                    &c.0.to_string().parse().unwrap(),
                    &c.1,
                    &Default::default(),
                    "analysis".to_string(),
                );
            } else {
                let const_id = egraph.add(c.0);
                egraph.union(id, const_id);
            }
        }
    }
}

fn var(s: &str) -> Var {
    s.parse().unwrap()
}

fn is_not_same_var(v1: Var, v2: Var) -> impl Fn(&mut EGraph, Id, &Subst) -> bool {
    move |egraph, _, subst| egraph.find(subst[v1]) != egraph.find(subst[v2])
}

fn is_const(v: Var) -> impl Fn(&mut EGraph, Id, &Subst) -> bool {
    move |egraph, _, subst| egraph[subst[v]].data.constant.is_some()
}

fn not_samp(v: Var) -> impl Fn(&mut EGraph, Id, &Subst) -> bool {
    // TODO: this only looks down one level, ideally any expression
    // containing a `samp` would be "tainted" by nondeterminism
    move |egraph, _, subst| {
        egraph[subst[v]]
            .nodes
            .iter()
            .any(|n| !matches!(n, Lambda::Samp(_)))
    }
}

struct CaptureAvoid {
    fresh: Var,
    v2: Var,
    e: Var,
    if_not_free: Pattern<Lambda>,
    if_free: Pattern<Lambda>,
}

impl Applier<Lambda, LambdaAnalysis> for CaptureAvoid {
    fn apply_one(
        &self,
        egraph: &mut EGraph,
        eclass: Id,
        subst: &Subst,
        searcher_ast: Option<&PatternAst<Lambda>>,
        rule_name: Symbol,
    ) -> Vec<Id> {
        let e = subst[self.e];
        let v2 = subst[self.v2];
        let v2_free_in_e = egraph[e].data.free.contains(&v2);
        if v2_free_in_e {
            let mut subst = subst.clone();
            let sym = Lambda::Symbol(format!("_{}", eclass).into());
            subst.insert(self.fresh, egraph.add(sym));
            self.if_free
                .apply_one(egraph, eclass, &subst, searcher_ast, rule_name)
        } else {
            self.if_not_free
                .apply_one(egraph, eclass, &subst, searcher_ast, rule_name)
        }
    }
}

fn prove_hybrids(left: impl AsRef<Path>, right: impl AsRef<Path>) -> Result<()> {
    let rules: &[Rewrite<Lambda, LambdaAnalysis>] = &[
        // open term rules
        rewrite!("if-true";  "(if  true ?then ?else)" => "?then"),
        rewrite!("if-false"; "(if false ?then ?else)" => "?else"),
        rewrite!("if-elim"; "(if (= (var ?x) ?e) ?then ?else)" => "?else"
            if ConditionEqual::parse("(let ?x ?e ?then)", "(let ?x ?e ?else)")),
        rewrite!("add-comm";  "(+ ?a ?b)"        => "(+ ?b ?a)"),
        rewrite!("add-assoc"; "(+ (+ ?a ?b) ?c)" => "(+ ?a (+ ?b ?c))"),
        rewrite!("eq-comm";   "(= ?a ?b)"        => "(= ?b ?a)"),
        // subst rules
        rewrite!("fix";      "(fix ?v ?e)"             => "(let ?v (fix ?v ?e) ?e)"),
        rewrite!("beta";     "(app (lam ?v ?body) ?e)" => "(let ?v ?e ?body)"),
        rewrite!("let-app";  "(let ?v ?e (app  ?a ?b))" => "(app  (let ?v ?e ?a) (let ?v ?e ?b))" if not_samp(var("?e"))),
        rewrite!("let-add";  "(let ?v ?e (+    ?a ?b))" => "(+    (let ?v ?e ?a) (let ?v ?e ?b))" if not_samp(var("?e"))),
        rewrite!("let-eq";   "(let ?v ?e (=    ?a ?b))" => "(=    (let ?v ?e ?a) (let ?v ?e ?b))" if not_samp(var("?e"))),
        rewrite!("let-cons"; "(let ?v ?e (cons ?a ?b))" => "(cons (let ?v ?e ?a) (let ?v ?e ?b))" if not_samp(var("?e"))), // TODO: other cons rules
        rewrite!("let-const"; "(let ?v ?e ?c)" => "?c" if is_const(var("?c"))),
        rewrite!("let-self"; "(let ?v ?e ?v)" => "?e"),
        rewrite!("let-if";
            "(let ?v ?e (if ?cond ?then ?else))" =>
            "(if (let ?v ?e ?cond) (let ?v ?e ?then) (let ?v ?e ?else))"
        ),
        rewrite!("let-var-same"; "(let ?v1 ?e (var ?v1))" => "?e"),
        // TODO: this rule is unsound when inner functions access their
        // outside environment (i.e. dynamic scoping)
        rewrite!("let-var-diff"; "(let ?v1 ?e (var ?v2))" => "(var ?v2)"
            if is_not_same_var(var("?v1"), var("?v2"))),
        rewrite!("let-lam-same"; "(let ?v1 ?e (lam ?v1 ?body))" => "(lam ?v1 ?body)"),
        rewrite!("let-lam-diff";
            "(let ?v1 ?e (lam ?v2 ?body))" =>
            { CaptureAvoid {
                fresh: var("?fresh"), v2: var("?v2"), e: var("?e"),
                if_not_free: "(lam ?v2 (let ?v1 ?e ?body))".parse().unwrap(),
                if_free: "(lam ?fresh (let ?v1 ?e (let ?v2 (var ?fresh) ?body)))".parse().unwrap(),
            }}
            if is_not_same_var(var("?v1"), var("?v2"))),
        //rewrite!("samp"; "(samp ?v)" => "?v"),
        rewrite!("samp-const"; "(samp ?v)" => "?v" if is_const(var("?v"))),
        // security-specific rules
        rewrite!("ots$"; "(samp (app ?m (app (samp (var Σ.KeyGen)) (var Σ.Enc))))" => "(samp (var Σ.C))"),
    ];

    let left_expr = fs::read_to_string(left)?
        .parse()
        .expect("failed to parse left library");
    let right_expr = fs::read_to_string(right)?
        .parse()
        .expect("failed to parse right library");

    let mut runner = Runner::default()
        //.with_time_limit(std::time::Duration::from_secs(20))
        //.with_node_limit(150_000)
        //.with_iter_limit(60000)
        .with_explanations_enabled()
        .with_expr(&left_expr)
        .with_expr(&right_expr)
        .run(rules);

    // This is how assumptions would be added at runtime,
    // e.g. by using `(assert (= ots-real ots-rand))`.
    /*let ots_real_id = runner.egraph.add_expr(&ots_real);
    let ots_rand_id = runner.egraph.add_expr(&ots_rand);
    runner.egraph.union(ots_real_id, ots_rand_id);*/

    runner.print_report();

    if !runner.egraph.equivs(&left_expr, &right_expr).is_empty() {
        println!("Libraries are interchangeable.");

        print!("Library egraph extraction: ");
        print_all(&mut HashSet::default(), &runner.egraph, runner.roots[0]);

        println!(
            "\n\nExplanation: {}",
            runner
                .explain_equivalence(&left_expr, &right_expr)
                .get_flat_string()
        );

        Ok(())
    } else {
        println!("Libraries could not be proven interchangeable!");
        println!("Left library egraph extraction: ");
        print_all(&mut HashSet::default(), &runner.egraph, runner.roots[0]);

        println!("\n\nRight library egraph extraction: ");
        print_all(&mut HashSet::default(), &runner.egraph, runner.roots[1]);
        println!("\n");

        Err(Error::new(
            ErrorKind::Other,
            "Libraries are not interchangeable",
        ))
    }
}

fn print_all<'a>(parents: &mut HashSet<&'a Lambda>, egraph: &'a EGraph, id: Id) {
    if egraph[id].nodes.len() != 1 {
        print!("{{");
    }
    for node in &egraph[id].nodes {
        if !parents.insert(node) {
            print!("<{}>", id);
            continue;
        }
        print!("{}:", id);
        match node {
            Lambda::Bool(b) => print!("{}", b),
            Lambda::Num(n) => print!("{}", n),

            Lambda::Var(v) => {
                print!("(var ");
                print_all(parents, egraph, *v);
                print!(")");
            }

            Lambda::Samp(s) => {
                print!("(samp ");
                print_all(parents, egraph, *s);
                print!(")");
            }

            Lambda::Add([a, b]) => {
                print!("(+ ");
                print_all(parents, egraph, *a);
                print!(" ");
                print_all(parents, egraph, *b);
                print!(")");
            }

            Lambda::Eq([a, b]) => {
                print!("(= ");
                print_all(parents, egraph, *a);
                print!(" ");
                print_all(parents, egraph, *b);
                print!(")");
            }

            Lambda::App([a, b]) => {
                print!("(app ");
                print_all(parents, egraph, *a);
                print!(" ");
                print_all(parents, egraph, *b);
                print!(")");
            }

            Lambda::Lambda([a, b]) => {
                print!("(lam ");
                print_all(parents, egraph, *a);
                print!(" ");
                print_all(parents, egraph, *b);
                print!(")");
            }

            Lambda::Let([a, b, c]) => {
                print!("(let ");
                print_all(parents, egraph, *a);
                print!(" ");
                print_all(parents, egraph, *b);
                print!(" ");
                print_all(parents, egraph, *c);
                print!(")");
            }

            Lambda::Fix([a, b]) => {
                print!("(fix ");
                print_all(parents, egraph, *a);
                print!(" ");
                print_all(parents, egraph, *b);
                print!(")");
            }

            Lambda::Cons([a, b]) => {
                print!("(cons ");
                print_all(parents, egraph, *a);
                print!(" ");
                print_all(parents, egraph, *b);
                print!(")");
            }

            Lambda::If([a, b, c]) => {
                print!("(if ");
                print_all(parents, egraph, *a);
                print!(" ");
                print_all(parents, egraph, *b);
                print!(" ");
                print_all(parents, egraph, *c);
                print!(")");
            }

            Lambda::Symbol(s) => print!("{}", s),
        }
        assert!(parents.remove(node));
        print!(",");
    }
    if egraph[id].nodes.len() != 1 {
        print!("}}");
    }
}

fn main() -> Result<()> {
    match &env::args().collect::<Vec<_>>()[..] {
        [_, left, right] => prove_hybrids(left, right),
        _ => Err(Error::new(
            ErrorKind::Other,
            "usage: jidoka <left.jidoka> <right.jidoka>",
        )),
    }
}
