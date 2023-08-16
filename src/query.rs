//! This module converts syntactic `Pattern`s to semantic `Query`s.

use crate::*;

/// A `Query` is an object that takes the tables in a database and returns
/// all possible variable bindings that satisfy a multi-pattern.
pub struct Query;

#[derive(Debug)]
enum InPattern {
    /// A value.
    Val(Value),
    /// A table name and UnionFind keys.
    Out(String, Vec<usize>),
    /// A variable name.
    Var(String),
    /// A table name and column index.
    Col(String, usize),
}

impl Query {
    /// Construct a new `Query` from a multi-pattern.
    pub fn new(patterns: &[Pattern]) -> Result<Query, String> {
        fn eq_from_expr(
            expr: &Expr,
            names: &mut HashMap<usize, InPattern>,
            eqs: &mut UnionFind<()>,
        ) -> Result<usize, String> {
            let key = eqs.new_key(());
            match expr {
                Expr::Unit => {
                    names.insert(key, InPattern::Val(Value::Unit));
                }
                Expr::Int(i) => {
                    names.insert(key, InPattern::Val(Value::Int(*i)));
                }
                Expr::Var(v) => {
                    names.insert(key, InPattern::Var(v.clone()));
                }
                Expr::Call(f, xs) => {
                    let xs = xs
                        .iter()
                        .enumerate()
                        .map(|(i, x)| {
                            let a = eq_from_expr(x, names, eqs)?;
                            let b = eqs.new_key(());
                            names.insert(b, InPattern::Col(f.clone(), i));
                            eqs.union(a, b)?;
                            Ok::<usize, String>(b)
                        })
                        .collect::<Result<_, _>>()?;
                    names.insert(key, InPattern::Out(f.clone(), xs));
                }
            }
            Ok(key)
        }

        // The values of the `UnionFind` are the names of the variables.
        let mut eqs = UnionFind::default();
        // Map from the keys of the UnionFind to the expressions they came from.
        let mut names: HashMap<usize, InPattern> = HashMap::new();

        for pattern in patterns {
            assert!(!pattern.0.is_empty());

            let a = eq_from_expr(&pattern.0[0], &mut names, &mut eqs)?;
            for expr in &pattern.0[1..] {
                let b = eq_from_expr(expr, &mut names, &mut eqs)?;
                eqs.union(a, b)?;
            }
        }

        let partition: Vec<Vec<_>> = eqs
            .partition()
            .values()
            .map(|v| v.into_iter().map(|x| names.remove(x)).collect())
            .collect();

        println!("{partition:?}");

        todo!()
    }

    /// Run this `Query` on the tables in the `Database`.
    pub fn run(
        &self,
        _funcs: &HashMap<String, Table>,
    ) -> impl Iterator<Item = HashMap<&str, Value>> {
        // // per pattern, per row, list of assignments
        // let binding: Vec<Vec<Vec<(&str, Value)>>> = patterns
        //     .iter()
        //     .map(|pattern| {
        //         self.funcs[&pattern.f]
        //             .rows()
        //             .map(|(xs, _)| {
        //                 pattern
        //                     .xs
        //                     .iter()
        //                     .zip(xs)
        //                     .map(|(a, b)| (a.as_str(), *b))
        //                     .collect()
        //             })
        //             .collect()
        //     })
        //     .collect();
        // // per rows, per pattern, assignments
        // let bindings = multi_cartesian_product(binding);
        // // flatten, filter out non-matching assignments, convert to hashmaps
        // let bindings = bindings.into_iter().filter_map(|binding| {
        //     let mut out: HashMap<&str, Value> = HashMap::new();
        //     for assignments in binding {
        //         for (key, value) in assignments {
        //             if let Some(v) = out.get(key) {
        //                 if *v != value {
        //                     return None;
        //                 }
        //             } else {
        //                 out.insert(key, value);
        //             }
        //         }
        //     }
        //     Some(out)
        // });

        todo!("run query");
        [].into_iter()
    }
}

// fn multi_cartesian_product<T: Clone>(vecs: Vec<Vec<T>>) -> Vec<Vec<T>> {
//     vecs.into_iter().fold(vec![vec![]], |xss, ys| {
//         let mut out = Vec::new();
//         for xs in xss {
//             for y in &ys {
//                 let mut xs = xs.clone();
//                 xs.push(y.clone());
//                 out.push(xs);
//             }
//         }
//         out
//     })
// }
