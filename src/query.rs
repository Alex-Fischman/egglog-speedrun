//! This module converts syntactic `Pattern`s to semantic `Query`s.

use crate::*;

/// A `Query` is an object that takes the tables in a database and returns
/// all possible variable bindings that satisfy a multi-pattern.
pub struct Query;

enum RowOrVal {
    /// A table name and column index.
    Row(String, usize),
    /// A value.
    Val(Value),
}

impl Query {
    /// Construct a new `Query` from a multi-pattern.
    pub fn new(patterns: &[Pattern]) -> Result<Query, String> {
        // The `vals` are all the things that we'll do equality constraints on.
        // We use the indices into the Vec as a variable name for the constraints.
        let _vals: Vec<RowOrVal> = Vec::new();
        // The `vars` are all the equality constraints between elements of `vals`.
        // The values of the `UnionFind` are the names of the variables.
        let mut vars: UnionFind<HashSet<String>> = UnionFind::new(|mut x: HashSet<String>, y| {
            x.extend(y);
            Ok(x)
        });

        let parse_expr = |_expr: &Expr| todo!();

        for pattern in patterns {
            assert!(!pattern.0.is_empty());

            let a = parse_expr(&pattern.0[0]);
            for expr in &pattern.0[1..] {
                let b = parse_expr(expr);
                vars.union(a, b)?;
            }
        }

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
