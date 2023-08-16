//! This module converts syntactic `Pattern`s to semantic `Query`s.

use crate::*;

/// A `Query` is an object that takes the tables in a database and returns
/// all possible variable bindings that satisfy a multi-pattern.
pub struct Query;

#[derive(Default)]
struct EqClass {
    names: HashSet<String>,
    exprs: Vec<Expr>,
    columns: Vec<(String, usize)>,
}

impl Query {
    /// Construct a new `Query` from a multi-pattern.
    pub fn new(funcs: &HashSet<&String>, patterns: &[Pattern]) -> Result<Query, String> {
        let mut eqs: UnionFind<EqClass> = UnionFind::new(|mut a: EqClass, mut b| {
            Ok(EqClass {
                names: {
                    a.names.extend(b.names);
                    a.names
                },
                exprs: {
                    a.exprs.append(&mut b.exprs);
                    a.exprs
                },
                columns: {
                    a.columns.append(&mut b.columns);
                    a.columns
                },
            })
        });

        let mut rows = Vec::new();

        // add constraints from each pattern individually
        for pattern in patterns {
            assert!(!pattern.0.is_empty());

            let a = eqs_from_expr(&pattern.0[0], funcs, &mut eqs, &mut rows)?;
            for expr in &pattern.0[1..] {
                let b = eqs_from_expr(expr, funcs, &mut eqs, &mut rows)?;
                eqs.union(a, b)?;
            }
        }

        // add constraints across patterns when names are the same
        let mut names_to_keys: HashMap<String, Vec<usize>> = HashMap::new();
        let mut to_union: Vec<(usize, usize)> = Vec::new();
        for (key, value) in eqs.iter() {
            for name in &value.names {
                let vec = names_to_keys.entry(name.clone()).or_default();
                if let Some(old) = vec.last() {
                    to_union.push((*old, key));
                }
                vec.push(key);
            }
        }
        for (a, b) in to_union {
            eqs.union(a, b)?;
        }
        let names_to_keys: HashMap<String, usize> = names_to_keys
            .into_iter()
            .map(|(k, v)| (k, eqs.find(v[0]).0))
            .collect();

        // Dependency constraints, where each key is a canonical key in `eqs`, and
        // the value is the set of canonical keys that any of its `exprs` depend on.
        let mut deps = HashMap::new();
        for (key, class) in eqs.iter() {
            let mut set = HashSet::new();
            for expr in &class.exprs {
                deps_from_expr(expr, &names_to_keys, &mut set);
            }
            deps.insert(key, set);
        }

        for (_, class) in eqs.iter() {
            println!("names: {:?}", class.names);
            println!("exprs: {:?}", class.exprs);
            println!("columns: {:?}", class.columns);
            println!();
        }
        for row in rows {
            println!("row: {row:?}");
        }

        todo!("generate query")
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

fn eqs_from_expr(
    // The expression to transform into an `EqClass`.
    expr: &Expr,
    // The set of functions that exist.
    funcs: &HashSet<&String>,
    // A `UnionFind` to combine the `EqClass`es.
    eqs: &mut UnionFind<EqClass>,
    // A list of `eqs` keys that are all in the same row.
    rows: &mut Vec<(String, Vec<usize>)>,
) -> Result<usize, String> {
    Ok(match expr {
        Expr::Var(var) => eqs.new_key(EqClass {
            names: HashSet::from([var.clone()]),
            ..EqClass::default()
        }),
        Expr::Call(f, xs) if funcs.contains(f) => {
            let mut row = Vec::new();
            for (i, x) in xs.iter().enumerate() {
                let x = eqs_from_expr(x, funcs, eqs, rows)?;
                eqs.merge(
                    x,
                    EqClass {
                        columns: vec![(f.clone(), i)],
                        ..EqClass::default()
                    },
                )?;
                row.push(x);
            }
            let y = eqs.new_key(EqClass {
                columns: vec![(f.clone(), xs.len())],
                ..EqClass::default()
            });
            row.push(y);
            rows.push((f.clone(), row));
            y
        }
        _ => eqs.new_key(EqClass {
            exprs: vec![expr.clone()],
            ..EqClass::default()
        }),
    })
}

fn deps_from_expr(
    // The `Expr` to find the dependencies of.
    _expr: &Expr,
    // A map from variable names to canoncial keys.
    _names: &HashMap<String, usize>,
    // The set of dependencies.
    _deps: &mut HashSet<usize>,
) {
    todo!()
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
