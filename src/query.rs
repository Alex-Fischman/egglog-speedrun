//! This module converts syntactic `Pattern`s to semantic `Query`s.

use crate::*;

/// A `Query` is an object that takes the tables in a database and returns
/// all possible variable bindings that satisfy a multi-pattern.
pub struct Query;

#[derive(Debug, Default)]
struct EqClass {
    name: Option<String>,
    value: Option<Value>,
    columns: Vec<(String, usize)>,
}

impl Query {
    /// Construct a new `Query` from a multi-pattern.
    pub fn new(slice: &Slice, patterns: &[Pattern]) -> Result<Query, String> {
        fn eq_from_expr(expr: &Expr, eqs: &mut UnionFind<EqClass>) -> Result<usize, String> {
            Ok(match expr {
                Expr::Unit => eqs.new_key(EqClass {
                    value: Some(Value::Unit),
                    ..EqClass::default()
                }),
                Expr::Int(i) => eqs.new_key(EqClass {
                    value: Some(Value::Int(*i)),
                    ..EqClass::default()
                }),
                Expr::Var(v) => eqs.new_key(EqClass {
                    name: Some(v.clone()),
                    ..EqClass::default()
                }),
                Expr::Call(f, xs) => {
                    // todo: need some way to constrain that all of
                    //       these keys are in the same row
                    for (i, x) in xs.iter().enumerate() {
                        let a = eq_from_expr(x, eqs)?;
                        let b = eqs.new_key(EqClass {
                            columns: vec![(f.clone(), i)],
                            ..EqClass::default()
                        });
                        eqs.union(a, b)?;
                    }
                    eqs.new_key(EqClass {
                        columns: vec![(f.clone(), xs.len())],
                        ..EqClass::default()
                    })
                }
            })
        }

        let mut eqs: UnionFind<EqClass> = UnionFind::new(|mut a: EqClass, mut b| {
            Ok(EqClass {
                name: match (a.name, b.name) {
                    (Some(a), None) | (None, Some(a)) => Some(a),
                    (None, None) => None,
                    (Some(a), Some(b)) if a == b => Some(a),
                    (Some(a), Some(b)) => {
                        return Err(format!("{a} and {b} refer to the same value in {slice}"))
                    }
                },
                value: match (a.value, b.value) {
                    (Some(a), None) | (None, Some(a)) => Some(a),
                    (None, None) => None,
                    (Some(a), Some(b)) if a == b => Some(a),
                    (Some(a), Some(b)) => {
                        return Err(format!("{a} and {b} are never equal in {slice}"))
                    }
                },
                columns: {
                    a.columns.append(&mut b.columns);
                    a.columns
                },
            })
        });

        // add constraints from each pattern individually
        for pattern in patterns {
            assert!(!pattern.0.is_empty());

            let a = eq_from_expr(&pattern.0[0], &mut eqs)?;
            for expr in &pattern.0[1..] {
                let b = eq_from_expr(expr, &mut eqs)?;
                eqs.union(a, b)?;
            }
        }

        // add constraints across patterns when names are the same
        let mut names_to_keys: HashMap<String, usize> = HashMap::new();
        let mut to_union = Vec::new();
        for (key, value) in eqs.iter() {
            if let Some(name) = &value.name {
                names_to_keys
                    .entry(name.clone())
                    .and_modify(|old| {
                        to_union.push((*old, key));
                        *old = key;
                    })
                    .or_insert(key);
            }
        }
        for (a, b) in to_union {
            eqs.union(a, b)?;
        }

        let classes: Vec<_> = eqs.iter().map(|t| t.1).collect();
        println!("{classes:#?}");

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
