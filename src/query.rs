//! This module converts syntactic `Pattern`s to semantic `Query`s.

use crate::*;

/// A `Query` is an object that takes the tables in a database and returns
/// all possible variable bindings that satisfy a multi-pattern.
pub struct Query {
    /// A list of `EqClasses`, each with a unique `usize` name.
    classes: HashMap<usize, EqClass>,
    /// A list of row constraints, where each class must come from the same row in the table.
    rows: HashSet<(String, Vec<usize>)>,
    /// The ordering in which to resolve `classes`.
    ordering: Vec<usize>,
}

#[derive(Clone, Default)]
struct EqClass {
    names: HashSet<String>,
    exprs: Vec<Expr>,
    columns: Vec<(String, usize)>,
}

impl Query {
    /// Construct a new `Query` from a multi-pattern.
    pub fn new(
        slice: &Slice,
        funcs: &HashSet<&String>,
        patterns: &[Pattern],
    ) -> Result<Query, String> {
        // Equality constraints among different expressions.
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

        // Add equality constraints from each pattern individually.
        for pattern in patterns {
            assert!(!pattern.0.is_empty());

            let a = eqs_from_expr(&pattern.0[0], funcs, &mut eqs, &mut rows)?;
            for expr in &pattern.0[1..] {
                let b = eqs_from_expr(expr, funcs, &mut eqs, &mut rows)?;
                eqs.union(a, b)?;
            }
        }

        // Add equality constraints across patterns when names are the same.
        let mut names_to_keys: HashMap<String, Vec<usize>> = HashMap::new();
        let mut to_union: Vec<(usize, usize)> = Vec::new();
        for (key, class) in eqs.iter() {
            for name in &class.names {
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

        // Do congruence closure; if functions have the same inputs, they have the same output.
        for (i, (f, f_cols)) in rows.iter().enumerate() {
            for (g, g_cols) in &rows[i + 1..] {
                let f_y = f_cols.len() - 1;
                let g_y = g_cols.len() - 1;
                if f == g && f_cols[0..f_y] == g_cols[0..g_y] {
                    eqs.union(f_cols[f_y], g_cols[g_y])?;
                }
            }
        }

        // Change keys to canonical keys.
        let rows: HashSet<(String, Vec<_>)> = rows
            .into_iter()
            .map(|(f, v)| (f, v.into_iter().map(|key| eqs.find(key)).collect()))
            .collect();
        let names_to_keys: HashMap<String, usize> = names_to_keys
            .into_iter()
            .map(|(k, v)| (k, eqs.find(v[0])))
            .collect();

        // We're about to do stuff with canoncial keys, so don't touch `eqs` anymore.
        let classes: HashMap<_, _> = eqs.into_iter().collect();

        // Dependency constraints, where each key is a canonical key in `eqs`, and
        // the value is the set of canonical keys that any of its `exprs` depend on.
        let mut deps: HashMap<usize, HashSet<usize>> = HashMap::new();
        for (key, class) in &classes {
            let mut set = HashSet::new();
            for expr in &class.exprs {
                deps_from_expr(expr, &names_to_keys, &mut set);
            }
            deps.insert(*key, set);
        }

        // Use `deps` to put the keys into buckets, where
        // bucket `i` must be computed before bucket `i+1`.
        let mut ordering: Vec<usize> = Vec::new();
        while !deps.is_empty() {
            // Pull all keys with no dependencies out of `deps` and into `bucket`.
            let bucket: HashSet<_> = deps
                .iter()
                .filter(|t| t.1.is_empty())
                .map(|t| t.0)
                .copied()
                .collect();
            if bucket.is_empty() {
                return Err(format!("dependency cycle in {slice}"));
            }
            deps.retain(|k, _| !bucket.contains(k));
            // The bucket "has been computed", so remove all of its elements from deps.
            for ends in deps.values_mut() {
                *ends = ends.difference(&bucket).copied().collect();
            }
            // For each bucket, sort its elements by:
            //  1) if they have `exprs`, we can compute them directly and filter
            //  2) if they have multiple `columns`, we can use the column indices
            let mut bucket: Vec<_> = bucket.into_iter().collect();
            bucket.sort_by_key(|i| {
                std::cmp::Reverse((classes[i].exprs.len(), classes[i].columns.len()))
            });
            ordering.append(&mut bucket);
        }

        Ok(Query {
            classes,
            rows,
            ordering,
        })
    }

    /// Run this `Query` on the tables in the `Database`.
    #[must_use]
    pub fn run<'a>(&'a mut self, funcs: &'a HashMap<String, Table>) -> Bindings<'a> {
        // Sort the columns in the classes by table size, which we didn't know until now.
        self.classes
            .values_mut()
            .for_each(|class| class.columns.sort_by_key(|(name, _)| funcs[name].len()));
        for class in self.ordering.iter().map(|class| &self.classes[class]) {
            println!("names: {:?}", class.names);
            println!("exprs: {:?}", class.exprs);
            println!("columns: {:?}", class.columns);
            println!();
        }
        for row in &self.rows {
            println!("row constraint: {row:?}");
        }
        println!("ordering: {:?}", self.ordering);

        Bindings { query: self, funcs }
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
    expr: &Expr,
    // A map from variable names to canoncial keys.
    names: &HashMap<String, usize>,
    // The set of dependencies.
    deps: &mut HashSet<usize>,
) {
    match expr {
        Expr::Var(var) if names.contains_key(var) => {
            deps.insert(names[var]);
        }
        Expr::Call(_, xs) => xs.iter().for_each(|x| deps_from_expr(x, names, deps)),
        Expr::Unit | Expr::Int(_) | Expr::Var(_) => {}
    }
}

/// An iterator over all possible variable assignments that match a `Query`.
pub struct Bindings<'a> {
    /// The query that generated this iterator.
    query: &'a Query,
    /// The current state of the `Table`s in the `Database`.
    funcs: &'a HashMap<String, Table>,
}

impl<'a> Iterator for Bindings<'a> {
    type Item = HashMap<&'a str, Value>;
    fn next(&mut self) -> Option<HashMap<&'a str, Value>> {
        let mut vars = HashMap::new();
        'match_failed: for class in &self.query.ordering {
            let class = &self.query.classes[class];
            let mut value = None;
            for expr in &class.exprs {
                match (value, expr.evaluate(&vars, self.funcs).unwrap()) {
                    (None, v) => value = Some(v),
                    (Some(value), v) if value == v => {}
                    (Some(_), _) => break 'match_failed,
                }
            }
            for _column in &class.columns {
                todo!("columns: need to get every possible combination of what goes here somehow")
            }
            for name in &class.names {
                vars.insert(name, value.unwrap());
            }
        }
        Some(vars)
    }
}
