//! This module converts syntactic `Pattern`s to semantic `Query`s.

use crate::*;

/// A `Query` is an object that takes the tables in a database and returns
/// all possible variable bindings that satisfy a multi-pattern.
pub struct Query {
    /// A list of `EqClasses`, each with a unique `usize` name.
    classes: HashMap<usize, EqClass>,
    /// A list of row constraints, where each class must come from the same row in the table.
    rows: HashSet<(String, Vec<usize>)>,
    /// The ordering in which to resolve the `Expr`s in `classes`.
    ordering: Vec<usize>,
}

#[derive(Default)]
struct EqClass {
    name: Option<String>,
    exprs: Vec<Expr>,
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
                name: match (a.name, b.name) {
                    (Some(a), Some(b)) if a == b => Some(a),
                    (Some(a), None) | (None, Some(a)) => Some(a),
                    (None, None) => None,
                    (Some(a), Some(b)) => {
                        return Err(format!("{a} and {b} refer to the same value in {slice}"))
                    }
                },
                exprs: {
                    a.exprs.append(&mut b.exprs);
                    a.exprs
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
            if let Some(name) = &class.name {
                let vec = names_to_keys.entry(name.clone()).or_default();
                if let Some(old) = vec.first() {
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
                if f == g && f_cols[..f_y] == g_cols[..g_y] {
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
            // For each bucket, sort its elements (mostly for determinism)
            let mut bucket: Vec<_> = bucket.into_iter().collect();
            bucket.sort_by_key(|i| std::cmp::Reverse(classes[i].exprs.len()));

            ordering.append(&mut bucket);
        }

        Ok(Query {
            classes,
            rows,
            ordering,
        })
    }

    /// Run this `Query` on the tables in the `Database`.
    pub fn run<'a>(&'a self, funcs: &'a HashMap<String, Table>) -> Result<Bindings<'a>, String> {
        // Sort rows by table size
        let mut rows: Vec<_> = self.rows.iter().collect();
        rows.sort_by_key(|(table, _)| funcs[table].len());

        // Compute instructions
        // todo: interleave rows and ordering instructions more efficiently
        let mut instructions = Vec::new();
        // Compute instructions for rows
        for (table, classes) in rows {
            instructions.push(Instruction::Row {
                table: table.clone(),
                classes: classes.clone(),
            });
        }
        // Compute instructions for ordering
        for &class in &self.ordering {
            for expr in &self.classes[&class].exprs {
                instructions.push(Instruction::Expr {
                    expr: expr.clone(),
                    class,
                });
            }
        }

        Ok(Bindings {
            classes: &self.classes,
            funcs,
            instructions,
            trie: Vec::new(),
        })
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
            name: Some(var.clone()),
            ..EqClass::default()
        }),
        Expr::Call(f, xs) if funcs.contains(f) => {
            let mut row = Vec::new();
            for x in xs {
                let x = eqs_from_expr(x, funcs, eqs, rows)?;
                row.push(x);
            }
            let y = eqs.new_key(EqClass::default());
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
    /// See `Query`.
    classes: &'a HashMap<usize, EqClass>,
    /// The current state of the `Table`s in the `Database`.
    funcs: &'a HashMap<String, Table>,
    /// The `Instruction`s to generate each layer in the trie.
    instructions: Vec<Instruction>,
    /// A lazy trie over the bindings.
    trie: Vec<std::iter::Peekable<Box<dyn Iterator<Item = HashMap<usize, Value>> + 'a>>>,
}

/// An instruction to generate one layer of the trie.
#[derive(Clone)]
enum Instruction {
    /// Iterate over all rows in a table
    Row {
        /// The name of the table to iterate over
        table: String,
        /// The classes to map the values in the columns into
        classes: Vec<usize>,
    },
    /// Compute an expression
    Expr {
        /// The expression to evaluate
        expr: Expr,
        /// The class to check the expression against
        class: usize,
    },
}

impl<'a> Bindings<'a> {
    /// Convert the keys of a map from classes to names.
    fn values_to_vars(&self, values: &HashMap<usize, Value>) -> HashMap<&'a str, Value> {
        values
            .iter()
            .filter_map(|(class, value)| {
                self.classes[class]
                    .name
                    .as_ref()
                    .map(|name| (name.as_str(), *value))
            })
            .collect()
    }

    /// Advance the lazy trie up to `height` to the next value.
    fn advance(&mut self, height: usize) -> Result<Option<HashMap<usize, Value>>, String> {
        // Advance the specified iterator to the next value.
        if self.trie[height].next().is_some() {
            // If we have a value, then great! Build the values map and return it.
            let mut values: HashMap<usize, Value> = HashMap::new();
            for iter in &mut self.trie[..=height] {
                for (&class, &value) in iter.peek().unwrap() {
                    values.insert(class, value);
                }
            }
            Ok(Some(values))
        } else if height == 0 {
            // If we don't have a value and we're at the bottom of the trie, we're done.
            Ok(None)
        } else if let Some(values) = self.advance(height - 1)? {
            // If we don't have a value and we're not at the bottom of the trie, advance
            // the previous height and then rebuild this height using the new `values` map.
            self.trie[height] = self.instructions[height]
                .clone()
                .to_iter(self, values)?
                .peekable();
            // Now try to advance the rebuilt layer.
            self.advance(height)
        } else {
            // If the recursive call said that we're done, then we're done.
            Ok(None)
        }
    }
}

impl Instruction {
    #[allow(clippy::wrong_self_convention)]
    fn to_iter<'a>(
        self,
        bindings: &Bindings<'a>,
        values: HashMap<usize, Value>,
    ) -> Result<Box<dyn Iterator<Item = HashMap<usize, Value>> + 'a>, String> {
        let iter: Box<dyn Iterator<Item = _>> = match self {
            Instruction::Row { table, classes } => {
                let known_column = classes
                    .iter()
                    .enumerate()
                    .find_map(|(i, c)| values.get(c).map(|v| (i, v)));
                match known_column {
                    None => Box::new(bindings.funcs[&table].rows().map(move |row| {
                        classes.iter().copied().zip(row.iter().copied()).collect()
                    })),
                    Some((column, value)) => Box::new(
                        bindings.funcs[&table]
                            .rows_with_value_in_column(*value, column)?
                            .map(move |row| {
                                classes.iter().copied().zip(row.iter().copied()).collect()
                            }),
                    ),
                }
            }
            Instruction::Expr { expr, class } => {
                let value = expr.evaluate(&bindings.values_to_vars(&values), bindings.funcs)?;
                Box::new(std::iter::once(HashMap::from([(class, value)])))
            }
        };
        Ok(Box::new(iter.filter(move |map: &HashMap<usize, Value>| {
            map.iter()
                .all(|(class, value)| !matches!(values.get(class), Some(v) if *v != *value))
        })))
    }
}

impl<'a> Iterator for Bindings<'a> {
    type Item = Result<HashMap<&'a str, Value>, String>;
    fn next(&mut self) -> Option<Result<HashMap<&'a str, Value>, String>> {
        if self.trie.is_empty() {
            todo!("build the initial tree and return")
        } else {
            match self.advance(self.instructions.len() - 1) {
                Ok(Some(values)) => Some(Ok(self.values_to_vars(&values))),
                Ok(None) => None,
                Err(e) => Some(Err(e)),
            }
        }
    }
}
