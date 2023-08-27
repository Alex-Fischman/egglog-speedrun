//! This module converts syntactic `Pattern`s to semantic `Query`s.

use crate::*;

/// A `Query` is an object that takes the tables in a database and returns
/// all possible variable bindings that satisfy a multi-pattern.
pub struct Query<'a> {
    /// The slice that generated this query.
    slice: Slice<'a>,
    /// A list of `EqClasses`, each with a unique `usize` name.
    classes: HashMap<usize, EqClass>,
    /// A list of row constraints, where each class must come from the same row in the table.
    rows: HashSet<(String, Vec<usize>)>,
    /// The dependency map used to generate an ordering. The keys are (class, expr) pairs,
    /// and the values are sets of class indices that need to be computed before the expr.
    dependencies: HashMap<(usize, usize), HashSet<usize>>,
}

#[derive(Default)]
struct EqClass {
    name: Option<String>,
    exprs: Vec<Expr>,
    calls: Vec<(String, Vec<usize>)>,
}

impl Query<'_> {
    /// Construct a new `Query` from a multi-pattern.
    pub fn new<'a>(
        slice: Slice<'a>,
        funcs: &HashSet<&String>,
        patterns: Vec<Pattern>,
    ) -> Result<Query<'a>, String> {
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
                calls: {
                    a.calls.append(&mut b.calls);
                    a.calls
                },
            })
        });

        let mut rows = Vec::new();

        // Add equality constraints from each pattern individually.
        for mut pattern in patterns {
            assert!(!pattern.0.is_empty());

            let a = eqs_from_expr(pattern.0.remove(0), funcs, &mut eqs, &mut rows)?;
            for expr in pattern.0 {
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
        let classes: HashMap<usize, EqClass> = eqs.into_iter().collect();

        // Compute dependency constraints, which give an ordering for `Expr` computation.
        let mut dependencies: HashMap<(usize, usize), HashSet<usize>> = HashMap::new();
        for (key, class) in &classes {
            for (i, expr) in class.exprs.iter().enumerate() {
                let mut set = HashSet::new();
                deps_from_expr(expr, &names_to_keys, &mut set);
                assert!(dependencies.insert((*key, i), set).is_none());
            }
        }

        Ok(Query {
            slice,
            classes,
            rows,
            dependencies,
        })
    }

    /// Run this `Query` on the tables in the `Database`.
    pub fn run<'a, 'b>(&'a self, funcs: &'b Funcs) -> Result<Bindings<'a, 'b>, String> {
        let names: HashMap<usize, &str> = self
            .classes
            .iter()
            .filter_map(|(k, v)| v.name.as_ref().map(|n| (*k, n.as_str())))
            .collect();

        // The ordering of instructions to build the trie.
        let mut instructions = Vec::new();
        // The classes that the current value of `instructions` computes.
        let mut known: HashSet<usize> = HashSet::new();
        // `Expr` indices that haven't been calculated yet.
        let mut exprs: HashSet<(usize, usize)> = self.dependencies.keys().copied().collect();
        // Call indices that haven't been calculated yet.
        let mut calls: HashSet<(usize, usize)> = self
            .classes
            .iter()
            .flat_map(|(k, v)| (0..v.calls.len()).map(|i| (*k, i)))
            .collect();
        // Row constraints that haven't been calculated yet.
        let mut rows: Vec<_> = self.rows.iter().cloned().collect();
        rows.sort_unstable(); // determinism
        rows.sort_by_key(|(table, _)| std::cmp::Reverse(funcs[table].len())); // efficiency

        while known.len() < self.classes.len() {
            // Get all expr indices that have known dependencies.
            let mut calculable_exprs: Vec<_> = exprs
                .iter()
                .copied()
                .filter(|expr| self.dependencies[expr].is_subset(&known))
                .collect();
            for expr in &calculable_exprs {
                assert!(exprs.remove(expr));
            }
            calculable_exprs.sort_unstable(); // determinism
            calculable_exprs.sort_by_key(|(i, _)| !known.contains(i)); // efficiency

            // Add `Expr` instructions.
            known.extend(calculable_exprs.iter().map(|(class, _)| class));
            instructions.extend(
                calculable_exprs
                    .iter()
                    .map(|&(class, j)| Instruction::Expr {
                        expr: &self.classes[&class].exprs[j],
                        class,
                    }),
            );

            // Do the same for calls.
            let mut calculable_calls: Vec<_> = calls
                .iter()
                .copied()
                .filter(|(class, call)| {
                    self.classes[class].calls[*call]
                        .1
                        .iter()
                        .copied()
                        .collect::<HashSet<_>>()
                        .is_subset(&known)
                })
                .collect();
            for call in &calculable_calls {
                assert!(calls.remove(call));
            }
            calculable_calls.sort_unstable(); // determinism
            calculable_calls.sort_by_key(|(i, _)| !known.contains(i)); // efficiency

            known.extend(calculable_calls.iter().map(|(class, _)| class));
            instructions.extend(calculable_calls.iter().map(|&(class, j)| {
                let (table, mut classes) = self.classes[&class].calls[j].clone();
                classes.push(class);
                Instruction::Row { table, classes }
            }));

            if calculable_exprs.is_empty() && calculable_calls.is_empty() {
                if let Some((table, classes)) = rows.pop() {
                    // Add a `Row` instruction.
                    known.extend(classes.iter());
                    instructions.push(Instruction::Row { table, classes });
                } else {
                    // If there aren't any rows and nothing was calculable, there's a cycle.
                    return Err(format!("dependency cycle in {}", self.slice));
                }
            }
        }

        Ok(Bindings {
            names,
            funcs,
            instructions,
            trie: Vec::new(),
        })
    }
}

fn eqs_from_expr(
    // The expression to transform into an `EqClass`.
    expr: Expr,
    // The set of functions that exist.
    funcs: &HashSet<&String>,
    // A `UnionFind` to combine the `EqClass`es.
    eqs: &mut UnionFind<EqClass>,
    // A list of `eqs` keys that are all in the same row.
    rows: &mut Vec<(String, Vec<usize>)>,
) -> Result<usize, String> {
    Ok(match expr {
        Expr::Var(var) => eqs.new_key(EqClass {
            name: Some(var),
            ..EqClass::default()
        }),
        Expr::Call(f, xs) if funcs.contains(&f) => {
            let mut row = Vec::new();
            for x in xs {
                let x = eqs_from_expr(x, funcs, eqs, rows)?;
                row.push(x);
            }
            let y = eqs.new_key(EqClass {
                calls: vec![(f.clone(), row.clone())],
                ..EqClass::default()
            });
            row.push(y);
            rows.push((f, row));
            y
        }
        _ => eqs.new_key(EqClass {
            exprs: vec![expr],
            ..EqClass::default()
        }),
    })
}

fn deps_from_expr(
    // The `Expr` to find the dependencies of.
    expr: &Expr,
    // A map from variable names to canonical keys.
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
// There are two lifetimes because `'a` comes from the `Query` but `'b` comes from the `Database`.
pub struct Bindings<'a, 'b> {
    /// See `Query`.
    names: HashMap<usize, &'a str>,
    /// The current state of the `Table`s in the `Database`.
    funcs: &'b Funcs,
    /// The `Instruction`s to generate each layer in the trie.
    instructions: Vec<Instruction<'a>>,
    /// A lazy trie over the bindings.
    trie: Vec<std::iter::Peekable<Box<dyn Iterator<Item = Result<Values, String>> + 'b>>>,
}

type Values = HashMap<usize, Value>;

/// An instruction to generate one layer of the trie.
enum Instruction<'a> {
    /// Compute an expression.
    Expr {
        /// The expression to evaluate
        expr: &'a Expr,
        /// The class to check the expression against
        class: usize,
    },
    /// Iterate over all rows in a table
    Row {
        /// The name of the table to iterate over
        table: String,
        /// The classes to map the values in the columns into
        classes: Vec<usize>,
    },
}

impl<'a, 'b> Bindings<'a, 'b> {
    /// Convert the keys of a map from classes to names.
    fn values_to_vars(&self, values: &Values) -> Vars<'a> {
        values
            .iter()
            .filter_map(|(class, value)| self.names.get(class).map(|name| (*name, *value)))
            .collect()
    }

    /// Get the current values map up to and including `height`.
    fn values(&mut self, height: usize) -> Result<Option<Values>, String> {
        let mut values = if height == 0 {
            HashMap::new()
        } else if let Some(values) = self.values(height - 1)? {
            values
        } else {
            return Ok(None);
        };

        if let Some(result) = self.trie[height].peek() {
            for (&class, &value) in result.as_ref()? {
                values.insert(class, value);
            }
            Ok(Some(values))
        } else if let Some(vs) = self.advance(height)? {
            for (class, value) in vs {
                values.insert(class, value);
            }
            Ok(Some(values))
        } else {
            Ok(None)
        }
    }

    /// Advance the lazy trie up to `height` to the next value.
    fn advance(&mut self, height: usize) -> Result<Option<Values>, String> {
        if self.trie[height].next().is_some() {
            self.values(height)
        } else if height == 0 {
            Ok(None)
        } else if let Some(values) = self.advance(height - 1)? {
            self.build(height, values);
            self.values(height)
        } else {
            Ok(None)
        }
    }

    /// Build a single layer of the trie.
    fn build(&mut self, height: usize, values: Values) {
        match height.cmp(&self.trie.len()) {
            // trie has been built
            std::cmp::Ordering::Less => assert!(self.trie[height].peek().is_none()),
            // trie is being built
            std::cmp::Ordering::Equal => self
                .trie
                .push((Box::new(std::iter::empty()) as Box<dyn Iterator<Item = _>>).peekable()),
            // this should never happen
            std::cmp::Ordering::Greater => panic!(),
        }

        self.trie[height] = match &self.instructions[height] {
            Instruction::Expr { expr, class } => {
                match expr.evaluate_ref(&self.values_to_vars(&values), self.funcs) {
                    Err(e) => Box::new(std::iter::once(Err(e))),
                    Ok(value) => match values.get(class) {
                        Some(v) if *v != value => {
                            Box::new(std::iter::empty()) as Box<dyn Iterator<Item = _>>
                        }
                        _ => Box::new(std::iter::once(Ok(HashMap::from([(*class, value)])))),
                    },
                }
            }
            Instruction::Row { table, classes } => {
                let xs: Vec<Value> = classes[..classes.len() - 1]
                    .iter()
                    .filter_map(|c| values.get(c).copied())
                    .collect();

                let rows: Box<dyn Iterator<Item = _>> = if xs.len() + 1 == classes.len() {
                    // We can use the function index.
                    Box::new(self.funcs[table].rows_with_inputs(&xs))
                } else if let Some((col, val)) = classes
                    .iter()
                    .enumerate()
                    .find_map(|(i, c)| values.get(c).map(|v| (i, *v)))
                {
                    // We can use a column index.
                    Box::new(self.funcs[table].rows_with_value_in_column(val, col))
                } else {
                    // We don't know any columns so we have to enumerate the whole table.
                    Box::new(self.funcs[table].rows())
                };

                let classes = classes.clone();
                Box::new(
                    rows.map(move |row| {
                        Ok(classes
                            .clone()
                            .into_iter()
                            .zip(row.iter().copied())
                            .collect())
                    })
                    .filter_map(move |result: Result<Values, String>| match result {
                        Err(e) => Some(Err(e)),
                        Ok(map) => {
                            let mut values = values.clone();
                            if map.iter().all(|(class, value)| {
                                if let Some(v) = values.get(class) {
                                    v == value
                                } else {
                                    values.insert(*class, *value);
                                    true
                                }
                            }) {
                                Some(Ok(map))
                            } else {
                                None
                            }
                        }
                    }),
                )
            }
        }
        .peekable();
    }
}

impl<'a, 'b> Iterator for Bindings<'a, 'b> {
    type Item = Result<Vars<'a>, String>;
    fn next(&mut self) -> Option<Result<Vars<'a>, String>> {
        // fun with nesting
        match if self.trie.is_empty() {
            for i in 0..self.instructions.len() {
                let values = if i == 0 {
                    HashMap::new()
                } else {
                    match self.values(i - 1) {
                        Ok(Some(values)) => values,
                        Ok(None) => return None,
                        Err(e) => return Some(Err(e)),
                    }
                };
                self.build(i, values);
            }
            self.values(self.instructions.len() - 1)
        } else {
            self.advance(self.instructions.len() - 1)
        } {
            Ok(Some(values)) => Some(Ok(self.values_to_vars(&values))),
            Ok(None) => None,
            Err(e) => Some(Err(e)),
        }
    }
}
