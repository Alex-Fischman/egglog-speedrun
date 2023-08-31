//! This module converts syntactic `Pattern`s to semantic `Query`s.

use crate::*;

/// A `Query` is an object that takes the tables in a database and returns
/// all possible variable bindings that satisfy a multi-pattern.
pub struct Query<'a> {
    /// The slice that generated this query.
    slice: Slice<'a>,
    /// A list of `EqClasses`, each with a unique `usize` name.
    classes: HashMap<usize, EqClass>,
    /// A dependency map used to generate an ordering. The keys are (class, expr) pairs,
    /// and the values are sets of class indices that need to be computed before the expr.
    expr_deps: HashMap<(usize, usize), HashSet<usize>>,
    /// A dependency map used to generate an ordering. The keys are (class, call) pairs,
    /// and the values are sets of class indices that should be computed before the call.
    call_deps: HashMap<(usize, usize), HashSet<usize>>,
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

        // Add equality constraints from each pattern individually.
        for mut pattern in patterns {
            assert!(!pattern.0.is_empty());
            let a = eqs_from_expr(pattern.0.remove(0), funcs, &mut eqs)?;
            for expr in pattern.0 {
                let b = eqs_from_expr(expr, funcs, &mut eqs)?;
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

        // Change keys to canonical keys.
        let canonicalize: HashMap<usize, usize> = eqs.keys().map(|k| (k, eqs.find(k))).collect();
        // We're about to do stuff with canonical keys, so don't touch `eqs` anymore.
        let mut classes: HashMap<usize, EqClass> = eqs.into_iter().collect();

        let names_to_keys: HashMap<String, usize> = names_to_keys
            .into_iter()
            .map(|(k, v)| (k, canonicalize[&v[0]]))
            .collect();

        for v in classes.values_mut() {
            v.calls
                .iter_mut()
                .for_each(|(_, xs)| xs.iter_mut().for_each(|v| *v = canonicalize[v]));
        }

        // Compute dependency constraints which give an ordering for `Expr` computation.
        let mut expr_deps: HashMap<(usize, usize), HashSet<usize>> = HashMap::new();
        for (key, class) in &classes {
            for (i, expr) in class.exprs.iter().enumerate() {
                let mut set = HashSet::new();
                deps_from_expr(expr, &names_to_keys, &mut set);
                assert!(expr_deps.insert((*key, i), set).is_none());
            }
        }

        // Compute dependency constraints which give an ordering for call computation.
        let mut call_deps: HashMap<(usize, usize), HashSet<usize>> = HashMap::new();
        for (key, class) in &classes {
            for (i, call) in class.calls.iter().enumerate() {
                let set: HashSet<usize> = call.1.iter().copied().collect();
                assert!(call_deps.insert((*key, i), set).is_none());
            }
        }

        Ok(Query {
            slice,
            classes,
            expr_deps,
            call_deps,
        })
    }

    /// Run this `Query` on the tables in the `Database`.
    pub fn run<'a, 'b>(&'a self, funcs: &'b Funcs) -> Result<Bindings<'a, 'b>, String> {
        // The ordering of instructions to build the trie.
        let mut instructions = Vec::new();
        // The classes that the current value of `instructions` computes.
        let mut known: HashSet<usize> = HashSet::new();
        // `Expr` indices that haven't been calculated yet.
        let mut exprs: HashSet<(usize, usize)> = self.expr_deps.keys().copied().collect();
        // Call indices that haven't been calculated yet.
        let mut calls: HashSet<(usize, usize)> = self.call_deps.keys().copied().collect();

        while !exprs.is_empty() || !calls.is_empty() {
            // Get all indices that have known dependencies.
            let mut calculable_exprs: Vec<_> = exprs
                .iter()
                .copied()
                .filter(|expr| self.expr_deps[expr].is_subset(&known))
                .collect();
            let mut calculable_calls: Vec<_> = calls
                .iter()
                .copied()
                .filter(|call| self.call_deps[call].is_subset(&known))
                .collect();

            // We should do some work every iteration.
            if calculable_exprs.is_empty() && calculable_calls.is_empty() {
                // We can still evaluate calls without all of their arguments using enumeration.
                if let Some(&call) = calls.iter().max_by_key(|&(i, j)| {
                    let (f, xs) = &self.classes[i].calls[*j];
                    (
                        std::cmp::Reverse(xs.iter().filter(|c| known.contains(c)).count()),
                        funcs[f].len(),
                    )
                }) {
                    calculable_calls.push(call);
                } else {
                    // If nothing was calculable, there's a cycle.
                    return Err(format!("dependency cycle in {}", self.slice));
                }
            }

            // We're going to calculate these now, so take them off of the list.
            for expr in &calculable_exprs {
                assert!(exprs.remove(expr));
            }
            for call in &calculable_calls {
                assert!(calls.remove(call));
            }

            // Once we add these instructions these classes will all be known.
            known.extend(calculable_exprs.iter().map(|(i, _)| i));
            for &(i, j) in &calculable_calls {
                known.insert(i);
                known.extend(&self.call_deps[&(i, j)]);
            }

            // Determinism
            calculable_exprs.sort_unstable();
            calculable_calls.sort_unstable();

            // Add instructions.
            instructions.extend(calculable_exprs.iter().map(|&(i, j)| Instruction::Expr {
                expr: &self.classes[&i].exprs[j],
                class: i,
            }));
            instructions.extend(calculable_calls.iter().map(|&(i, j)| {
                let (table, mut classes) = self.classes[&i].calls[j].clone();
                classes.push(i);
                Instruction::Row { table, classes }
            }));
        }

        Ok(Bindings {
            names: self
                .classes
                .iter()
                .filter_map(|(k, v)| v.name.as_ref().map(|n| (*k, n.as_str())))
                .collect(),
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
) -> Result<usize, String> {
    Ok(match expr {
        Expr::Var(var) => eqs.new_key(EqClass {
            name: Some(var),
            ..EqClass::default()
        }),
        Expr::Call(f, xs) if funcs.contains(&f) => {
            let xs = xs
                .into_iter()
                .map(|x| eqs_from_expr(x, funcs, eqs))
                .collect::<Result<Vec<_>, _>>()?;
            eqs.new_key(EqClass {
                calls: vec![(f, xs)],
                ..EqClass::default()
            })
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
        Expr::Var(var) => {
            deps.insert(names[var]);
        }
        Expr::Call(_, xs) => xs.iter().for_each(|x| deps_from_expr(x, names, deps)),
        _ => {}
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
            .filter_map(|(class, value)| self.names.get(class).map(|name| (*name, value.clone())))
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
            for (&class, value) in result.as_ref()? {
                values.insert(class, value.clone());
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
                .push((Box::new(empty()) as Box<dyn Iterator<Item = _>>).peekable()),
            // this should never happen
            std::cmp::Ordering::Greater => panic!(),
        }

        self.trie[height] = match &self.instructions[height] {
            Instruction::Expr { expr, class } => {
                let x = expr.evaluate_ref(&self.values_to_vars(&values), self.funcs);
                match (x, values.get(class)) {
                    (Err(e), _) => Box::new(once(Err(e))) as Box<dyn Iterator<Item = _>>,
                    (Ok(None), _) => Box::new(empty()),
                    (Ok(Some(value)), Some(v)) if *v != value => Box::new(empty()),
                    (Ok(Some(value)), _) => Box::new(once(Ok(HashMap::from([(*class, value)])))),
                }
            }
            Instruction::Row { table, classes } => {
                let xs: Vec<Value> = classes[..classes.len() - 1]
                    .iter()
                    .filter_map(|c| values.get(c).cloned())
                    .collect();

                let rows: Box<dyn Iterator<Item = _>> = if xs.len() + 1 == classes.len() {
                    // We can use the function index.
                    Box::new(self.funcs[table].rows_with_inputs(&xs))
                } else if let Some((col, val)) = classes
                    .iter()
                    .enumerate()
                    .find_map(|(i, c)| values.get(c).map(|v| (i, v)))
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
                            .iter()
                            .zip(row)
                            .map(|(a, b)| (*a, b.clone()))
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
                                    values.insert(*class, value.clone());
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
