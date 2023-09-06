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

        // Do congruence closure; if two calls are the same, the classes are the same
        let canonicalize: HashMap<_, _> = eqs.keys().map(|k| (k, eqs.find(k))).collect();
        let mut to_union: Vec<(usize, usize)> = Vec::new();
        for (x, a) in eqs.iter() {
            for (y, b) in eqs.iter() {
                if x < y
                    && a.calls.iter().any(|(f, c)| {
                        b.calls.iter().any(|(g, d)| {
                            f == g && {
                                let c: Vec<_> = c.iter().map(|c| canonicalize[c]).collect();
                                let d: Vec<_> = d.iter().map(|d| canonicalize[d]).collect();
                                c == d
                            }
                        })
                    })
                {
                    to_union.push((x, y));
                }
            }
        }
        for (a, b) in to_union {
            eqs.union(a, b)?;
        }

        // We're about to do stuff with canonical keys, so don't touch `eqs` anymore.
        let canonicalize: HashMap<_, _> = eqs.keys().map(|k| (k, eqs.find(k))).collect();
        let mut classes: HashMap<usize, EqClass> = eqs.into_iter().collect();

        // Change keys to canonical keys.
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
        let mut todo = Vec::new();
        todo.extend(self.expr_deps.iter().map(|(&(class, expr), deps)| {
            let expr = &self.classes[&class].exprs[expr];
            Constraint::Expr { expr, class, deps }
        }));
        todo.extend(self.call_deps.iter().map(|(&(y, call), deps)| {
            let (f, xs) = &self.classes[&y].calls[call];
            Constraint::Row { f, xs, y, deps }
        }));

        Ok(Bindings {
            slice: &self.slice,
            names: self
                .classes
                .iter()
                .filter_map(|(k, v)| v.name.as_ref().map(|n| (*k, n.as_str())))
                .collect(),
            funcs,
            todo,
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
    slice: &'a Slice<'a>,
    /// See `Query`.
    names: HashMap<usize, &'a str>,
    /// The current state of the `Table`s in the `Database`.
    funcs: &'b Funcs,
    /// Constraints to complete before we're done with the trie. Not in order.
    todo: Vec<Constraint<'a>>,
    /// A lazy trie over the bindings, represented as an ordered list of iterators.
    /// Each iterator is bundled with the constraint used to generate it, if there is one.
    trie: Vec<Layer<'a, 'b>>,
}

struct Layer<'a, 'b> {
    constraint: Option<Constraint<'a>>,
    iterator: std::iter::Peekable<Box<dyn Iterator<Item = Result<Values, String>> + 'b>>,
}

type Values = HashMap<usize, Value>;

/// A constraint that will be used to generate one layer of the trie.
enum Constraint<'a> {
    /// Compute an expression.
    Expr {
        /// The expression to evaluate
        expr: &'a Expr,
        /// The class to check the expression against
        class: usize,
        /// The set of classes that `expr` refers to
        deps: &'a HashSet<usize>,
    },
    /// Iterate over all rows in a table
    Row {
        /// The name of the table to iterate over
        f: &'a str,
        /// The classes to map the values in the inputs columns into
        xs: &'a [usize],
        /// The classes to map the value in the output column into
        y: usize,
        /// The set of classes that are inputs
        deps: &'a HashSet<usize>,
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

        if let Some(result) = self.trie[height].iterator.peek() {
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
        if self.trie[height].iterator.next().is_some() {
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
        fn rows_to_iter<'a>(
            rows: impl Iterator<Item = &'a [Value]> + 'a,
            xs: &[usize],
            y: usize,
        ) -> Box<dyn Iterator<Item = Result<HashMap<usize, Value>, String>> + 'a> {
            let mut classes = xs.to_vec();
            classes.push(y);
            Box::new(rows.map(move |row| {
                Ok(classes
                    .iter()
                    .zip(row)
                    .map(|(a, b)| (*a, b.clone()))
                    .collect())
            }))
        }

        self.todo
            .extend(self.trie.drain(height..).filter_map(|mut l| {
                assert!(l.iterator.peek().is_none());
                l.constraint
            }));

        assert!(!self.todo.is_empty());
        let known: HashSet<usize> = values.keys().copied().collect();
        // None: todo, Some(None): pass, Some(Some(i)): remove ith constraint
        let mut next: Option<Option<usize>> = None;
        let mut iter: Option<Box<dyn Iterator<Item = _>>> = None;
        // First, try to choose any constraint that can be immediately evaluated.
        for (i, constraint) in self.todo.iter().enumerate() {
            match constraint {
                Constraint::Expr { expr, class, deps } if deps.is_subset(&known) => {
                    let y = expr.evaluate_ref(&self.values_to_vars(&values), self.funcs);
                    next = Some(Some(i));
                    iter = Some(match y {
                        Ok(Some(v)) => Box::new(once(Ok(HashMap::from([(*class, v)])))),
                        Ok(None) => Box::new(empty()),
                        Err(e) => Box::new(once(Err(e))),
                    });
                    break;
                }
                Constraint::Row { f, xs, y, deps } if deps.is_subset(&known) => {
                    let vs: Vec<Value> = xs.iter().map(|class| values[class].clone()).collect();
                    next = Some(Some(i));
                    iter = Some(rows_to_iter(self.funcs[*f].rows_with_inputs(&vs), xs, *y));
                    break;
                }
                _ => {}
            }
        }
        if next.is_none() {
            // Second, try to choose the row constraint with the shortest column index
            let mut shortest_column = usize::MAX;
            for (i, constraint) in self.todo.iter().enumerate() {
                if let Constraint::Row { f, xs, y, .. } = constraint {
                    for (column, class) in xs.iter().chain([y]).enumerate() {
                        if let Some(value) = values.get(class) {
                            let column_len =
                                self.funcs[*f].num_rows_with_value_in_column(value, column);
                            if column_len < shortest_column {
                                shortest_column = column_len;
                                next = Some(Some(i));
                                iter = Some(rows_to_iter(
                                    self.funcs[*f].rows_with_value_in_column(value, column),
                                    xs,
                                    *y,
                                ));
                            }
                        }
                    }
                }
            }
        }
        if next.is_none() {
            // Third, check if any classes appear in more than one row constraint.
            // If any do, intersect the values in those columns without consuming any constraints.
            let mut classes_to_columns: HashMap<usize, Vec<(String, _, _)>> = HashMap::new();
            for constraint in &self.todo {
                if let Constraint::Row { f, xs, y, .. } = constraint {
                    for (column, class) in xs.iter().chain([y]).enumerate() {
                        classes_to_columns.entry(*class).or_default().push((
                            (*f).to_owned(),
                            column,
                            self.funcs[*f].num_values_in_column(column),
                        ));
                    }
                }
            }
            classes_to_columns.retain(|_, columns| columns.len() >= 2);
            classes_to_columns
                .values_mut()
                .for_each(|columns| columns.sort_unstable_by_key(|(_, _, len)| *len));
            // This is the only strategy with any estimation; we don't know how many
            // rows doing the intersection actually eliminates.
            if let Some((class, columns)) = classes_to_columns
                .into_iter()
                .min_by_key(|(_, columns)| columns[0].2)
            {
                let funcs = self.funcs; // don't move self into the closure
                next = Some(None);
                iter = Some(Box::new(
                    funcs[&columns[0].0]
                        .values_in_column(columns[0].1)
                        .filter(move |v| {
                            columns[1..]
                                .iter()
                                .all(|(f, c, _)| funcs[f].is_value_in_column(v, *c))
                        })
                        .map(move |v| Ok(HashMap::from([(class, v.clone())]))),
                ));
            }
        }
        if next.is_none() {
            // Fourth, try to choose the row constraint with the shortest table
            let mut shortest_table = usize::MAX;
            for (i, constraint) in self.todo.iter().enumerate() {
                if let Constraint::Row { f, xs, y, .. } = constraint {
                    let table_len = self.funcs[*f].num_rows();
                    if table_len < shortest_table {
                        shortest_table = table_len;
                        next = Some(Some(i));
                        iter = Some(rows_to_iter(self.funcs[*f].rows(), xs, *y));
                    }
                }
            }
        }
        if next.is_none() {
            // Otherwise, give up since we have a dependency cycle
            assert!(self
                .todo
                .iter()
                .all(|i| matches!(i, Constraint::Expr { .. })));
            next = Some(None);
            iter = Some(Box::new(once(Err(format!(
                "dependency cycle in {}",
                self.slice
            )))));
        }
        let iter: Box<dyn Iterator<Item = _>> = Box::new(iter.unwrap().filter_map(
            move |result: Result<Values, String>| match result {
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
            },
        ));

        self.trie.push(Layer {
            constraint: next.unwrap().map(|i| self.todo.remove(i)),
            iterator: iter.peekable(),
        });
    }
}

impl<'a, 'b> Iterator for Bindings<'a, 'b> {
    type Item = Result<Vars<'a>, String>;
    fn next(&mut self) -> Option<Result<Vars<'a>, String>> {
        // fun with nesting
        match if self.trie.is_empty() {
            let mut i = 0;
            while !self.todo.is_empty() {
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
                i += 1;
            }
            self.values(self.trie.len() - 1)
        } else {
            self.advance(self.trie.len() - 1)
        } {
            Ok(Some(values)) => Some(Ok(self.values_to_vars(&values))),
            Ok(None) => None,
            Err(e) => Some(Err(e)),
        }
    }
}
