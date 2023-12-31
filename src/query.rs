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
    expr_deps: BTreeMap<(usize, usize), HashSet<usize>>,
    /// A dependency map used to generate an ordering. The keys are (class, call) pairs,
    /// and the values are sets of class indices that should be computed before the call.
    call_deps: BTreeMap<(usize, usize), HashSet<usize>>,
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
        let mut names_to_keys: HashMap<String, Vec<usize>> = HashMap::default();
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
        let mut expr_deps: BTreeMap<(usize, usize), HashSet<usize>> = BTreeMap::new();
        for (key, class) in &classes {
            for (i, expr) in class.exprs.iter().enumerate() {
                let mut set = HashSet::default();
                deps_from_expr(expr, &names_to_keys, &mut set);
                assert!(expr_deps.insert((*key, i), set).is_none());
            }
        }

        // Compute dependency constraints which give an ordering for call computation.
        let mut call_deps: BTreeMap<(usize, usize), HashSet<usize>> = BTreeMap::new();
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
    pub fn run<'a: 'c, 'b: 'c, 'c>(
        &'a self,
        funcs: &'b Funcs,
        seminaive: bool,
    ) -> impl Iterator<Item = Result<Vars<'a>, String>> + 'c {
        let mut constraints = Vec::new();
        constraints.extend(
            self.expr_deps
                .keys()
                .map(|&(class, index)| Constraint::Expr { class, index }),
        );
        constraints.extend(
            self.call_deps
                .keys()
                .map(|&(y, index)| Constraint::Row { y, index }),
        );

        let mut call_order: Vec<_> = self.call_deps.keys().copied().collect();
        call_order.sort_unstable(); // determinism

        let iterations: Box<dyn Iterator<Item = _>> = if seminaive {
            Box::new(
                (1..2_usize.pow(call_order.len().try_into().unwrap())).map(move |i| {
                    call_order
                        .iter()
                        .enumerate()
                        .map(|(j, call)| match (i >> j) & 1 {
                            0 => (*call, Iteration::Past),
                            1 => (*call, Iteration::Prev),
                            _ => unreachable!(),
                        })
                        .collect()
                }),
            )
        } else {
            Box::new(once(
                call_order
                    .iter()
                    .map(|call| (*call, Iteration::All))
                    .collect(),
            ))
        };

        iterations.flat_map(move |iteration| -> Box<dyn Iterator<Item = _>> {
            let mut bindings = Bindings {
                query: self,
                funcs,
                iteration,
                todo: constraints.clone(),
                trie: Vec::new(),
            };
            while !bindings.todo.is_empty() {
                let values = if bindings.trie.is_empty() {
                    HashMap::default()
                } else {
                    match bindings.advance(bindings.trie.len() - 1) {
                        Ok(Some(values)) => values,
                        Ok(None) => return Box::new(empty()),
                        Err(e) => return Box::new(once(Err(e))),
                    }
                };
                bindings.build(bindings.trie.len(), values);
            }
            Box::new(bindings)
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
        Expr::Call(ref f, ref xs) => {
            let xs = xs
                .iter()
                .map(|x| eqs_from_expr(x.clone(), funcs, eqs))
                .collect::<Result<Vec<_>, _>>()?;
            if funcs.contains(&f) {
                eqs.new_key(EqClass {
                    calls: vec![(f.clone(), xs)],
                    ..EqClass::default()
                })
            } else {
                eqs.new_key(EqClass {
                    exprs: vec![expr],
                    ..EqClass::default()
                })
            }
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
    /// The `Query` that was used to construct this iterator.
    query: &'a Query<'a>,
    /// The current state of the `Table`s in the `Database`.
    funcs: &'b Funcs,
    /// Which `Iteration` values to use for each row constraint in seminaive.
    iteration: HashMap<(usize, usize), Iteration>,
    /// Constraints to complete before we're done with the trie. Not in order.
    todo: Vec<Constraint>,
    /// A lazy trie over the bindings, represented as an ordered list of iterators.
    /// Each iterator is bundled with the constraint used to generate it, if there is one.
    trie: Vec<Layer<'b>>,
}

struct Layer<'b> {
    constraint: Option<Constraint>,
    iterator: Box<dyn Iterator<Item = Result<Values, String>> + 'b>,
}

type Values = HashMap<usize, Value>;

/// A constraint that will be used to generate one layer of the trie.
#[derive(Clone)]
enum Constraint {
    /// Assert that an expression is equal to a class.
    Expr {
        /// The class that the constraint is in.
        class: usize,
        /// The index of the constraint inside the class.
        index: usize,
    },
    /// Assert that all classes come from the same row in a table.
    Row {
        /// The class that the constraint is in.
        y: usize,
        /// The index of the constraint inside the class.
        index: usize,
    },
}

impl<'a, 'b> Bindings<'a, 'b> {
    /// Convert the keys of a map from classes to names.
    fn values_to_vars(&self, values: &Values) -> Vars<'a> {
        values
            .iter()
            .filter_map(|(class, value)| {
                self.query.classes[class]
                    .name
                    .as_ref()
                    .map(|name| (name.as_str(), value.clone()))
            })
            .collect()
    }

    /// Advance the lazy trie layer at `height` to the next value and return it.
    fn advance(&mut self, height: usize) -> Result<Option<Values>, String> {
        loop {
            if let Some(values) = self.trie[height].iterator.next() {
                return Ok(Some(values?));
            } else if height == 0 {
                return Ok(None);
            } else if let Some(values) = self.advance(height - 1)? {
                self.build(height, values);
            } else {
                return Ok(None);
            }
        }
    }

    /// Build a single layer of the trie.
    fn build(&mut self, height: usize, prev: Values) {
        self.todo
            .extend(self.trie.drain(height..).filter_map(|mut l| {
                assert!(l.iterator.next().is_none());
                l.constraint
            }));

        assert!(!self.todo.is_empty());
        let known: HashSet<usize> = prev.keys().copied().collect();
        // None: todo, Some(None): pass, Some(Some(i)): remove ith constraint
        let mut next: Option<Option<usize>> = None;
        let mut iter: Option<Box<dyn Iterator<Item = Result<Vec<_>, _>>>> = None;

        // First, try to resolve any expr constraint.
        for (i, constraint) in self.todo.iter().enumerate() {
            if let &Constraint::Expr { class, index } = constraint {
                if self.query.expr_deps[&(class, index)].is_subset(&known) {
                    let y = self.query.classes[&class].exprs[index]
                        .evaluate_ref(&self.values_to_vars(&prev), self.funcs);
                    next = Some(Some(i));
                    iter = Some(match y {
                        Ok(Some(v)) => Box::new(once(Ok(vec![(class, v)]))),
                        Ok(None) => Box::new(empty()),
                        Err(e) => Box::new(once(Err(e))),
                    });
                    break;
                }
            }
        }
        if next.is_none() {
            // Then, try to resolve the shortest row constriant.
            let mut vec: Vec<(_, Peekable<Box<dyn Iterator<Item = _>>>)> = Vec::new();
            for (i, constraint) in self.todo.iter().enumerate() {
                if let &Constraint::Row { y, index } = constraint {
                    let (f, xs) = &self.query.classes[&y].calls[index];
                    let iteration = self.iteration[&(y, index)];
                    let mut in_vec = false;
                    for (c, x) in xs.iter().chain([&y]).enumerate() {
                        if let Some(v) = prev.get(x) {
                            in_vec = true;
                            vec.push((
                                (i, f, xs, y, iteration, Some((v, c))),
                                (Box::new(self.funcs[f].rows_with_value(iteration, v, c))
                                    as Box<dyn Iterator<Item = _>>)
                                    .peekable(),
                            ));
                        }
                    }
                    // Any column index must be shorter than the whole table
                    if !in_vec {
                        vec.push((
                            (i, f, xs, y, iteration, None),
                            (Box::new(self.funcs[f].rows(iteration))
                                as Box<dyn Iterator<Item = _>>)
                                .peekable(),
                        ));
                    }
                }
            }
            if let Some((i, f, xs, y, iteration, option)) = pick_shortest(&mut vec) {
                let mut cs = xs.clone();
                cs.push(y);
                let zip_row =
                    move |vs: &[Value]| Ok(cs.iter().copied().zip(vs.iter().cloned()).collect());

                next = Some(Some(i));
                iter = if let Some((v, c)) = option {
                    Some(Box::new(
                        self.funcs[f].rows_with_value(iteration, v, c).map(zip_row),
                    ))
                } else {
                    Some(Box::new(self.funcs[f].rows(iteration).map(zip_row)))
                };
            }
        }
        if next.is_none() {
            // Otherwise, give up, since the query wasn't range restricted.
            assert!(self
                .todo
                .iter()
                .all(|i| matches!(i, Constraint::Expr { .. })));
            next = Some(None);
            iter = Some(Box::new(once(Err(format!(
                "query was not range-restricted in {}",
                self.query.slice
            )))));
        }

        // Check that none of the iterators values collide with known values
        let iterator = Box::new(iter.unwrap().filter_map(move |result| match result {
            Err(e) => Some(Err(e)),
            Ok(curr) => {
                let mut prev = prev.clone();
                for (class, value) in &curr {
                    if let Some(v) = prev.get(class) {
                        if v != value {
                            return None;
                        }
                    } else {
                        let old = prev.insert(*class, value.clone());
                        assert!(old.is_none());
                    }
                }
                Some(Ok(prev))
            }
        }));

        self.trie.push(Layer {
            constraint: next.unwrap().map(|i| self.todo.remove(i)),
            iterator,
        });
    }
}

impl<'a, 'b> Iterator for Bindings<'a, 'b> {
    type Item = Result<Vars<'a>, String>;
    fn next(&mut self) -> Option<Result<Vars<'a>, String>> {
        match self.advance(self.trie.len() - 1) {
            Ok(Some(values)) => Some(Ok(self.values_to_vars(&values))),
            Ok(None) => None,
            Err(e) => Some(Err(e)),
        }
    }
}

/// Goes through the iterators and iterates each one until one of the iterators runs out.
/// then returns the data associated with that iterator and its length. Do not rely on
/// the state of the iterators after this function is finished.
fn pick_shortest<T, I: Iterator>(vec: &mut Vec<(T, Peekable<I>)>) -> Option<T> {
    if vec.is_empty() {
        None
    } else {
        loop {
            for (i, (_, iterator)) in vec.iter_mut().enumerate() {
                if iterator.peek().is_none() {
                    return Some(vec.remove(i).0);
                }
                iterator.next();
            }
        }
    }
}
