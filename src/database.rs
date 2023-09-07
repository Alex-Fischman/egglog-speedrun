//! This module defines the state for the whole `egglog` program.

use crate::*;

/// The current state of the `egglog` program.
#[derive(Default)]
pub struct Database<'a> {
    rules: Vec<(Query<'a>, Vec<Action<'a>>)>,
    funcs: Funcs,
    sorts: Sorts,
}

impl Display for Database<'_> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        for (f_, table) in &self.funcs {
            for row in table.rows(&vec![None; table.width()], Iteration::All) {
                writeln!(
                    f,
                    "{f_}: {}",
                    row.iter()
                        .map(|x| format!("{x}"))
                        .collect::<Vec<_>>()
                        .join(" ")
                )?;
            }
        }
        Ok(())
    }
}

impl<'a> Database<'a> {
    /// Add a sort to this `Database`.
    pub fn sort(&mut self, sort: String) -> Result<&mut Database<'a>, String> {
        if self.sorts.contains_key(&sort) {
            return Err(format!("{sort} was declared twice"));
        }
        self.sorts.insert(sort, UnionFind::default());
        Ok(self)
    }

    /// Add a function to this `Database`.
    pub fn function(
        &mut self,
        f: String,
        schema: Vec<Type>,
        merge: Option<Expr>,
    ) -> Result<&mut Database<'a>, String> {
        if self.funcs.contains_key(&f) {
            return Err(format!("{f} was declared twice"));
        }
        self.funcs.insert(f.clone(), Table::new(f, schema, merge));
        Ok(self)
    }

    /// Add a rule to this `Database`.
    pub fn rule(
        &mut self,
        query: Query<'a>,
        actions: Vec<Action<'a>>,
    ) -> Result<&mut Database<'a>, String> {
        self.rules.push((query, actions));
        Ok(self)
    }

    /// Run an action to this `Database`.
    pub fn action(&mut self, action: &Action) -> Result<&mut Database<'a>, String> {
        run_action(action, &HashMap::new(), &mut self.funcs, &mut self.sorts)?;
        Ok(self)
    }

    /// Get the value of `expr` given the functions in this `Database`.
    pub fn check(&self, query: &Query) -> Result<bool, String> {
        match query.run(&self.funcs, false).next() {
            Some(Ok(_)) => Ok(true),
            None => Ok(false),
            Some(Err(e)) => Err(e),
        }
    }

    /// Run the rules in this `Database` to fixpoint.
    pub fn run(&mut self) -> Result<(), String> {
        fixpoint(self, |db| {
            db.funcs.values_mut().for_each(Table::iteration_start);
            let mut changed = false;
            for (query, actions) in &db.rules {
                let bindings: Vec<Vars> = query.run(&db.funcs, true).collect::<Result<_, _>>()?;
                for vars in bindings {
                    for action in actions {
                        changed |= run_action(action, &vars, &mut db.funcs, &mut db.sorts)?;
                    }
                }
            }
            Ok(changed)
        })
        .map(|_| ())
    }

    /// Get the names of the functions in this database.
    #[must_use]
    pub fn funcs(&self) -> HashSet<&String> {
        self.funcs.keys().collect()
    }
}

/// Returns true if running the action changed `funcs` or `sorts`.
/// Not a method on `Database` because we need to not borrow `rules`.
fn run_action(
    action: &Action,
    vars: &Vars,
    funcs: &mut Funcs,
    sorts: &mut Sorts,
) -> Result<bool, String> {
    // Run the action
    let changed = match action {
        Action::Insert(_, f, xs, y) => {
            let row = xs
                .iter()
                .chain([y])
                .map(|x| x.evaluate_mut(vars, funcs, sorts))
                .collect::<Result<Vec<_>, _>>()?;
            funcs
                .get_mut(f.as_str())
                .ok_or(format!("unknown function {f}"))?
                .insert(row, sorts)
        }
        Action::Union(x, y, s) => match (
            x.evaluate_mut(vars, funcs, sorts)?,
            y.evaluate_mut(vars, funcs, sorts)?,
        ) {
            (Value::Sort(x), Value::Sort(y)) => sorts.get_mut(s).unwrap().union(x, y),
            (_, _) => unreachable!(),
        },
    }?;
    if changed {
        // Rebuild the database
        fixpoint(&mut (funcs, sorts), |(funcs, sorts)| {
            let mut changed = false;
            for table in funcs.values_mut() {
                changed |= table.rebuild(sorts)?;
            }
            Ok(changed)
        })?;
        Ok(true)
    } else {
        Ok(false)
    }
}

fn fixpoint<X, F: Fn(&mut X) -> Result<bool, String>>(x: &mut X, f: F) -> Result<bool, String> {
    let mut changed = f(x)?;
    if changed {
        while changed {
            changed = f(x)?;
        }
        Ok(true)
    } else {
        Ok(false)
    }
}
