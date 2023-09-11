//! This module defines the state for the whole `egglog` program.

use crate::*;

/// The current state of the `egglog` program.
#[derive(Default)]
pub struct Database<'a> {
    rules: Vec<(Query<'a>, Vec<Action>)>,
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
        actions: Vec<Action>,
    ) -> Result<&mut Database<'a>, String> {
        self.rules.push((query, actions));
        Ok(self)
    }

    /// Run an action to this `Database`.
    pub fn action(&mut self, action: &Action) -> Result<&mut Database<'a>, String> {
        run_action(action, &HashMap::new(), &mut self.funcs, &mut self.sorts)?;
        rebuild(&mut self.funcs, &mut self.sorts)?;
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
    pub fn run(&mut self, iterations: Option<usize>) -> Result<(), String> {
        fixpoint(iterations, self, |db| {
            db.funcs.values_mut().for_each(Table::iteration_start);
            let mut changed = false;
            for (query, actions) in &db.rules {
                let bindings: Vec<Vars> = query.run(&db.funcs, true).collect::<Result<_, _>>()?;
                for vars in bindings {
                    for action in actions {
                        changed |= run_action(action, &vars, &mut db.funcs, &mut db.sorts)?;
                    }
                }
                if changed {
                    rebuild(&mut db.funcs, &mut db.sorts)?;
                }
            }
            Ok(changed)
        })
    }

    /// Get the names of the functions in this database.
    #[must_use]
    pub fn funcs(&self) -> HashSet<&String> {
        self.funcs.keys().collect()
    }

    /// Get the length of a table in this database.
    pub fn get_table_len(&self, func: &str) -> Result<usize, String> {
        match self.funcs.get(func) {
            Some(table) => Ok(table.len()),
            None => Err(format!("no function {func} in database")),
        }
    }
}

/// Returns true if running the action changed `funcs` or `sorts`.
// Not a method on `Database` because we need to not borrow `rules`.
fn run_action(
    action: &Action,
    vars: &Vars,
    funcs: &mut Funcs,
    sorts: &mut Sorts,
) -> Result<bool, String> {
    // Run the action
    Ok(match action {
        Action::Insert(f, xs, y) => {
            let row = xs
                .iter()
                .chain([y])
                .map(|x| x.evaluate_mut(vars, funcs, sorts))
                .collect::<Result<Vec<_>, _>>()?;
            funcs
                .get_mut(f.as_str())
                .ok_or(format!("unknown function {f}"))?
                .insert(row, sorts)?
        }
        Action::GetMut(f, xs) => {
            let xs = xs
                .iter()
                .map(|x| x.evaluate_mut(vars, funcs, sorts))
                .collect::<Result<Vec<_>, _>>()?;
            funcs
                .get_mut(f.as_str())
                .ok_or(format!("unknown function {f}"))?
                .get_mut(xs, sorts)?
                .1
        }
        Action::Union(x, y, s) => match (
            x.evaluate_mut(vars, funcs, sorts)?,
            y.evaluate_mut(vars, funcs, sorts)?,
        ) {
            (Value::Sort(x), Value::Sort(y)) => sorts.get_mut(s).unwrap().union(x, y)?,
            (_, _) => unreachable!(),
        },
    })
}

/// Rebuild every table in this database.
// Not a method on `Database` because we need to not borrow `rules`.
fn rebuild(funcs: &mut Funcs, sorts: &mut Sorts) -> Result<(), String> {
    fixpoint(None, &mut (funcs, sorts), |(funcs, sorts)| {
        let dirty: HashMap<String, HashSet<usize>> = sorts
            .iter_mut()
            .map(|(sort, uf)| (sort.clone(), uf.dirty()))
            .collect();

        let mut changed = false;
        for table in funcs.values_mut() {
            changed |= table.rebuild(sorts, &dirty)?;
        }
        Ok(changed)
    })
}

fn fixpoint<X, F>(iterations: Option<usize>, x: &mut X, f: F) -> Result<(), String>
where
    F: Fn(&mut X) -> Result<bool, String>,
{
    let mut changed = true;
    let mut i = 0;
    while changed && iterations.map_or(true, |j| i < j) {
        changed = f(x)?;
        i += 1;
    }
    Ok(())
}
