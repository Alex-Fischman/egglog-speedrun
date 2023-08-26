//! This module defines the state for the whole `egglog` program.

use crate::*;

/// The current state of the `egglog` program.
#[derive(Default)]
pub struct Database<'a> {
    sorts: Vec<String>,
    funcs: HashMap<String, Table>,
    rules: Vec<(Query<'a>, Vec<Action<'a>>)>,
}

impl Display for Database<'_> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        for (f_, table) in &self.funcs {
            for row in table.rows() {
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
        if self.sorts.contains(&sort) {
            return Err(format!("{sort} was declared twice"));
        }
        self.sorts.push(sort);
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
        run_action(action, &HashMap::new(), &mut self.funcs)?;
        Ok(self)
    }

    /// Get the value of `expr` given the functions in this `Database`.
    pub fn check(&self, query: &Query) -> Result<bool, String> {
        match query.run(&self.funcs)?.next() {
            Some(Ok(_)) => Ok(true),
            None => Ok(false),
            Some(Err(e)) => Err(e),
        }
    }

    /// Run the rules in this `Database` to fixpoint.
    pub fn run(&mut self) -> Result<(), String> {
        let mut changed = true;
        while changed {
            changed = false;
            let pre = self.funcs.clone();
            for (query, actions) in &mut self.rules {
                for vars in query.run(&pre)? {
                    let vars = vars?;
                    for action in &mut *actions {
                        if run_action(action, &vars, &mut self.funcs)? {
                            changed = true;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    /// Get the names of the functions in this database.
    #[must_use]
    pub fn funcs(&self) -> HashSet<&String> {
        self.funcs.keys().collect()
    }
}

/// Returns true if running the action changed `funcs`.
/// Not a method on `Database` because we need to not borrow `rules`.
fn run_action(
    action: &Action,
    vars: &HashMap<&str, Value>,
    funcs: &mut HashMap<String, Table>,
) -> Result<bool, String> {
    match action {
        Action::Insert(_, f, xs, y) => {
            let xs = xs
                .iter()
                .map(|x| x.evaluate(vars, funcs))
                .collect::<Result<Vec<_>, _>>()?;
            let y = y.evaluate(vars, funcs)?;
            let changed = funcs
                .get_mut(f.as_str())
                .ok_or(format!("unknown function {f}"))?
                .insert(&xs, y)?;
            Ok(changed)
        }
    }
}
