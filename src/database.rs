//! This module defines the state for the whole `egglog` program.

use crate::*;

/// The current state of the `egglog` program.
#[derive(Default)]
pub struct Database<'a> {
    sorts: Vec<String>,
    funcs: HashMap<String, Table>,
    rules: Vec<(Query, Vec<Action<'a>>)>,
}

impl Display for Database<'_> {
    fn fmt(&self, f: &mut Formatter) -> FmtResult {
        for (f_, table) in &self.funcs {
            for (xs, y) in table.rows() {
                writeln!(
                    f,
                    "{f_}: {}: {y}",
                    xs.iter()
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
        xs: Vec<Type>,
        y: Type,
        merge: Option<Expr>,
    ) -> Result<&mut Database<'a>, String> {
        if self.funcs.contains_key(&f) {
            return Err(format!("{f} was declared twice"));
        }
        self.funcs.insert(f.clone(), Table::new(f, xs, y, merge));
        Ok(self)
    }

    /// Add a rule to this `Database`.
    pub fn rule(
        &mut self,
        query: Query,
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
    pub fn check(&mut self, expr: &Expr) -> Result<Value, String> {
        expr.evaluate(&HashMap::new(), &self.funcs)
    }

    /// Run the rules in this `Database` to fixpoint.
    pub fn run(&mut self) -> Result<(), String> {
        let mut changed = true;
        while changed {
            changed = false;
            for (query, actions) in &self.rules {
                for binding in query.run(&self.funcs) {
                    for action in actions {
                        if run_action(action, &binding, &mut self.funcs)? {
                            changed = true;
                        }
                    }
                }
            }
        }
        Ok(())
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
