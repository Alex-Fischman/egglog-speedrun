//! This module contains an implementation of the Union Find datastructure,
//! also called the Disjoint Set Union datastructure.

/// A data structure for efficiently unioning sets of `usize`s.
/// It also supports having each set map to a value, and merging them when their
/// sets are unioned. If you don't want values, use () as the value and call `default`.
pub struct UnionFind<'a, V> {
    trees: Vec<ParentOrValue<V>>,
    merge: Box<dyn Fn(V, V) -> Result<V, String> + 'a>,
}

enum ParentOrValue<V> {
    Parent(usize),
    Value(V),
}

impl Default for UnionFind<'static, ()> {
    fn default() -> UnionFind<'static, ()> {
        UnionFind {
            trees: Vec::new(),
            merge: Box::new(|_: (), _: ()| Ok(())),
        }
    }
}

impl<'a, V> UnionFind<'a, V> {
    /// Create a new `UnionFind` with a given merge function.
    pub fn new(merge: impl Fn(V, V) -> Result<V, String> + 'a) -> UnionFind<'a, V> {
        UnionFind {
            trees: Vec::new(),
            merge: Box::new(merge),
        }
    }

    /// Create a new key with a given value and return it.
    pub fn new_key(&mut self, value: V) -> usize {
        self.trees.push(ParentOrValue::Value(value));
        self.trees.len() - 1
    }

    /// Get the canonical key associated with the given key.
    pub fn find(&mut self, key: usize) -> usize {
        match &self.trees[key] {
            ParentOrValue::Value(_) => key,
            ParentOrValue::Parent(parent) => {
                let out = self.find(*parent);
                self.trees[key] = ParentOrValue::Parent(out);
                out
            }
        }
    }

    /// Get the value associated with the given key.
    pub fn find_value(&mut self, key: usize) -> &V {
        let key = self.find(key);
        match &self.trees[key] {
            ParentOrValue::Value(v) => v,
            ParentOrValue::Parent(_) => unreachable!(), // key must be a root!
        }
    }

    /// Union the sets that the given keys belong to, erroring if the merge function errors.
    pub fn union(&mut self, a: usize, b: usize) -> Result<(), String> {
        let a = self.find(a);
        let b = self.find(b);
        if a != b {
            // We want to move x and y into self.merge, so we swap them with a default value.
            // This is okay since we're about to replace them. (Luckily we have a default!)
            let x = std::mem::replace(&mut self.trees[a], ParentOrValue::Parent(0));
            let y = std::mem::replace(&mut self.trees[b], ParentOrValue::Parent(0));
            let z = match (x, y) {
                (ParentOrValue::Value(x), ParentOrValue::Value(y)) => (self.merge)(x, y)?,
                _ => unreachable!(), // a and b are both roots!
            };
            self.trees[a] = ParentOrValue::Value(z);
            self.trees[b] = ParentOrValue::Parent(a);
        }
        Ok(())
    }

    /// Merge a new value into an existing set without creating a new key.
    pub fn merge(&mut self, key: usize, x: V) -> Result<(), String> {
        // See comments for `Database::union` on why we do it this way.
        let key = self.find(key);
        let y = std::mem::replace(&mut self.trees[key], ParentOrValue::Parent(0));
        let z = match y {
            ParentOrValue::Value(y) => (self.merge)(x, y)?,
            ParentOrValue::Parent(_) => unreachable!(),
        };
        self.trees[key] = ParentOrValue::Value(z);
        Ok(())
    }

    /// Get all of the canonical keys and their values in this `UnionFind`.
    pub fn iter(&self) -> impl Iterator<Item = (usize, &V)> {
        self.trees.iter().enumerate().filter_map(|(k, v)| match v {
            ParentOrValue::Parent(_) => None,
            ParentOrValue::Value(v) => Some((k, v)),
        })
    }
}
