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

    /// Get the canonical key and value associated with the given key.
    #[must_use]
    pub fn find(&self, key: usize) -> (usize, &V) {
        match &self.trees[key] {
            ParentOrValue::Parent(key) => self.find(*key),
            ParentOrValue::Value(value) => (key, value),
        }
    }

    /// Union the sets that the given keys belong to, erroring if the merge function errors.
    pub fn union(&mut self, a: usize, b: usize) -> Result<(), String> {
        let (a, _) = self.find(a);
        let (b, _) = self.find(b);
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

    /// Get all of the canonical keys and their values in this `UnionFind`.
    pub fn iter(&self) -> impl Iterator<Item = (usize, &V)> {
        self.trees.iter().enumerate().filter_map(|(k, v)| match v {
            ParentOrValue::Parent(_) => None,
            ParentOrValue::Value(v) => Some((k, v)),
        })
    }
}
