//! This module contains an implementation of the Union Find datastructure,
//! also called the Disjoint Set Union datastructure.

/// A data structure for efficiently unioning sets.
/// It also supports having each set map to a value.
/// If you don't want this, use () as the value and call `default`.
pub struct UnionFind<'a, V> {
    trees: Vec<ParentOrValue<V>>,
    merge: Box<dyn Fn(V, V) -> Result<V, String> + 'a>,
}

/// We don't let users change the key type, but we do provide a nice alias!
pub type Key = usize;

enum ParentOrValue<V> {
    Parent(Key),
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
    pub fn new_key(&mut self, value: V) -> Key {
        self.trees.push(ParentOrValue::Value(value));
        self.trees.len() - 1
    }

    /// Get the canonical key and value associated with the given key.
    #[must_use]
    pub fn find(&self, key: Key) -> (Key, &V) {
        match &self.trees[key] {
            ParentOrValue::Parent(key) => self.find(*key),
            ParentOrValue::Value(value) => (key, value),
        }
    }

    /// Union the sets that the given keys belong to, erroring if the merge function errors.
    #[allow(clippy::many_single_char_names)]
    pub fn union(&mut self, a: Key, b: Key) -> Result<(), String> {
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
}
