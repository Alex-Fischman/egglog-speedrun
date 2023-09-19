//! This module contains an implementation of the Union Find datastructure,
//! also called the Disjoint Set Union datastructure.

use crate::*;

/// A data structure for efficiently unioning sets of `usize`s.
/// It also supports having each set map to a value, and merging them when their
/// sets are unioned. If you don't want values, use () as the value and call `default`.
pub struct UnionFind<'a, V> {
    /// The graph that this data structure uses internally.
    trees: Vec<Node<V>>,
    /// The function to combine to `V`s.
    merge: Box<dyn Fn(V, V) -> Result<V, String> + 'a>,
    /// The set of keys that were canonical but aren't anymore.
    dirty: HashSet<usize>,
}

enum Node<V> {
    // index of parent
    Child(usize),
    // value, rank
    Root(V, usize),
}

impl Default for UnionFind<'static, ()> {
    fn default() -> UnionFind<'static, ()> {
        UnionFind {
            trees: Vec::new(),
            merge: Box::new(|(), ()| Ok(())),
            dirty: HashSet::default(),
        }
    }
}

impl<'a, V> UnionFind<'a, V> {
    /// Create a new `UnionFind` with a given merge function.
    pub fn new(merge: impl Fn(V, V) -> Result<V, String> + 'a) -> UnionFind<'a, V> {
        UnionFind {
            trees: Vec::new(),
            merge: Box::new(merge),
            dirty: HashSet::default(),
        }
    }

    /// Create a new key with a given value and return it.
    pub fn new_key(&mut self, value: V) -> usize {
        self.trees.push(Node::Root(value, 0));
        self.trees.len() - 1
    }

    /// Get the canonical key associated with the given key.
    pub fn find(&mut self, key: usize) -> usize {
        match &self.trees[key] {
            Node::Root(..) => key,
            Node::Child(parent) => {
                let root = self.find(*parent);
                self.trees[key] = Node::Child(root);
                root
            }
        }
    }

    /// Union the sets that the given keys belong to, erroring if the merge function errors.
    /// Returns the new root, as well as whether the `UnionFind` was changed or not.
    pub fn union(&mut self, a: usize, b: usize) -> Result<(usize, bool), String> {
        let a = self.find(a);
        let b = self.find(b);
        if a == b {
            return Ok((a, false));
        }

        // We want to move x and y into self.merge, so we swap them with a default value.
        // This is okay since we're about to replace them. (Luckily we have a default!)
        let x = replace(&mut self.trees[a], Node::Child(0));
        let y = replace(&mut self.trees[b], Node::Child(0));
        let (z, p, q) = match (x, y) {
            (Node::Root(x, p), Node::Root(y, q)) => ((self.merge)(x, y)?, p, q),
            _ => unreachable!(), // a and b are both roots!
        };
        match p.cmp(&q) {
            Ordering::Less => {
                self.trees[a] = Node::Child(b);
                self.trees[b] = Node::Root(z, q);
                self.dirty.insert(a);
                Ok((b, true))
            }
            Ordering::Greater => {
                self.trees[a] = Node::Root(z, p);
                self.trees[b] = Node::Child(a);
                self.dirty.insert(b);
                Ok((a, true))
            }
            Ordering::Equal => {
                self.trees[a] = Node::Root(z, p + 1);
                self.trees[b] = Node::Child(a);
                self.dirty.insert(b);
                Ok((a, true))
            }
        }
    }

    /// Get all of the keys in this `UnionFind`.
    pub fn keys(&self) -> impl Iterator<Item = usize> {
        0..self.trees.len()
    }

    /// Get all of the canonical keys and their values in this `UnionFind`.
    pub fn iter(&self) -> impl Iterator<Item = (usize, &V)> {
        self.trees.iter().enumerate().filter_map(|(k, v)| match v {
            Node::Child(_) => None,
            Node::Root(v, _) => Some((k, v)),
        })
    }

    /// Get all of the keys that used to be canonical but aren't anymore,
    /// since the last time this function was called.
    pub fn dirty(&mut self) -> HashSet<usize> {
        take(&mut self.dirty)
    }
}

impl<V> IntoIterator for UnionFind<'_, V> {
    type Item = (usize, V);
    type IntoIter = Canonical<V>;
    fn into_iter(self) -> Canonical<V> {
        Canonical(
            self.trees
                .into_iter()
                .enumerate()
                .filter_map(|(k, v)| match v {
                    Node::Child(_) => None,
                    Node::Root(v, _) => Some((k, v)),
                }),
        )
    }
}

/// An iterator over the canonical keys and values of a `UnionFind`.
// We could inline this, but we use it to hide the private `Node` type.
pub struct Canonical<V>(
    FilterMap<Enumerate<IntoIter<Node<V>>>, fn((usize, Node<V>)) -> Option<(usize, V)>>,
);

impl<V> Iterator for Canonical<V> {
    type Item = (usize, V);
    fn next(&mut self) -> Option<(usize, V)> {
        self.0.next()
    }
}
