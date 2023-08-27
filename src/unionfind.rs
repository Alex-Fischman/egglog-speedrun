//! This module contains an implementation of the Union Find datastructure,
//! also called the Disjoint Set Union datastructure.

/// A data structure for efficiently unioning sets of `usize`s.
/// It also supports having each set map to a value, and merging them when their
/// sets are unioned. If you don't want values, use () as the value and call `default`.
pub struct UnionFind<'a, V> {
    trees: Vec<Node<V>>,
    merge: Box<dyn Fn(V, V) -> Result<V, String> + 'a>,
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

    /// Get the value associated with the given key.
    pub fn find_value(&mut self, key: usize) -> &V {
        let root = self.find(key);
        match &self.trees[root] {
            Node::Root(v, _) => v,
            Node::Child(_) => unreachable!(), // root must be a root!
        }
    }

    /// Union the sets that the given keys belong to, erroring if the merge function errors.
    /// Returns whether the `UnionFind` was changed or not.
    pub fn union(&mut self, a: usize, b: usize) -> Result<bool, String> {
        let a = self.find(a);
        let b = self.find(b);
        if a == b {
            return Ok(false);
        }

        // We want to move x and y into self.merge, so we swap them with a default value.
        // This is okay since we're about to replace them. (Luckily we have a default!)
        let x = std::mem::replace(&mut self.trees[a], Node::Child(0));
        let y = std::mem::replace(&mut self.trees[b], Node::Child(0));
        let (z, p, q) = match (x, y) {
            (Node::Root(x, p), Node::Root(y, q)) => ((self.merge)(x, y)?, p, q),
            _ => unreachable!(), // a and b are both roots!
        };
        match p.cmp(&q) {
            std::cmp::Ordering::Less => {
                self.trees[a] = Node::Child(b);
                self.trees[b] = Node::Root(z, q);
            }
            std::cmp::Ordering::Greater => {
                self.trees[a] = Node::Root(z, p);
                self.trees[b] = Node::Child(a);
            }
            std::cmp::Ordering::Equal => {
                self.trees[a] = Node::Root(z, p + 1);
                self.trees[b] = Node::Child(a);
            }
        }
        Ok(true)
    }

    /// Merge a new value into an existing set without creating a new key.
    pub fn merge(&mut self, key: usize, x: V) -> Result<(), String> {
        // See comments for `Database::union` on why we do it this way.
        let root = self.find(key);
        let y = std::mem::replace(&mut self.trees[root], Node::Child(0));
        let (z, r) = match y {
            Node::Root(y, r) => ((self.merge)(x, y)?, r),
            Node::Child(_) => unreachable!(),
        };
        self.trees[root] = Node::Root(z, r);
        Ok(())
    }

    /// Get all of the canonical keys and their values in this `UnionFind`.
    pub fn iter(&self) -> impl Iterator<Item = (usize, &V)> {
        self.trees.iter().enumerate().filter_map(|(k, v)| match v {
            Node::Child(_) => None,
            Node::Root(v, _) => Some((k, v)),
        })
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
    std::iter::FilterMap<
        std::iter::Enumerate<std::vec::IntoIter<Node<V>>>,
        fn((usize, Node<V>)) -> Option<(usize, V)>,
    >,
);

impl<V> Iterator for Canonical<V> {
    type Item = (usize, V);
    fn next(&mut self) -> Option<(usize, V)> {
        self.0.next()
    }
}
