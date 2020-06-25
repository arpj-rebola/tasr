use std::{
    cmp::{Ordering},
};

struct BstNode<K: Ord, V> {
    key: K,
    value: V,
    left: Option<usize>,
    right: Option<usize>,
}

pub struct BinarySearchTree<K: Ord, V> {
    root: usize,
    vec: Vec<BstNode<K, V>>,
}
impl<K: Ord, V> BinarySearchTree<K, V> {
    pub fn new() -> BinarySearchTree<K, V> {
        BinarySearchTree::<K, V> {
            root: 0usize,
            vec: Vec::<BstNode<K, V>>::new(),
        }
    }
    pub fn insert(&mut self, key: K, value: V) -> Option<()> {
        if self.vec.is_empty() {
            self.root = 0usize;
        } else {
            let next = self.vec.len();
            let pindex = unsafe { self.leaf(&key, self.root) };
            let parent = unsafe { self.vec.get_unchecked_mut(pindex) };
            match key.cmp(&parent.key) {
                Ordering::Less => parent.left = Some(next),
                Ordering::Greater => parent.right = Some(next),
                Ordering::Equal => return None,
            }
        }
        let node = BstNode::<K, V> {
            key: key,
            value: value,
            left: None,
            right: None,
        };
        self.vec.push(node);
        Some(())
    }
    pub fn clear(&mut self) {
        self.vec.clear()
    }
    pub fn get(&self, key: &K) -> Option<&V> {
        if self.vec.is_empty() {
            None
        } else {
            let pindex = unsafe { self.leaf(&key, self.root) };
            let parent = unsafe { self.vec.get_unchecked(pindex) };
            if key == &parent.key {
                Some(&parent.value)
            } else {
                None
            }
        }
    }
    pub fn iter<'a, 'b: 'a>(&'b self) -> BstIterator<'a, K, V> {
        let mut it = BstIterator::<'a, K, V> {
            bst: self,
            stack: Vec::<usize>::new(),
        };
        if !it.bst.vec.is_empty() {
            it.down_left()
        }
        it
    }
    unsafe fn leaf(&self, key: &K, index: usize) -> usize {
        let mut curr = index;
        loop {
            let node = self.vec.get_unchecked(curr);
            let next = match key.cmp(&node.key) {
                Ordering::Less => &node.left,
                Ordering::Greater => &node.right,
                Ordering::Equal => break curr,
            };
            match next {
                &Some(nx) => curr = nx,
                None => break curr,
            }
        }
    }
}
impl<'a, K: Ord, V> IntoIterator for &'a BinarySearchTree<K, V> {
    type Item = (&'a K, &'a V);
    type IntoIter = BstIterator<'a, K, V>;
    fn into_iter(self) -> BstIterator<'a, K, V> {
        self.iter()
    }
}

pub struct BstIterator<'a, K: Ord, V> {
    bst: &'a BinarySearchTree<K, V>,
    stack: Vec<usize>,
}
impl<'a, K: Ord, V> BstIterator<'a, K, V> {
    fn down_left(&mut self) {
        if self.stack.is_empty() {
            self.stack.push(self.bst.root);
        }
        let mut last = *self.stack.last().unwrap();
        while let Some(l) = unsafe { self.bst.vec.get_unchecked(last).left } {
            self.stack.push(l);
            last = l;
        }
    }
}
impl<'a, K: Ord, V> Iterator for BstIterator<'a, K, V> {
    type Item = (&'a K, &'a V);
    fn next(&mut self) -> Option<(&'a K, &'a V)> {
        let last = self.stack.pop()?;
        let node = unsafe { self.bst.vec.get_unchecked(last) };
        if let Some(r) = node.right {
            self.stack.push(r);
            self.down_left()
        }
        Some((&node.key, &node.value))
    }
}