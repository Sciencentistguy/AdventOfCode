#![feature(pattern)]

use std::str::pattern::Pattern;

use collect_slice::CollectSlice;

pub trait ArraySplit {
    type Out<'a>
    where
        Self: 'a;

    fn array_split<const N: usize>(&self, pat: impl Pattern) -> [Self::Out<'_>; N];
    fn array_lines<const N: usize>(&self) -> [Self::Out<'_>; N];
}

impl ArraySplit for str {
    type Out<'a> = &'a Self;

    fn array_split<const N: usize>(&self, pat: impl Pattern) -> [Self::Out<'_>; N] {
        let mut a = [""; N];
        self.split(pat).collect_slice_checked(&mut a);
        a
    }

    fn array_lines<const N: usize>(&self) -> [Self::Out<'_>; N] {
        let mut a = [""; N];
        self.lines().collect_slice_checked(&mut a);
        a
    }
}
