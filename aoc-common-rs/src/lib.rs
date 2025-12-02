#![feature(pattern)]

use std::str::pattern::Pattern;

use collect_slice::CollectSlice;
use smallvec::SmallVec;

pub mod vec2d;
pub use vec2d::*;
pub mod byte_split;
pub use byte_split::*;

pub trait ArraySplit {
    type Out<'a>
    where
        Self: 'a;

    fn array_split<const N: usize>(&self, pat: impl Pattern) -> [Self::Out<'_>; N];
    fn array_lines<const N: usize>(&self) -> [Self::Out<'_>; N];
    fn array_split_ascii_whitespace<const N: usize>(&self) -> [Self::Out<'_>; N];
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

    fn array_split_ascii_whitespace<const N: usize>(&self) -> [Self::Out<'_>; N] {
        let mut a = [""; N];
        self.split_ascii_whitespace().collect_slice_checked(&mut a);
        a
    }
}

pub trait MonadicJoin {
    type Output;
    fn join(self) -> Self::Output;
}

impl<T> MonadicJoin for Option<Option<T>> {
    type Output = Option<T>;
    fn join(self) -> Self::Output {
        self.and_then(|x| x)
    }
}
impl<'a, T> MonadicJoin for Option<&'a Option<T>> {
    type Output = Option<&'a T>;
    fn join(self) -> Self::Output {
        self.and_then(|x| x.as_ref())
    }
}

#[inline]
pub fn generate_combs_n<T: Clone, const N: usize>(vals: &[T], n: usize) -> Vec<SmallVec<T, N>> {
    #[inline]
    fn generate_combs_recursive<T: Clone, const N: usize>(
        vals: &[T],
        n: usize,
        current: &mut SmallVec<T, N>,
        result: &mut Vec<SmallVec<T, N>>,
    ) {
        if n == 0 {
            result.push(current.clone());
            return;
        }
        for c in vals {
            current.push(c.clone());
            generate_combs_recursive(vals, n - 1, current, result);
            current.pop();
        }
    }

    let num_combs = vals.len().pow(n as u32);
    let mut result = Vec::with_capacity(num_combs);
    let mut current = SmallVec::<_, N>::with_capacity(n);
    generate_combs_recursive(vals, n, &mut current, &mut result);
    result
}
