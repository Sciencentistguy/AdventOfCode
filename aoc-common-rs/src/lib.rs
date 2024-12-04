#![feature(pattern)]

use std::{ops::{Add, AddAssign}, str::pattern::Pattern};

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Vec2D<T> {
    pub x: T,
    pub y: T,
}

impl<T> Add for Vec2D<T>
where
    T: Add<Output = T>,
{
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        Self {
            x: self.x + rhs.x,
            y: self.y + rhs.y,
        }
    }
}

impl<T> AddAssign for Vec2D<T>
where
    T: AddAssign,
{
    fn add_assign(&mut self, rhs: Self) {
        self.x += rhs.x;
        self.y += rhs.y;
    }
}

impl<T> From<(T, T)> for Vec2D<T> {
    fn from((x, y): (T, T)) -> Self {
        Self { x, y }
    }
}