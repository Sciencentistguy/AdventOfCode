use std::{
    fmt::Display,
    ops::{Add, AddAssign, Div, Mul, Neg, Rem, Sub},
    simd::prelude::*,
};

use ndarray::{Dimension, Ix, Ix2, Ixs, NdIndex};

#[repr(C)]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default, PartialOrd)]
pub struct Vec2D<T> {
    pub x: T,
    pub y: T,
}

impl<T> Vec2D<T> {
    pub const UP: Vec2D<isize> = Vec2D { x: 0, y: -1 };
    pub const DOWN: Vec2D<isize> = Vec2D { x: 0, y: 1 };
    pub const LEFT: Vec2D<isize> = Vec2D { x: -1, y: 0 };
    pub const RIGHT: Vec2D<isize> = Vec2D { x: 1, y: 0 };

    pub const CARDINAL_DIRECTIONS: [Vec2D<isize>; 4] =
        [Self::UP, Self::RIGHT, Self::DOWN, Self::LEFT];

    pub const fn new(x: T, y: T) -> Self {
        Self { x, y }
    }

    pub fn new_opt(x: Option<T>, y: Option<T>) -> Option<Self> {
        Some(Self { x: x?, y: y? })
    }

    pub fn rotate_right(&mut self)
    where
        T: Neg<Output = T> + Copy,
    {
        *self = Self::new(-self.y, self.x);
    }

    pub fn is_divisible_by(&self, divisor: T) -> bool
    where
        T: Rem<Output = T> + PartialEq + From<u8> + Copy,
    {
        self.x % divisor == T::from(0) && self.y % divisor == T::from(0)
    }

    fn as_array(&self) -> &[T; 2] {
        // Safety: Self is repr(C), so the 3 fields are guaranteed to be contiguous and in order.
        unsafe { std::mem::transmute(self) }
    }

    fn cross_product(&self, other: &Self) -> T
    where
        T: Sub<Output = T> + Mul<Output = T> + Copy,
    {
        self.x * other.y - self.y * other.x
    }

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

impl<T> Sub for Vec2D<T>
where
    T: Sub<Output = T>,
{
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        Self {
            x: self.x - rhs.x,
            y: self.y - rhs.y,
        }
    }
}

impl<T> Mul<T> for Vec2D<T>
where
    T: Mul<Output = T> + Copy,
{
    type Output = Self;

    fn mul(self, rhs: T) -> Self {
        Self {
            x: self.x * rhs,
            y: self.y * rhs,
        }
    }
}

impl<T> Div<T> for Vec2D<T>
where
    T: Div<Output = T> + Copy,
{
    type Output = Self;

    fn div(self, rhs: T) -> Self {
        Self {
            x: self.x / rhs,
            y: self.y / rhs,
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

impl<T> From<Vec2D<T>> for [T; 2]
where
    T: Copy,
{
    fn from(vec: Vec2D<T>) -> Self {
        [vec.x, vec.y]
    }
}

impl<T> Add<(T, T)> for Vec2D<T>
where
    T: Add<Output = T>,
{
    type Output = Self;

    fn add(self, rhs: (T, T)) -> Self {
        Self {
            x: self.x + rhs.0,
            y: self.y + rhs.1,
        }
    }
}

impl Display for Vec2D<usize> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.x, self.y)
    }
}

impl Vec2D<usize> {
    pub fn wrapping_add_assign_signed(&mut self, rhs: Vec2D<isize>) {
        self.x = self.x.wrapping_add_signed(rhs.x);
        self.y = self.y.wrapping_add_signed(rhs.y);
    }
    pub fn wrapping_add_signed(&self, rhs: Vec2D<isize>) -> Self {
        Self {
            x: self.x.wrapping_add_signed(rhs.x),
            y: self.y.wrapping_add_signed(rhs.y),
        }
    }
    pub fn checked_add_signed(&self, rhs: Vec2D<isize>) -> Option<Self> {
        self.x
            .checked_add_signed(rhs.x)
            .and_then(|x| self.y.checked_add_signed(rhs.y).map(|y| Self { x, y }))
    }
    pub fn checked_sub_signed(&self, rhs: Vec2D<isize>) -> Option<Self> {
        self.x
            .checked_sub_signed(rhs.x)
            .and_then(|x| self.y.checked_sub_signed(rhs.y).map(|y| Self { x, y }))
    }
    pub fn checked_sub(&self, rhs: Vec2D<usize>) -> Option<Self> {
        self.x
            .checked_sub(rhs.x)
            .and_then(|x| self.y.checked_sub(rhs.y).map(|y| Self { x, y }))
    }
    pub fn signed_sub(&self, rhs: Vec2D<usize>) -> Vec2D<isize> {
        Vec2D {
            x: self.x as isize - rhs.x as isize,
            y: self.y as isize - rhs.y as isize,
        }
    }
    pub fn signed(&self) -> Vec2D<isize> {
        Vec2D {
            x: self.x as isize,
            y: self.y as isize,
        }
    }
    pub fn neighbours(&self) -> impl Iterator<Item = Self> {
        Self::CARDINAL_DIRECTIONS
            .iter()
            .filter_map(|&dir| self.checked_add_signed(dir))
    }
    pub fn signed_neighbours(&self) -> impl Iterator<Item = Vec2D<isize>> + 'static + use<> {
        let signed = self.signed();
        Self::CARDINAL_DIRECTIONS
            .iter()
            .map(move |&dir| signed + dir)
    }

    pub fn cross(&self, rhs: &Self) -> isize {
        self.x as isize * rhs.y as isize - self.y as isize * rhs.x as isize
    }

    pub fn up(&self) -> Option<Self> {
        self.checked_add_signed(Self::UP)
    }
    pub fn down(&self) -> Option<Self> {
        self.checked_add_signed(Self::DOWN)
    }
    pub fn left(&self) -> Option<Self> {
        self.checked_add_signed(Self::LEFT)
    }
    pub fn right(&self) -> Option<Self> {
        self.checked_add_signed(Self::RIGHT)
    }
}

impl Vec2D<isize> {
    pub fn unsigned(&self) -> Option<Vec2D<usize>> {
        if self.x < 0 || self.y < 0 {
            None
        } else {
            Some(Vec2D {
                x: self.x as usize,
                y: self.y as usize,
            })
        }
    }
    pub fn cross(&self, rhs: &Self) -> isize {
        self.x * rhs.y - self.y * rhs.x
    }
}

// Safety: this is identical to the impl for `(usize, usize)` in ndarray.
unsafe impl NdIndex<Ix2> for Vec2D<usize> {
    #[inline]
    fn index_checked(&self, dim: &Ix2, strides: &Ix2) -> Option<isize> {
        dim.stride_offset_checked(strides, &Ix2(self.x, self.y))
    }
    #[inline]
    fn index_unchecked(&self, strides: &Ix2) -> isize {
        fn stride_offset(n: Ix, stride: Ix) -> isize {
            (n as isize) * (stride as Ixs)
        }
        stride_offset(self.x, strides[0]) + stride_offset(self.y, strides[1])
    }
}

pub trait Distance {
    fn distance(&self, other: &Self) -> f64;
}

macro_rules! distance_impl {
    ($t:ty) => {
        impl Distance for Vec2D<$t> {
            fn distance(&self, other: &Self) -> f64 {
                let v_self = Simd::from_array(*self.as_array()).cast::<f64>();
                let v_other = Simd::from_array(*other.as_array()).cast::<f64>();

                let v_d = v_self - v_other;

                let v_sq = v_d * v_d;

                v_sq.reduce_sum().sqrt()
            }
        }
    };
}

distance_impl!(u64);
distance_impl!(i64);
