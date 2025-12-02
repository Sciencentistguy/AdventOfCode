//! Modified from https://users.rust-lang.org/t/why-is-splitting-slice-by-a-comma-so-slow/66354/9

use memchr::{Memchr, memchr};

pub struct ByteSplitImpl<'a> {
    iter: Memchr<'a>,
    slice: &'a [u8],
    position: usize,
    add_next: bool,
}

pub trait ByteSplit<'a> {
    fn byte_split(&'a self, separator: u8) -> ByteSplitImpl<'a>;
    fn byte_split_once(&'a self, separator: u8) -> Option<(&'a [u8], &'a [u8])>;
    fn byte_lines(&'a self) -> ByteSplitImpl<'a> {
        self.byte_split(b'\n')
    }
}

impl<'a> ByteSplit<'a> for &'a [u8] {
    #[inline]
    fn byte_split(&'a self, separator: u8) -> ByteSplitImpl<'a> {
        ByteSplitImpl {
            iter: memchr::memchr_iter(separator, self),
            slice: self,
            position: 0,
            add_next: true,
        }
    }

    #[inline]
    fn byte_split_once(&'a self, separator: u8) -> Option<(&'a [u8], &'a [u8])> {
        if let Some(pos) = memchr(separator, self) {
            Some((&self[..pos], &self[pos + 1..]))
        } else {
            None
        }
    }
}

impl<'a> Iterator for ByteSplitImpl<'a> {
    type Item = &'a [u8];
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next_position) = self.iter.next() {
            let slice = self.slice.get(self.position..next_position);
            self.position = next_position + 1;
            self.add_next = true;
            return slice;
        }

        // If the iterator is consumed check if the last part of the string
        // is missing to be added.
        if !self.add_next {
            None
        } else {
            // Use case for reading from last comma to end.
            let slice = self.slice.get(self.position..);
            self.position = self.slice.len();
            self.add_next = false;
            slice
        }
    }
}
