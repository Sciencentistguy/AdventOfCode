use std::ptr;

/// Remove n elements from the index of the slice
pub(crate) trait RemoveN {
    /// Remove n elements from the index of the slice
    ///
    /// # Panics
    ///
    /// Panics if removing n elements from the index of the slice would overflow
    fn remove_n(&mut self, index: usize, n: usize);
}

impl<T> RemoveN for Vec<T> {
    fn remove_n(&mut self, index: usize, n: usize) {
        if n == 0 {
            return;
        }
        let len = self.len();
        assert!(
            index.wrapping_add(n).wrapping_sub(1) < len,
            "removed elements are out of range"
        );
        unsafe {
            let ptr = self.as_mut_ptr().add(index);
            ptr::copy(ptr.add(n), ptr, len.wrapping_sub(index).wrapping_sub(n));
            self.set_len(len.wrapping_sub(n));
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_remove_n() {
        let mut v = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];
        v.remove_n(3, 2);
        assert_eq!(v, vec![1, 2, 3, 6, 7, 8, 9, 10]);
        assert_eq!(v.len(), 8);
    }

    #[test]
    #[should_panic]
    fn test_remove_n_panic() {
        let mut v = vec![1, 2, 3, 4, 5];
        v.remove_n(3, 4);
    }
}
