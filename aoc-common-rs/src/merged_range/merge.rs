use std::ops::Bound;

/// Merge two overlapping ranges.
pub(crate) trait BoundMerge<Rhs = Self> {
    /// Merge start bound
    fn merge_start(&mut self, rhs: &Rhs);
    /// Merge end bound
    fn merge_end(&mut self, rhs: &Rhs);
}

impl<K> BoundMerge for Bound<K>
where
    K: Clone + Ord,
{
    fn merge_start(&mut self, rhs: &Self) {
        // If rhs is less than self, new_val will be Some(rhs), otherwise will be None
        let new_val = match *self {
            Bound::Unbounded => None,
            _ if rhs == &Bound::Unbounded => Some(Bound::Unbounded),
            Bound::Excluded(ref k1) | Bound::Included(ref k1) => {
                if let &(Bound::Excluded(ref k2) | Bound::Included(ref k2)) = rhs {
                    (k2 < k1).then(|| rhs.clone())
                } else {
                    None
                }
            }
        };
        if let Some(val) = new_val {
            *self = val;
        }
    }

    fn merge_end(&mut self, rhs: &Self) {
        // If rhs is greater than self, new_val will be Some(rhs), otherwise will be None
        let new_val = match *self {
            Bound::Unbounded => None,
            _ if rhs == &Bound::Unbounded => Some(Bound::Unbounded),
            Bound::Excluded(ref k1) | Bound::Included(ref k1) => {
                if let &(Bound::Excluded(ref k2) | Bound::Included(ref k2)) = rhs {
                    (k2 > k1).then(|| rhs.clone())
                } else {
                    None
                }
            }
        };
        if let Some(val) = new_val {
            *self = val;
        }
    }
}

impl<K> BoundMerge for (Bound<K>, Bound<K>)
where
    K: Clone + Ord,
{
    fn merge_start(&mut self, rhs: &Self) {
        self.0.merge_start(&rhs.0);
    }

    fn merge_end(&mut self, rhs: &Self) {
        self.1.merge_end(&rhs.1);
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_bound_merge() {
        let mut tmp1 = Bound::Unbounded;
        tmp1.merge_start(&Bound::Excluded(1));
        assert_eq!(tmp1, Bound::Unbounded);

        let mut tmp2 = Bound::Excluded(10);
        tmp2.merge_start(&Bound::Excluded(1));
        assert_eq!(tmp2, Bound::Excluded(1));

        let mut tmp3 = Bound::Excluded(10);
        tmp3.merge_start(&Bound::Unbounded);
        assert_eq!(tmp3, Bound::Unbounded);

        let mut tmp4 = Bound::Excluded(10);
        tmp4.merge_end(&Bound::Excluded(1));
        assert_eq!(tmp4, Bound::Excluded(10));

        let mut tmp5 = Bound::Excluded(10);
        tmp5.merge_end(&Bound::Unbounded);
        assert_eq!(tmp5, Bound::Unbounded);
    }
}
