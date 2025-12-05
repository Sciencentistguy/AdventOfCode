//! Modified from https://github.com/datenlord/merged_range under the terms of the Apache licence

/// Merge bound trait
mod merge;
/// `remove_n` trait
mod remove_n;

use std::{
    cmp::Ordering,
    fmt::Debug,
    ops::{Bound, RangeBounds},
};

use {merge::BoundMerge, remove_n::RemoveN};

/// Store ranges in a sorted vector, and merge overlapping ranges.
///
/// # Examples
///
/// ```
/// use std::ops::Bound;
/// use merged_range::MergedRange;
///
/// let mut mr = MergedRange::new();
/// mr.insert_range(&(0..10));
/// mr.insert((Bound::Included(5), Bound::Excluded(15)));
///
/// assert_eq!(mr, MergedRange::from_iter(vec![(Bound::Included(0), Bound::Excluded(15))]));
/// ```
#[derive(Debug, PartialEq, Eq, Default, Clone)]
pub struct MergedRange<K> {
    /// Sorted ranges
    ranges: Vec<(Bound<K>, Bound<K>)>,
}

impl<K> MergedRange<K>
where
    K: Ord + Clone,
{
    /// New `MergedRange`
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self { ranges: Vec::new() }
    }

    #[inline]
    #[must_use]
    pub fn with_capacity(n: usize) -> Self {
        Self {
            ranges: Vec::with_capacity(n),
        }
    }

    /// Insert a range that implements `RangeBounds` trait into `MergedRange`
    ///
    /// will clone data because of the `RangeBounds` just provides reference
    #[inline]
    pub fn insert_range<R>(&mut self, range: &R)
    where
        R: RangeBounds<K>,
    {
        let range = (range.start_bound().cloned(), range.end_bound().cloned());
        self.insert(range);
    }

    /// Insert `(Bound<K>, Bound<K>)` directly
    #[inline]
    pub fn insert(&mut self, mut range: (Bound<K>, Bound<K>)) {
        let mut remove_len;
        let mut remove_start;

        let (l_idx_res, r_idx_res) = (
            self.binary_search_start(&range),
            self.binary_search_end(&range),
        );

        match (l_idx_res, r_idx_res) {
            (Ok(l_idx), Ok(r_idx)) => {
                remove_len = r_idx.wrapping_add(1).wrapping_sub(l_idx);
                remove_start = l_idx;
                range.merge_start(&self.ranges[l_idx]);
                range.merge_end(&self.ranges[r_idx]);
            }
            (Ok(l_idx), Err(r_idx)) => {
                remove_len = r_idx.wrapping_sub(l_idx);
                remove_start = l_idx;
                range.merge_start(&self.ranges[l_idx]);
                if l_idx == r_idx
                    || (r_idx < self.ranges.len()
                        && match (range.end_bound(), self.ranges[r_idx].start_bound()) {
                            (Bound::Excluded(k1), Bound::Excluded(k2)) if k1 == k2 => false,
                            (
                                Bound::Excluded(k1) | Bound::Included(k1),
                                Bound::Excluded(k2) | Bound::Included(k2),
                            ) => k1 >= k2,
                            _ => false,
                        })
                {
                    range.merge_end(&self.ranges[r_idx]);
                    remove_len = remove_len.wrapping_add(1);
                }
            }
            (Err(l_idx), Ok(r_idx)) => {
                remove_len = r_idx.wrapping_add(1).wrapping_sub(l_idx);
                remove_start = l_idx;
                range.merge_end(&self.ranges[r_idx]);
                if l_idx > 0 {
                    match (
                        range.start_bound(),
                        self.ranges[l_idx.wrapping_sub(1)].end_bound(),
                    ) {
                        (Bound::Excluded(k1), Bound::Excluded(k2)) if k1 == k2 => {}
                        (
                            Bound::Excluded(k1) | Bound::Included(k1),
                            Bound::Excluded(k2) | Bound::Included(k2),
                        ) if k1 <= k2 => {
                            remove_len = remove_len.wrapping_add(1);
                            remove_start = remove_start.wrapping_sub(1);
                            range.merge_start(&self.ranges[l_idx.wrapping_sub(1)]);
                        }
                        _ => {}
                    }
                }
            }
            (Err(l_idx), Err(r_idx)) => {
                remove_len = r_idx.wrapping_sub(l_idx);
                remove_start = l_idx;
                if l_idx > 0 {
                    match (
                        range.start_bound(),
                        self.ranges[l_idx.wrapping_sub(1)].end_bound(),
                    ) {
                        (Bound::Excluded(k1), Bound::Excluded(k2)) if k1 == k2 => {}
                        (
                            Bound::Excluded(k1) | Bound::Included(k1),
                            Bound::Excluded(k2) | Bound::Included(k2),
                        ) if k1 <= k2 => {
                            remove_len = remove_len.wrapping_add(1);
                            remove_start = remove_start.wrapping_sub(1);
                            range.merge_start(&self.ranges[l_idx.wrapping_sub(1)]);
                        }
                        _ => {}
                    }
                };
                if r_idx < self.ranges.len() {
                    match (range.end_bound(), self.ranges[r_idx].start_bound()) {
                        (Bound::Excluded(k1), Bound::Excluded(k2)) if k1 == k2 => {}
                        (
                            Bound::Excluded(k1) | Bound::Included(k1),
                            Bound::Excluded(k2) | Bound::Included(k2),
                        ) if k1 >= k2 => {
                            remove_len = remove_len.wrapping_add(1);
                            range.merge_end(&self.ranges[r_idx]);
                        }
                        _ => {}
                    }
                }
            }
        }
        self.ranges.remove_n(remove_start, remove_len);
        self.ranges.insert(remove_start, range);
    }

    /// Returns `true` if `key` is contained in the range set
    #[inline]
    pub fn contains(&self, key: &K) -> bool {
        let idx = self.binary_search_start(&(Bound::Included(key), Bound::Unbounded));
        match idx {
            Ok(_) => true,
            Err(idx) => {
                if idx > 0 {
                    self.ranges[idx.wrapping_sub(1)].contains(key)
                } else {
                    false
                }
            }
        }
    }

    /// Returns `true` if `range` is contained in the range set
    #[inline]
    pub fn contains_range<R>(&self, range: &R) -> bool
    where
        R: RangeBounds<K>,
    {
        if let (Bound::Included(k1), Bound::Included(k2)) = (range.start_bound(), range.end_bound())
            && k1 == k2
        {
            return self.contains(k1);
        }

        let check_end = |l_end, r_end| match (l_end, r_end) {
            (Bound::Excluded(k1), Bound::Included(k2)) if k1 == k2 => false,
            (
                Bound::Included(k1) | Bound::Excluded(k1),
                Bound::Included(k2) | Bound::Excluded(k2),
            ) => k1 >= k2,
            (Bound::Unbounded, _) => true,
            (_, Bound::Unbounded) => false,
        };

        match self.binary_search_start(range) {
            Ok(idx) => {
                if matches!(self.ranges[idx].start_bound(), Bound::Excluded(_))
                    && matches!(range.start_bound(), Bound::Included(_))
                {
                    return false;
                }
                check_end(self.ranges[idx].end_bound(), range.end_bound())
            }
            Err(idx) => {
                if idx == 0 {
                    return false;
                }
                check_end(
                    self.ranges[idx.wrapping_sub(1)].end_bound(),
                    range.end_bound(),
                )
            }
        }
    }

    /// Search target range start in sorted ranges
    fn binary_search_start<R>(&self, target: &R) -> Result<usize, usize>
    where
        R: RangeBounds<K>,
    {
        self.ranges
            .binary_search_by(|rg| match (rg.start_bound(), target.start_bound()) {
                (Bound::Unbounded, Bound::Unbounded) => Ordering::Equal,
                (Bound::Unbounded, _) => Ordering::Less,
                (_, Bound::Unbounded) => Ordering::Greater,
                (
                    Bound::Excluded(rg_start) | Bound::Included(rg_start),
                    Bound::Excluded(range_start) | Bound::Included(range_start),
                ) => rg_start.cmp(range_start),
            })
    }

    /// Search target range end in sorted ranges
    fn binary_search_end<R>(&self, target: &R) -> Result<usize, usize>
    where
        R: RangeBounds<K>,
    {
        self.ranges
            .binary_search_by(|rg| match (rg.end_bound(), target.end_bound()) {
                (Bound::Unbounded, Bound::Unbounded) => Ordering::Equal,
                (Bound::Unbounded, _) => Ordering::Greater,
                (_, Bound::Unbounded) => Ordering::Less,
                (
                    Bound::Excluded(rg_end) | Bound::Included(rg_end),
                    Bound::Excluded(range_end) | Bound::Included(range_end),
                ) => rg_end.cmp(range_end),
            })
    }

    pub fn bounds(&self) -> &[(Bound<K>, Bound<K>)] {
        &self.ranges
    }
}

impl<K, R> FromIterator<R> for MergedRange<K>
where
    R: RangeBounds<K>,
    K: Ord + Clone,
{
    #[inline]
    fn from_iter<T: IntoIterator<Item = R>>(iter: T) -> Self {
        let iter = iter.into_iter();
        let size_hint = iter.size_hint();
        iter.fold(
            if let (_, Some(upper)) = size_hint {
                MergedRange::with_capacity(upper)
            } else {
                MergedRange::with_capacity(size_hint.0)
            },
            |mut set, range| {
                set.insert_range(&range);
                set
            },
        )
    }
}

#[allow(clippy::single_range_in_vec_init)]
#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test_empty_insert() {
        let mut set = MergedRange::new();
        set.insert_range(&(1..=1));
        assert_eq!(set.ranges, vec![(Bound::Included(1), Bound::Included(1))]);
    }

    #[test]
    fn test_case_1() {
        let mut set = MergedRange::from_iter(vec![3..6, 9..20, 25..30]);

        set.insert_range(&(3..20));
        assert_eq!(set, MergedRange::from_iter(vec![3..20, 25..30]));

        set.insert_range(&(3..30));
        assert_eq!(set, MergedRange::from_iter(vec![3..30]));
    }

    #[test]
    fn test_case_2() {
        let mut set = MergedRange::from_iter(vec![3..6, 9..20, 25..30]);

        set.insert_range(&(3..5));
        assert_eq!(set, MergedRange::from_iter(vec![3..6, 9..20, 25..30]),);

        set.insert_range(&(9..25));
        assert_eq!(set, MergedRange::from_iter(vec![3..6, 9..30]));
    }

    #[test]
    fn test_case_3() {
        let mut set = MergedRange::from_iter(vec![3..6, 9..20, 25..30]);

        set.insert_range(&(1..6));
        assert_eq!(set, MergedRange::from_iter(vec![1..6, 9..20, 25..30]));

        set.insert_range(&(3..20));
        assert_eq!(set, MergedRange::from_iter(vec![1..20, 25..30]));

        set.insert_range(&(23..30));
        assert_eq!(set, MergedRange::from_iter(vec![1..20, 23..30]));
    }

    #[test]
    fn test_case_4() {
        let mut set = MergedRange::from_iter(vec![3..6, 9..20, 25..30]);

        set.insert_range(&(2..7));
        assert_eq!(set, MergedRange::from_iter(vec![2..7, 9..20, 25..30]));

        set.insert_range(&(3..18));
        assert_eq!(set, MergedRange::from_iter(vec![2..20, 25..30]));

        set.insert_range(&(1..35));
        assert_eq!(set, MergedRange::from_iter(vec![1..35]));
    }

    #[test]
    fn test_unbonded() {
        let mut set = MergedRange::from_iter(vec![3..6, 9..20, 25..30]);

        set.insert_range(&(10..));
        assert_eq!(set, {
            MergedRange {
                ranges: vec![
                    (Bound::Included(3), Bound::Excluded(6)),
                    (Bound::Included(9), Bound::Unbounded),
                ],
            }
        });

        set.insert_range(&(..4));
        assert_eq!(set, {
            MergedRange {
                ranges: vec![
                    (Bound::Unbounded, Bound::Excluded(6)),
                    (Bound::Included(9), Bound::Unbounded),
                ],
            }
        });

        set.insert_range(&(..));
        assert_eq!(set, {
            MergedRange {
                ranges: vec![(Bound::Unbounded, Bound::Unbounded)],
            }
        });
    }

    #[test]
    fn test_excluded() {
        let mut set = MergedRange::from_iter(vec![
            (Bound::Excluded(3), Bound::Excluded(6)),
            (Bound::Excluded(9), Bound::Excluded(20)),
            (Bound::Excluded(25), Bound::Excluded(30)),
            (Bound::Excluded(50), Bound::Excluded(59)),
        ]);

        set.insert_range(&(Bound::Excluded(3), Bound::Excluded(9)));
        assert_eq!(
            set,
            MergedRange::from_iter(vec![
                (Bound::Excluded(3), Bound::Excluded(9)),
                (Bound::Excluded(9), Bound::Excluded(20)),
                (Bound::Excluded(25), Bound::Excluded(30)),
                (Bound::Excluded(50), Bound::Excluded(59)),
            ])
        );

        set.insert_range(&(Bound::Excluded(20), Bound::Excluded(30)));
        assert_eq!(
            set,
            MergedRange::from_iter(vec![
                (Bound::Excluded(3), Bound::Excluded(9)),
                (Bound::Excluded(9), Bound::Excluded(20)),
                (Bound::Excluded(20), Bound::Excluded(30)),
                (Bound::Excluded(50), Bound::Excluded(59)),
            ])
        );

        set.insert_range(&(Bound::Excluded(30), Bound::Excluded(50)));
        assert_eq!(
            set,
            MergedRange::from_iter(vec![
                (Bound::Excluded(3), Bound::Excluded(9)),
                (Bound::Excluded(9), Bound::Excluded(20)),
                (Bound::Excluded(20), Bound::Excluded(30)),
                (Bound::Excluded(30), Bound::Excluded(50)),
                (Bound::Excluded(50), Bound::Excluded(59)),
            ])
        );
    }

    #[test]
    fn test_contains_one_key() {
        let set = MergedRange::from_iter(vec![3..6, 9..20, 25..30]);
        assert!(set.contains(&5));
        assert!(!set.contains(&8));
    }

    #[test]
    fn test_contains_range() {
        let mut set = MergedRange::from_iter(vec![3..6, 9..20, 25..30]);
        set.insert_range(&(40..));

        assert!(set.contains_range(&(10..15)));
        assert!(!set.contains_range(&(5..7)));
        assert!(!set.contains_range(&(18..23)));
        assert!(set.contains_range(&(50..)));
        assert!(!set.contains_range(&(..1)));
        assert!(!set.contains_range(&(..)));
    }
}
