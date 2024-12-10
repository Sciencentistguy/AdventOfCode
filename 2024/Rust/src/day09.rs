use std::ops::Range;

type Parsed = Vec<Option<usize>>;
type Solution = usize;

pub fn parse(input: &str) -> Parsed {
    let len = input.chars().filter_map(|x| x.to_digit(10)).sum::<u32>();
    let mut ret = Vec::with_capacity(len as usize);
    let mut file_id= 0;
    let mut is_file = true;
    for b in input.trim().as_bytes() {
        let b = b - b'0';
        if is_file {
            for _ in 0..b {
                ret.push(Some(file_id as usize));
            }
            file_id += 1;
        } else {
            for _ in 0..b {
                ret.push(None);
            }
        }
        is_file = !is_file;
    }
    ret
}

trait GetMutTwice<T> {
    fn get_mut_twice(&mut self, idx1: usize, idx2: usize) -> (&mut T, &mut T);
    fn get_mut_twice_slice(&mut self, idx1: Range<usize>, idx2: Range<usize>) -> (&mut [T], &mut [T]);
}

impl<T> GetMutTwice<T> for [T] {
    fn get_mut_twice(&mut self, idx1: usize, idx2: usize) -> (&mut T, &mut T) {
        assert!(idx1 < self.len());
        assert!(idx2 < self.len());
        // SAFETY: idx1 and idx2 are guaranteed to be in bounds
        unsafe {
            let ptr = self.as_mut_ptr();
            let ptr1 = ptr.add(idx1);
            let ptr2 = ptr.add(idx2);
            (&mut *ptr1, &mut *ptr2)
        }
    }
    fn get_mut_twice_slice(&mut self, idx1: Range<usize>, idx2: Range<usize>) -> (&mut [T], &mut [T]) {
        // assert in-bounds
        assert!(idx1.end <= self.len());
        assert!(idx2.end <= self.len());
        // assert non-overlapping
        assert!(idx1.end <= idx2.start || idx2.end <= idx1.start);
        // SAFETY: idx1 and idx2 are guaranteed to be in bounds
        unsafe {
            let ptr = self.as_mut_ptr();
            let ptr1 = ptr.add(idx1.start);
            let ptr2 = ptr.add(idx2.start);
            let len1 = idx1.end - idx1.start;
            let len2 = idx2.end - idx2.start;
            (std::slice::from_raw_parts_mut(ptr1, len1), std::slice::from_raw_parts_mut(ptr2, len2))
        }
    }
}


pub fn part1(parsed: &Parsed) -> Solution {
    let mut disk = parsed.clone();
    loop {
        // find first empty 
        let first_empty_idx = disk.iter().position(|x| x.is_none()).unwrap();
        // find last non-empty
        let last_non_empty_idx = disk.iter().rposition(|x| x.is_some()).unwrap();
        // let last_non_empty = disk.iter_mut().rfind(|&x| x.is_some()).unwrap();
        // if all of first_empty..len() are empty, we're done
        if disk[first_empty_idx..].iter().all(|&x| x.is_none()) {
            return disk.iter().enumerate().filter_map(|(i, x)| x.map(|x| x * i)).sum();
            // return disk.iter().enumerate().fold(0, |acc, (x, i)| acc + x * i);
        }

        let (first_empty,last_non_empty) = disk.get_mut_twice(first_empty_idx, last_non_empty_idx);

        // swap them
        std::mem::swap(first_empty, last_non_empty);

    }
}

fn swap_ranges(a: &mut [Option<usize>], b: &mut [Option<usize>]) {
    for (a, b) in a.iter_mut().zip(b.iter_mut()) {
        std::mem::swap(a, b);
    }
}


fn find_blocks_iter<'a> (slice: &'a [Option<usize>]) -> impl Iterator<Item = (usize, usize)> + 'a {
    slice
        .iter()
        .enumerate()
        .rev()
        .scan(None, |state, (i, &item)| {
            match (state.clone(), item) {
                (None, Some(x)) => {
                    *state = Some((i, x));
                    Some(None)
                }
                (Some((start, x)), Some(y)) if x == y => Some(None),
                (Some((start, x)), _) => {
                    let block = Some((start, i - 1));
                    *state = None;
                    Some(block)
                }
                _ => Some(None),
            }
        })
        .filter_map(|x| x)
}

pub fn part2(parsed: &Parsed) -> Solution {
    // fn parse_disk_map(disk_map: &str) -> Vec<Option<usize>> {
    //     let mut disk = Vec::new();
    //     let mut is_file = true;
    //     let mut file_id = 0;

    //     for c in disk_map.chars() {
    //         let length = c.to_digit(10).unwrap() as usize;
    //         for _ in 0..length {
    //             if is_file {
    //                 disk.push(Some(file_id));
    //             } else {
    //                 disk.push(None);
    //             }
    //         }
    //         if is_file {
    //             file_id += 1;
    //         }
    //         is_file = !is_file;
    //     }

    //     disk
    // }

    fn find_free_space(disk: &[Option<usize>], length: usize) -> Option<usize> {
        let mut free_length = 0;
        for (i, &block) in disk.iter().enumerate() {
            if block.is_none() {
                free_length += 1;
                if free_length == length {
                    return Some(i + 1 - length);
                }
            } else {
                free_length = 0;
            }
        }
        None
    }

    fn move_file(disk: &mut Vec<Option<usize>>, start: usize, length: usize, free_start: usize) {
        for i in 0..length {
            disk[free_start + i] = disk[start + i];
            disk[start + i] = None;
        }
    }

    fn calculate_checksum(disk: &[Option<usize>]) -> usize {
        disk.iter()
            .enumerate()
            .filter_map(|(i, &block)| block.map(|id| i * id))
            .sum()
    }

    fn compact_disk(disk: &mut Parsed) -> usize {
        let mut files = Vec::new();
        let mut i = 0;

        while i < disk.len() {
            if let Some(file_id) = disk[i] {
                let mut length = 0;
                while i + length < disk.len() && disk[i + length] == Some(file_id) {
                    length += 1;
                }
                files.push((file_id, i, length));
                i += length;
            } else {
                i += 1;
            }
        }

        files.sort_by_key(|&(file_id, _, _)| std::cmp::Reverse(file_id));

        for (_, start, length) in files {
            if let Some(free_start) = find_free_space(&disk[..start], length) {
                move_file(disk, start, length, free_start);
            }
        }

        calculate_checksum(&disk)
    }

    let mut disk = parsed.clone();
    compact_disk(&mut disk)
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "2333133121414131402";

    const P1_SOLUTION: Solution = 1928;
    const P2_SOLUTION: Solution = 2858;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
