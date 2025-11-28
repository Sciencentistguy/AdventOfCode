use std::ops::Index;

type Parsed = TwoDimensionalVec<u64>;
type Solution = usize;

#[derive(Clone, Copy)]
struct TwoPartSlice<'a, T>(&'a [T], &'a [T]);

impl<T> Index<usize> for TwoPartSlice<'_, T> {
    type Output = T;
    fn index(&self, index: usize) -> &Self::Output {
        if index < self.0.len() {
            &self.0[index]
        } else {
            &self.1[index - self.0.len()]
        }
    }
}

impl<T> TwoPartSlice<'_, T> {
    fn len(&self) -> usize {
        self.0.len() + self.1.len()
    }
}

pub struct TwoDimensionalVec<T> {
    storage: Vec<T>,
    row_lengths: Vec<usize>,
}

impl<T> TwoDimensionalVec<T> {
    fn new() -> Self {
        Self {
            storage: Vec::new(),
            row_lengths: Vec::new(),
        }
    }

    #[allow(dead_code)] // used in test
    fn push_row(&mut self, row: &[T])
    where
        T: Clone,
    {
        self.row_lengths.push(row.len());
        self.storage.extend_from_slice(row);
    }

    fn extend_row(&mut self, row: impl Iterator<Item = T>)
    where
        T: Clone,
    {
        self.row_lengths.push(0);

        for item in row {
            self.storage.push(item);
            *self.row_lengths.last_mut().unwrap() += 1;
        }
    }

    fn row_iter(&self) -> impl Iterator<Item = &[T]> {
        self.row_lengths.iter().scan(0, |start, &len| {
            let end = *start + len;
            let slice = &self.storage[*start..end];
            *start = end;
            Some(slice)
        })
    }
}

pub fn parse(input: &str) -> Parsed {
    let mut v = TwoDimensionalVec::new();
    for line in input.lines() {
        v.extend_row(line.split_ascii_whitespace().map(|x| x.parse().unwrap()));
    }
    v
}

pub fn part1(parsed: &Parsed) -> Solution {
    let mut count = 0;
    for report in parsed.row_iter() {
        let mut desc = true;
        let mut inc = true;
        let mut gradual = true;

        for i in 1..report.len() {
            desc &= report[i] <= report[i - 1];
            inc &= report[i] >= report[i - 1];
            gradual &= (1..=3).contains(&report[i].abs_diff(report[i - 1]));
        }

        if (desc || inc) && gradual {
            count += 1;
        }
    }
    count
}

fn tolerate(v: &[u64]) -> impl Iterator<Item = TwoPartSlice<'_, u64>> {
    (0..v.len()).map(move |i| {
        let (a, b) = v.split_at(i);
        TwoPartSlice(a, &b[1..])
    })
}

pub fn part2(parsed: &Parsed) -> Solution {
    let mut count = 0;
    for report in parsed.row_iter() {
        for report in tolerate(report) {
            let mut desc = true;
            let mut inc = true;
            let mut gradual = true;

            for i in 1..report.len() {
                desc &= report[i] <= report[i - 1];
                inc &= report[i] >= report[i - 1];
                gradual &= (1..=3).contains(&report[i].abs_diff(report[i - 1]));
            }

            if (desc || inc) && gradual {
                count += 1;
                break;
            }
        }
    }
    count
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9
";

    const P1_SOLUTION: Solution = 2;
    const P2_SOLUTION: Solution = 4;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }

    #[test]
    fn test_two_dimensional_vec() {
        let mut v = TwoDimensionalVec::new();
        v.push_row(&[1, 2, 3]);
        v.push_row(&[4, 5, 6, 7]);
        v.push_row(&[8, 9]);

        let mut it = v.row_iter();
        assert_eq!(it.next(), Some(&[1, 2, 3] as _));
        assert_eq!(it.next(), Some(&[4, 5, 6, 7] as _));
        assert_eq!(it.next(), Some(&[8, 9] as _));
        assert_eq!(it.next(), None);
    }
}
