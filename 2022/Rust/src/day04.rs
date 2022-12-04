use std::{convert::Infallible, str::FromStr};

use itertools::Itertools;

pub struct Range {
    start: usize,
    end: usize,
}

impl Range {
    const fn contains(&self, other: &Range) -> bool {
        self.start <= other.start && self.end >= other.end
    }

    const fn overlaps(&self, other: &Range) -> bool {
        self.start <= other.end && self.end >= other.start
    }
}

impl FromStr for Range {
    type Err = Infallible; // Panic !

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (a, b) = s.split('-').collect_tuple().unwrap();
        Ok(Range {
            start: a.parse().unwrap(),
            end: b.parse().unwrap(),
        })
    }
}

pub fn parse(input: &str) -> Vec<(Range, Range)> {
    input
        .lines()
        .map(|x| {
            let (first, second) = x.split(',').collect_tuple().unwrap();

            (
                Range::from_str(first).unwrap(),
                Range::from_str(second).unwrap(),
            )
        })
        .collect()
}

pub fn part1(parsed: &[(Range, Range)]) -> usize {
    parsed
        .iter()
        .filter(|(a, b)| a.contains(b) || b.contains(a))
        .count()
}

pub fn part2(parsed: &[(Range, Range)]) -> usize {
    parsed
        .iter()
        .filter(|(a, b)| a.overlaps(b) || b.overlaps(a))
        .count()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8
";

    #[test]
    fn test_part1() {
        let parsed = parse(INPUT);
        assert_eq!(2, part1(&parsed));
    }

    #[test]
    fn test_part2() {
        let parsed = parse(INPUT);
        assert_eq!(4, part2(&parsed));
    }
}
