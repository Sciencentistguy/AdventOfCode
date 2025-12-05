use std::ops::{Bound, RangeInclusive};

use common::MergedRange;

type Parsed = (Vec<RangeInclusive<u64>>, Vec<u64>);
type Solution = u64;

pub fn parse(input: &str) -> Parsed {
    let (ranges, values) = input.trim().split_once("\n\n").unwrap();

    let ranges = ranges
        .lines()
        .map(|line| {
            let (from, to) = line.split_once('-').unwrap();
            let from = from.parse().unwrap();
            let to = to.parse().unwrap();
            from..=to
        })
        .collect();

    let values = values.lines().map(|line| line.parse().unwrap()).collect();

    (ranges, values)
}

pub fn part1((ranges, values): &Parsed) -> Solution {
    values
        .iter()
        .filter(|val| ranges.iter().any(|x| x.contains(val)))
        .count() as _
}

pub fn part2((ranges, _): &Parsed) -> Solution {
    let mut mr = MergedRange::new();

    for rng in ranges {
        mr.insert_range(rng);
    }

    let mut count = 0;
    for (lower, upper) in mr.bounds() {
        let upper = match upper {
            Bound::Included(n) => n + 1,
            Bound::Excluded(n) => *n,
            _ => unreachable!(),
        };
        let lower = match lower {
            Bound::Included(n) => *n,
            Bound::Excluded(n) => n + 1,
            _ => unreachable!(),
        };
        count += upper - lower;
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

    const INPUT: &str = "3-5
10-14
16-20
12-18

1
5
8
11
17
32
";

    const P1_SOLUTION: Solution = 3;
    const P2_SOLUTION: Solution = 14;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
