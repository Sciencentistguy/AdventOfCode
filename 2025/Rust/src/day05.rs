use atoi::FromRadix10;
use std::ops::Bound;

use common::{ByteSplit, MergedRange};

type Parsed = (MergedRange<u64>, Vec<u64>);
type Solution = u64;

pub fn parse(input: &str) -> Parsed {
    let (ranges, values) = input.trim().split_once("\n\n").unwrap();

    let ranges = ranges
        .as_bytes()
        .byte_lines()
        .map(|line| {
            let (from, to) = line.byte_split_once(b'-').unwrap();
            let from = u64::from_radix_10(from).0;
            let to = u64::from_radix_10(to).0;
            from..=to
        })
        .collect::<MergedRange<_>>();

    let values = values
        .lines()
        .map(|line| u64::from_radix_10(line.as_bytes()).0)
        .collect();

    (ranges, values)
}

pub fn part1((ranges, values): &Parsed) -> Solution {
    values.iter().filter(|val| ranges.contains(val)).count() as _
}

pub fn part2((ranges, _): &Parsed) -> Solution {
    let mut count = 0;
    for (lower, upper) in ranges.bounds() {
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
