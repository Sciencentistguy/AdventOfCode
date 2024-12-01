use std::collections::HashMap;

use atoi::FromRadix10;


type Parsed = (Vec<u64>, Vec<u64>);
type Solution = u64;

pub fn parse(input: &str) -> Parsed {
    let lines = input.lines().collect::<Vec<_>>();
    let length = lines.len();

    let mut left = Vec::with_capacity(length);
    let mut right = Vec::with_capacity(length);

    for line in lines {
        let line = line.as_bytes();

        let (a, idx) = u64::from_radix_10(line);
        let (b, _) = u64::from_radix_10(&line[idx + 3..]);

        left.push(a);
        right.push(b);
    }

    left.sort_unstable();
    right.sort_unstable();

    (left, right)
}

pub fn part1((left, right): &Parsed) -> Solution {
    left.iter()
        .zip(right.iter())
        .map(|(a, &b)| a.abs_diff(b))
        .sum()
}

fn count(v: &[u64]) -> HashMap<u64, u64> {
    let mut counts = HashMap::with_capacity(v.len());
    for &num in v {
        *counts.entry(num).or_insert(0) += 1;
    }
    counts
}

pub fn part2((left, right): &Parsed) -> Solution {
    let counts = count(right);

    let mut counter = 0;
    for &num in left {
        counter += num * counts.get(&num).unwrap_or(&0);
    }
    counter
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "3   4
4   3
2   5
1   3
3   9
3   3";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 11);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 31);
    }
}