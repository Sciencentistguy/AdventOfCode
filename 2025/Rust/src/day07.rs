use std::collections::{HashMap, HashSet};

use memchr::{memchr, memchr_iter};

type Parsed = Vec<Vec<usize>>;
type Solution = u64;

pub fn parse(input: &str) -> Parsed {
    let mut ret = Vec::with_capacity(memchr_iter(b'\n', input.as_bytes()).count());

    let start = memchr(b'S', input.as_bytes()).unwrap();
    ret.push(vec![start]);

    ret.extend(
        input
            .trim()
            .lines()
            .skip(1)
            .map(|line| memchr_iter(b'^', line.as_bytes()).collect()),
    );

    ret
}

pub fn part1(rows: &Parsed) -> Solution {
    let mut split_count = 0;
    let mut cols = HashSet::new();
    cols.insert(*rows[0].first().unwrap());
    for row in &rows[1..] {
        for col in cols.clone() {
            if row.binary_search(&col).is_ok() {
                cols.remove(&col);
                cols.insert(col + 1);
                cols.insert(col - 1);
                split_count += 1;
            }
        }
    }

    split_count
}

pub fn part2(rows: &Parsed) -> Solution {
    let mut counts = HashMap::new();
    let mut next_counts = HashMap::new();

    if let Some(&start_col) = rows[0].first() {
        counts.insert(start_col, 1);
    }

    for row_splitters in &rows[1..] {
        next_counts.clear();
        for (&col, &count) in &counts {
            if row_splitters.binary_search(&col).is_ok() {
                *next_counts.entry(col - 1).or_default() += count;
                *next_counts.entry(col + 1).or_default() += count;
            } else {
                *next_counts.entry(col).or_default() += count;
            }
        }

        std::mem::swap(&mut counts, &mut next_counts);
    }

    counts.values().sum::<u64>()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............
";

    const P1_SOLUTION: Solution = 21;
    const P2_SOLUTION: Solution = 40;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
