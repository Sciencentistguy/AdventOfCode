use std::ops::Index;

type Parsed = Vec<Vec<u64>>;
type Solution = usize;

#[derive(Clone, Copy)]
struct TwoPartSlice<'a>(&'a [u64], &'a [u64]);

impl Index<usize> for TwoPartSlice<'_> {
    type Output = u64;
    fn index(&self, index: usize) -> &Self::Output {
        if index < self.0.len() {
            &self.0[index]
        } else {
            &self.1[index - self.0.len()]
        }
    }
}

impl TwoPartSlice<'_> {
    fn len(&self) -> usize {
        self.0.len() + self.1.len()
    }
}

pub fn parse(input: &str) -> Parsed {
    input
        .lines()
        .map(|x| {
            x.split_ascii_whitespace()
                .map(|x| x.parse().unwrap())
                .collect()
        })
        .collect()
}

pub fn part1(parsed: &Parsed) -> Solution {
    let mut count = 0;
    for report in parsed {
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

fn tolerate(v: &[u64]) -> impl Iterator<Item = TwoPartSlice> {
    (0..v.len()).map(move |i| {
        let (a, b) = v.split_at(i);
        TwoPartSlice(a, &b[1..])
    })
}

pub fn part2(parsed: &Parsed) -> Solution {
    let mut count = 0;
    for report in parsed {
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
}
