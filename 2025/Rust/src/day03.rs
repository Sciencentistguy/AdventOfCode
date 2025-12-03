use rayon::prelude::*;

type Parsed = Vec<Vec<u32>>;
type Solution = u64;

pub fn parse(input: &str) -> Parsed {
    input
        .lines()
        .map(|line| line.chars().flat_map(|c| c.to_digit(10)).collect())
        .collect()
}

fn compute_max_joltage(input: &[u32], length: usize) -> u64 {
    let mut joltage: u64 = 0;
    let mut pos = 0;
    for i in (0..length).rev() {
        // let mut best = (0, 0);
        let best = input[pos..input.len() - i]
            .iter()
            .enumerate()
            .rev()
            .max_by_key(|x| x.1)
            .unwrap();

        pos += best.0 + 1;
        joltage += (10 as u64).pow(i as u32) * *best.1 as u64;
    }
    joltage
}

pub fn part1(parsed: &Parsed) -> Solution {
    parsed
        // .par_iter()
        .iter()
        .map(|bank| compute_max_joltage(bank, 2))
        .sum()
}

pub fn part2(parsed: &Parsed) -> Solution {
    parsed
        // .par_iter()
        .iter()
        .map(|bank| compute_max_joltage(bank, 12))
        .sum()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "987654321111111
811111111111119
234234234234278
818181911112111
";

    const P1_SOLUTION: Solution = 357;
    const P2_SOLUTION: Solution = 3121910778619;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
