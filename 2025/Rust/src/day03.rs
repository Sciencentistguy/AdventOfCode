use rayon::prelude::*;

type Parsed = Vec<Vec<u8>>;
type Solution = u64;

pub fn parse(input: &str) -> Parsed {
    input
        .trim()
        .lines()
        .map(|line| line.bytes().map(|c| c - b'0').collect())
        .collect()
}

fn simd_max(input: &[u8]) -> (usize, u8) {
    use std::simd::prelude::*;

    const LANES: usize = 32;
    type Sv = Simd<u8, LANES>;

    let mut best = (0, 0);
    let (chunks, remainder) = input.as_chunks::<LANES>();

    let mut index = 0;
    let mut max_values = Sv::splat(0);
    let mut max_indices = Sv::splat(0);

    for chunk in chunks {
        let values = Sv::from_array(*chunk);
        let indices = {
            let mut arr = [0u8; LANES];
            for i in 0..LANES {
                arr[i] = (index + i) as u8;
            }
            Sv::from_array(arr)
        };

        let mask = values.simd_gt(max_values);
        max_values = mask.select(values, max_values);
        max_indices = mask.select(indices, max_indices);

        index += 16;
    }

    for i in 0..16 {
        if max_values[i] > best.1 {
            best = (max_indices[i] as usize, max_values[i]);
        }
    }
    for (i, &value) in remainder.iter().enumerate() {
        if value > best.1 {
            best = (index + i, value);
        }
    }
    best
}

fn compute_max_joltage(input: &[u8], length: usize) -> u64 {
    let mut joltage: u64 = 0;
    let mut pos = 0;
    for i in (0..length).rev() {
        let (idx, val) = simd_max(&input[pos..input.len() - i]);

        pos += idx + 1;
        joltage += (10 as u64).pow(i as u32) * val as u64;
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
