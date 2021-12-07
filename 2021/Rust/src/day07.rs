use std::time::Instant;

use eyre::Result;

fn parse(input: &str) -> Vec<u64> {
    let mut x: Vec<_> = input
        .trim()
        .split(',')
        .map(|x| x.parse().unwrap())
        .collect();
    x.sort_unstable();
    x
}

fn part1(input: &[u64]) -> u64 {
    let midpoint = input[input.len() / 2];
    input
        .iter()
        .map(|&x| x.max(midpoint) - x.min(midpoint))
        .sum()
}

fn part2(input: &[u64]) -> u64 {
    let mean = input.iter().sum::<u64>() / input.len() as u64;
    let mut min = u64::MAX;

    for p in mean - 1..=mean + 1 {
        min = min.min(
            input
                .iter()
                .map(|&x| {
                    let n = x.max(p) - x.min(p);
                    (n * (n + 1)) / 2
                })
                .sum(),
        )
    }

    min
}

pub fn run(input: String) -> Result<()> {
    let parsed = parse(input.as_str());
    let start = Instant::now();
    let part1 = part1(&parsed);
    let p1_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 07 (part 1) is {}. Took {:?}",
        part1, p1_time
    );
    let start = Instant::now();
    let part2 = part2(&parsed);
    let p2_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 07 (part 2) is {}. Took {:?}",
        part2, p2_time
    );

    Ok(())
}
