use std::{hint::unreachable_unchecked, time::Instant};

use eyre::{Context, Result};

fn parse(input: &str) -> Result<Vec<u64>> {
    input
        .lines()
        .map(|x| x.parse().wrap_err("Failed to parse input"))
        .collect::<Result<Vec<_>, _>>()
}

fn part1(input: &[u64]) -> u64 {
    let mut c = 0;
    for w in input.windows(2) {
        if let [a, b] = w {
            if a < b {
                c += 1;
            }
        } else {
            unsafe { unreachable_unchecked() }
        }
    }
    c
}

fn part2(input: &[u64]) -> u64 {
    let mut c = 0;
    for w in input.windows(4) {
        if let [a, _, _, b] = w {
            if a < b {
                c += 1;
            }
        } else {
            unsafe { unreachable_unchecked() }
        }
    }
    c
}

pub fn run(input: String) -> Result<()> {
    let parsed = parse(input.as_str())?;
    let start = Instant::now();
    let part1 = part1(&parsed);
    let p1_time = Instant::now() - start;
    println!("The solution to 2021 day 01 (part 1) is {}. Took {:?}", part1, p1_time);
    let start = Instant::now();
    let part2 = part2(&parsed);
    let p2_time = Instant::now() - start;
    println!("The solution to 2021 day 01 (part 2) is {}. Took {:?}", part2, p2_time);

    Ok(())
}
