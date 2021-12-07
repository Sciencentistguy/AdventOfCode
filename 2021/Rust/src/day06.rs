use std::time::Instant;

use eyre::Result;

fn parse(input: &str) -> Vec<u64> {
    input
        .trim()
        .split(',')
        .map(|x| x.parse().unwrap())
        .collect()
}

fn total_fish_after(input: &[u64], days: u64) -> u64 {
    let mut now = [0; 9];
    for fish in input {
        now[(*fish) as usize] += 1;
    }
    for _ in 0..days {
        let mut tomorrow = [0; 9];
        for (timer, count) in now.iter().enumerate() {
            if timer == 0 {
                tomorrow[6] += count;
                tomorrow[8] += count;
            } else {
                tomorrow[timer - 1] += count;
            }
        }
        now = tomorrow;
    }

    now.iter().sum()
}

fn part1(input: &[u64]) -> u64 {
    total_fish_after(input, 80)
}

fn part2(input: &[u64]) -> u64 {
    total_fish_after(input, 256)
}

pub fn run(input: String) -> Result<()> {
    let parsed = parse(input.as_str());
    let start = Instant::now();
    let part1 = part1(&parsed);
    let p1_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 06 (part 1) is {}. Took {:?}",
        part1, p1_time
    );
    let start = Instant::now();
    let part2 = part2(&parsed);
    let p2_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 06 (part 2) is {}. Took {:?}",
        part2, p2_time
    );

    Ok(())
}
