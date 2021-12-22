use std::{collections::HashMap, time::Instant};

use eyre::Result;

struct Input {
    pairs: HashMap<(char, char), u64>,
    rules: HashMap<(char, char), char>,
    last_char: char,
}

fn parse(input: &str) -> Input {
    let mut lines = input.lines();
    let starting_point: Vec<_> = lines.next().unwrap().trim().chars().collect();
    let mut pairs = HashMap::new();

    for &[a, b] in starting_point.array_windows() {
        *pairs.entry((a, b)).or_insert(0) += 1;
    }

    let mut rules = HashMap::new();
    for line in lines.skip(1) {
        let line = line.as_bytes();
        let from = (line[0] as char, line[1] as char);
        let to = line[6] as char;
        rules.insert(from, to);
    }

    Input {
        pairs,
        rules,
        last_char: *starting_point.last().unwrap(),
    }
}

#[inline]
fn common_solution(
    Input {
        pairs,
        rules,
        last_char,
    }: &Input,
    n: usize,
) -> u64 {
    let mut polymer = pairs.clone();
    let mut next = HashMap::new();

    for _ in 0..n {
        for (pair @ &(first, second), count) in &polymer {
            let new_element = rules[pair];

            *next.entry((first, new_element)).or_insert(0) += count;
            *next.entry((new_element, second)).or_insert(0) += count;
        }
        polymer.clear();
        polymer.extend(next.drain());
    }

    let mut counts = HashMap::new();
    for ((letter, _), count) in &polymer {
        *counts.entry(letter).or_default() += count;
    }
    *counts.entry(last_char).or_default() += 1u64;

    let most: u64 = *counts.values().max().unwrap();
    let least: u64 = *counts.values().min().unwrap();

    most - least
}

fn part1(input: &Input) -> u64 {
    common_solution(input, 10)
}

fn part2(input: &Input) -> u64 {
    common_solution(input, 40)
}

pub fn run(input: String) -> Result<()> {
    let parsed = parse(input.as_str());
    let start = Instant::now();
    let part1 = part1(&parsed);
    let p1_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 14 (part 1) is {}. Took {:?}",
        part1, p1_time
    );
    let start = Instant::now();
    let part2 = part2(&parsed);
    let p2_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 14 (part 2) is {}. Took {:?}",
        part2, p2_time
    );

    Ok(())
}
