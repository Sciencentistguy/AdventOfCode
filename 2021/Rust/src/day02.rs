use std::time::Instant;

use eyre::Result;

struct Instruction {
    dir: Direction,
    amount: u64,
}

#[derive(Debug, Clone, Copy)]
enum Direction {
    Up,
    Down,
    Forward,
}

fn parse(input: &str) -> Vec<Instruction> {
    input
        .lines()
        .map(|line| {
            let mut i = line.split(' ');
            let dir = match i.next() {
                Some("up") => Direction::Up,
                Some("down") => Direction::Down,
                Some("forward") => Direction::Forward,
                _ => unreachable!("invalid input"),
            };
            let amount = i
                .next()
                .and_then(|x| x.parse().ok())
                .expect("invalid input");

            debug_assert!(i.next().is_none());

            Instruction { dir, amount }
        })
        .collect()
}

fn part1(input: &[Instruction]) -> u64 {
    let mut depth = 0;
    let mut horiz = 0;

    for Instruction { dir, amount } in input {
        match dir {
            Direction::Forward => horiz += amount,
            Direction::Up => depth -= amount,
            Direction::Down => depth += amount,
        }
    }

    depth * horiz
}

fn part2(input: &[Instruction]) -> u64 {
    let mut depth = 0;
    let mut horiz = 0;
    let mut aim = 0;

    for Instruction { dir, amount } in input {
        match dir {
            Direction::Forward => {
                horiz += amount;
                depth += amount * aim;
            }
            Direction::Up => aim -= amount,
            Direction::Down => aim += amount,
        }
    }

    depth * horiz
}

pub fn run(input: String) -> Result<()> {
    let parsed = parse(input.as_str());
    let start = Instant::now();
    let part1 = part1(&parsed);
    let p1_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 02 (part 1) is {}. Took {:?}",
        part1, p1_time
    );
    let start = Instant::now();
    let part2 = part2(&parsed);
    let p2_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 02 (part 2) is {}. Took {:?}",
        part2, p2_time
    );

    Ok(())
}
