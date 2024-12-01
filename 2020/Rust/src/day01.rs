use std::time::{Duration, Instant};

const TARGET: u64 = 2020;

pub fn parse_input(input: &str) -> (Vec<u64>, Duration) {
    let start = Instant::now();
    let mut v = input
        .lines()
        .map(str::parse::<u64>)
        .collect::<Result<Vec<_>, _>>()
        .expect("Input contains invalid line");
    v.sort_unstable();
    let end = Instant::now();
    (v, end - start)
}

pub fn solve_part1_bruteforce(input: &[u64]) -> (u64, Duration) {
    let start = Instant::now();
    for (c, i) in input.iter().enumerate() {
        for j in input[..c].iter() {
            if i + j == TARGET {
                let end = Instant::now();
                return (i * j, end - start);
            }
        }
    }
    unreachable!()
}

pub fn solve_part2_bruteforce(input: &[u64]) -> (u64, Duration) {
    let start = Instant::now();
    for i in input {
        for j in input {
            for k in input {
                if i + j + k == TARGET {
                    let end = Instant::now();
                    return (i * j * k, end - start);
                }
            }
        }
    }
    unreachable!()
}

pub fn run(input: String) {
    let (parsed_input, time) = parse_input(&input);
    println!("Day 01, parsing input took {}ns", time.as_nanos());
    let (p1, time) = solve_part1_bruteforce(&parsed_input);
    println!("Day 01, part 1: {}. Took {}ns", p1, time.as_nanos());
    let (p2, time) = solve_part2_bruteforce(&parsed_input);
    println!("Day 01, part 2: {}. Took {}ns", p2, time.as_nanos());
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &str = "1721
979
366
299
675
1456";

    #[test]
    fn day01_part1() {
        let parsed = parse_input(INPUT).0;
        assert_eq!(solve_part1_bruteforce(&parsed).0, 514579);
    }
    #[test]
    fn day01_part2() {
        let parsed = parse_input(INPUT).0;
        assert_eq!(solve_part2_bruteforce(&parsed).0, 241861950);
    }
}
