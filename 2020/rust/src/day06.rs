use std::time::Duration;
use std::time::Instant;

fn parse_input(input: &str) -> (Vec<Vec<&str>>, Duration) {
    let start = Instant::now();
    let collected = input
        .lines()
        .collect::<Vec<_>>()
        .split(|x| x.is_empty())
        .map(|s| s.to_vec())
        .collect();
    let end = Instant::now();
    (collected, end - start)
}

fn solve_part1(input: &[Vec<&str>]) -> (usize, Duration) {
    let start = Instant::now();
    let mut count = 0;
    for group in input {
        let mut is_there: [bool; 26] = Default::default();
        for person in group {
            for response in person.as_bytes() {
                is_there[(response - b'a') as usize] = true;
            }
        }
        count += is_there.iter().filter(|&&x| x).count();
    }
    let end = Instant::now();
    (count, end - start)
}

fn solve_part2(input: &[Vec<&str>]) -> (usize, Duration) {
    let start = Instant::now();
    let mut count = 0;
    for group in input {
        let mut responses: [usize; 26] = Default::default();
        for member in group {
            for response in member.as_bytes() {
                responses[(response - b'a') as usize] += 1;
            }
        }
        for i in responses {
            if i == group.len() {
                count += 1;
            }
        }
    }
    let end = Instant::now();
    (count, end - start)
}

pub fn run(input: String) {
    let (parsed_input, time) = parse_input(&input);
    println!("Day 06, parsing input took {}ns", time.as_nanos());
    let (p1, time) = solve_part1(&parsed_input);
    println!("Day 06, part 1: {}. Took {}ns", p1, time.as_nanos());
    let (p2, time) = solve_part2(&parsed_input);
    println!("Day 06, part 2: {}. Took {}ns", p2, time.as_nanos());
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &str = "abc

a
b
c

ab
ac

a
a
a
a

b";

    #[test]
    fn day06_part1() {
        let parsed = parse_input(INPUT).0;
        assert_eq!(solve_part1(&parsed).0, 11);
    }
    #[test]
    fn day06_part2() {
        let parsed = parse_input(INPUT).0;
        assert_eq!(solve_part2(&parsed).0, 6);
    }
}
