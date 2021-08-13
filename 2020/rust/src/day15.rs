use std::{
    collections::HashMap,
    time::{Duration, Instant},
};

fn parse_input(input: &str) -> (Vec<usize>, Duration) {
    let start = Instant::now();
    let out = input.split(',').map(|x| x.parse().ok()).flatten().collect();
    let end = Instant::now();
    (out, end - start)
}

fn solve_common(input: &[usize], limit: usize) -> usize {
    let mut input = input.to_vec();
    input.reserve(limit);
    let mut previous_occurence = HashMap::new();
    for (c, i) in input.iter().enumerate() {
        previous_occurence.insert(*i, c);
    }
    previous_occurence.remove(input.last().unwrap());

    let mut index = input.len();
    while index < limit {
        let num = *input.last().unwrap();
        if let Some(x) = previous_occurence.get(&num) {
            let new_num = index - x - 1;
            input.push(new_num);
            previous_occurence.insert(num, index - 1);
        } else {
            input.push(0);
            previous_occurence.insert(num, index - 1);
        }
        index += 1;
    }
    input[limit - 1]
}

fn solve_part1(input: &[usize]) -> (usize, Duration) {
    let start = Instant::now();
    let ret = solve_common(input, 2020);
    let end = Instant::now();
    (ret, end - start)
}

fn solve_part2(input: &[usize]) -> (usize, Duration) {
    let start = Instant::now();
    let ret = solve_common(input, 30_000_000);
    let end = Instant::now();
    (ret, end - start)
}

pub fn run(input: String) {
    let (parsed_input, time) = parse_input(&input);
    println!("Day 15, parsing input took {}ns", time.as_nanos());
    let (p1, time) = solve_part1(&parsed_input);
    println!("Day 15, part 1: {}. Took {}ns", p1, time.as_nanos());
    let (p2, time) = solve_part2(&parsed_input);
    println!("Day 15, part 2: {}. Took {}ns", p2, time.as_nanos());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn day15_part1() {
        assert_eq!(solve_part1(&[1, 3, 2]).0, 1);
        assert_eq!(solve_part1(&[2, 1, 3]).0, 10);
        assert_eq!(solve_part1(&[1, 2, 3]).0, 27);
        assert_eq!(solve_part1(&[2, 3, 1]).0, 78);
        assert_eq!(solve_part1(&[3, 2, 1]).0, 438);
        assert_eq!(solve_part1(&[3, 1, 2]).0, 1836);
    }
}
