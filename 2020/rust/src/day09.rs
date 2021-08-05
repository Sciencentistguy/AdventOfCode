use std::{
    ops::Add,
    time::{Duration, Instant},
};

fn parse_input(input: &str) -> (Vec<usize>, Duration) {
    let start = Instant::now();
    let v = input
        .lines()
        .map(|x| x.parse().expect("Invalid input"))
        .collect();
    let end = Instant::now();
    (v, end - start)
}

fn slice_contains_pair_that_sum_to(slice: &[usize], target: usize) -> bool {
    for x in slice {
        for y in slice {
            let sum = x + y;
            if x != y && sum == target {
                return true;
            }
        }
    }
    false
}

fn solve_part1(input: &[usize]) -> (usize, Duration) {
    let start = Instant::now();
    let windows = input.windows(26);
    for window in windows {
        let (last, init) = window.split_last().unwrap();
        if !slice_contains_pair_that_sum_to(init, *last) {
            let end = Instant::now();
            return (window[25], end - start);
        }
    }
    unreachable!();
}

fn find_bounds(sums: &[usize], target: usize) -> (usize, usize) {
    let mut i = 0;
    let mut j = 1;
    loop {
        let x = sums[i];
        let y = sums[j];
        match (y - x).cmp(&target) {
            std::cmp::Ordering::Greater => {
                i += 1;
                continue;
            }
            std::cmp::Ordering::Equal => {
                return (i + 1, j + 1);
            }
            std::cmp::Ordering::Less => {
                j += 1;
                continue;
            }
        }
    }
}

fn solve_part2(input: &[usize], target: usize) -> (usize, Duration) {
    let start = Instant::now();
    let cumulatives = input
        .iter()
        .scan(0, |acc, &i| {
            *acc += i;
            Some(*acc)
        })
        .collect::<Vec<_>>();
    let (i, j) = find_bounds(&cumulatives, target);
    let xs = &input[i..j];
    let res = xs.iter().max().unwrap() + xs.iter().min().unwrap();

    let end = Instant::now();
    (res, end - start)
}

pub fn run(input: String) {
    let (parsed_input, time) = parse_input(&input);
    println!("Day 09, parsing input took {}ns", time.as_nanos());
    let (p1, time) = solve_part1(&parsed_input);
    println!("Day 09, part 1: {}. Took {}ns", p1, time.as_nanos());
    let (p2, time) = solve_part2(&parsed_input, p1);
    println!("Day 00, part 2: {}. Took {}ns", p2, time.as_nanos());
}
