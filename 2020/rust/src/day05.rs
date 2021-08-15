use std::time::{Duration, Instant};

fn parse_input(input: &str) -> (Vec<(u8, u8)>, Duration) {
    let start = Instant::now();
    let res = input
        .lines()
        .map(|pass| {
            let (row, seat) = pass.split_at(7);
            let mut rowint = 0;
            for (idx, val) in row.as_bytes().iter().rev().enumerate() {
                let idx = 1u8 << idx;
                if *val == b'B' {
                    rowint |= idx
                }
            }
            let mut seatint = 0;
            for (idx, val) in seat.as_bytes().iter().rev().enumerate() {
                let idx = 1u8 << idx;
                if *val == b'R' {
                    seatint |= idx
                }
            }
            (rowint, seatint)
        })
        .collect();
    let end = Instant::now();
    (res, end - start)
}

fn solve_part1(input: &[(u8, u8)]) -> (usize, Duration) {
    let start = Instant::now();
    let res = input
        .iter()
        .map(|&(row, col)| (row as usize * 8) + col as usize)
        .max()
        .unwrap();
    let end = Instant::now();
    (res, end - start)
}

fn solve_part2(input: &[(u8, u8)]) -> (usize, Duration) {
    let start = Instant::now();
    let mut ids = input
        .iter()
        .map(|(row, col)| (*row as usize * 8) + *col as usize)
        .collect::<Vec<_>>();
    ids.sort_unstable();
    for id in 0usize.. {
        if ids.binary_search(&id).is_err()
            && ids.binary_search(&id.saturating_sub(1)).is_ok()
            && ids.binary_search(&id.saturating_add(1)).is_ok()
        {
            let end = Instant::now();
            return (id, end - start);
        }
    }
    unreachable!()
}

pub fn run(input: String) {
    let (parsed_input, time) = parse_input(&input);
    println!("Day 05, parsing input took {}ns", time.as_nanos());
    let (p1, time) = solve_part1(&parsed_input);
    println!("Day 05, part 1: {}. Took {}ns", p1, time.as_nanos());
    let (p2, time) = solve_part2(&parsed_input);
    println!("Day 05, part 2: {}. Took {}ns", p2, time.as_nanos());
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn day05_parse() {
        let input = "FBFBBFFRLR";
        assert_eq!(parse_input(input).0[0], (44, 5));
    }
}
