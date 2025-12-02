use std::hint::unreachable_unchecked;

type Parsed = Vec<i16>;
type Solution = i16;

pub fn parse(input: &str) -> Parsed {
    input
        .trim()
        .bytes()
        .map(|chr| match chr {
            b'(' => 1,
            b')' => -1,
            _ => unsafe { unreachable_unchecked() },
        })
        .collect()
}

pub fn part1(parsed: &Parsed) -> Solution {
    parsed.iter().sum()
}

pub fn part2(parsed: &Parsed) -> Solution {
    let mut count = 0;
    let mut floor = 0;
    for r#move in parsed {
        floor += r#move;
        count += 1;
        if floor == -1 {
            return count;
        }
    }
    unreachable!()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}
