use tap::Tap;

fn parse(inpt: &str) -> Vec<u64> {
    inpt.split("\n\n")
        .map(|s| s.lines().map(|s| s.parse::<u64>().unwrap()).sum())
        .collect::<Vec<_>>()
        .tap_mut(|x| x.sort_unstable())
}

fn part1(parsed: &[u64]) -> u64 {
    *parsed.last().unwrap()
}

fn part2(parsed: &[u64]) -> u64 {
    parsed.iter().rev().take(3).sum()
}

pub fn run(input: String) {
    let parsed = parse(&input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}
