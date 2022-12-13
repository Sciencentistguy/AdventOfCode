use tap::Tap;

pub fn parse(inpt: &str) -> Vec<u64> {
    inpt.split("\n\n")
        .map(|s| s.lines().map(|s| s.parse::<u64>().unwrap()).sum())
        .collect::<Vec<_>>()
        .tap_mut(|x| x.sort_unstable())
}

pub fn part1(parsed: &[u64]) -> u64 {
    *parsed.last().unwrap()
}

pub fn part2(parsed: &[u64]) -> u64 {
    parsed.iter().rev().take(3).sum()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 24000);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 45000);
    }
}
