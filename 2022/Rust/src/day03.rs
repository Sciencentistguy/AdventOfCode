use itertools::Itertools;

pub fn parse(input: &str) -> Vec<&str> {
    input.lines().collect()
}

fn priority(x: u8) -> usize {
    match x {
        b'a'..=b'z' => (x - 96) as _,
        b'A'..=b'Z' => (x - 38) as _,
        _ => unreachable!(),
    }
}

pub fn part1(parsed: &[&str]) -> usize {
    parsed
        .iter()
        .map(|line| {
            let mid = line.len() / 2;
            line.split_at(mid)
        })
        .map(|(first, second)| {
            first
                .bytes()
                .filter(|c| second.as_bytes().contains(c))
                .unique()
                .map(priority)
                .sum::<usize>()
        })
        .sum()
}

pub fn part2(parsed: &[&str]) -> usize {
    parsed
        .iter()
        .array_chunks::<3>()
        .map(|[a, b, c]| {
            priority(
                a.bytes()
                    .find(|chr| b.contains(*chr as char) && c.contains(*chr as char))
                    .unwrap(),
            )
        })
        .sum()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw
";

    #[test]
    fn test_part1() {
        let parsed = parse(INPUT);
        assert_eq!(157, part1(&parsed));
    }

    #[test]
    fn test_part2() {
        let parsed = parse(INPUT);
        assert_eq!(70, part2(&parsed));
    }
}
