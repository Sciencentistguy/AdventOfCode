use itertools::Itertools;

pub fn parse(input: &mut str) -> Vec<(&str, &str)> {
    assert!(input.is_ascii());
    // Safety: input is ascii, moving bytes around is Fine™
    unsafe {
        for line in input.as_bytes_mut().split_mut(|&x| x == b'\n') {
            let mid = line.len() / 2;
            let (left, right) = line.split_at_mut(mid);
            left.sort_unstable();
            right.sort_unstable();
        }
    }

    input
        .lines()
        .map(|line| {
            let mid = line.len() / 2;
            line.split_at(mid)
        })
        .collect()
}

fn priority(x: u8) -> usize {
    match x {
        b'a'..=b'z' => (x - 96) as _,
        b'A'..=b'Z' => (x - 38) as _,
        _ => unreachable!(),
    }
}

pub fn part1(parsed: &[(&str, &str)]) -> usize {
    parsed
        .iter()
        .map(|(first, second)| {
            first
                .bytes()
                .filter(|c| second.as_bytes().binary_search(c).is_ok())
                .unique()
                .map(priority)
                .sum::<usize>()
        })
        .sum()
}

pub fn part2(parsed: &[(&str, &str)]) -> usize {
    parsed
        .iter()
        .array_chunks::<3>()
        .map(|[a, b, c]| {
            priority(
                a.0.bytes()
                    .find(|chr| {
                        (b.0.as_bytes().binary_search(chr).is_ok()
                            || b.1.as_bytes().binary_search(chr).is_ok())
                            && (c.0.as_bytes().binary_search(chr).is_ok()
                                || c.1.as_bytes().binary_search(chr).is_ok())
                    })
                    .or_else(|| {
                        a.1.bytes().find(|chr| {
                            (b.0.as_bytes().binary_search(chr).is_ok()
                                || b.1.as_bytes().binary_search(chr).is_ok())
                                && (c.0.as_bytes().binary_search(chr).is_ok()
                                    || c.1.as_bytes().binary_search(chr).is_ok())
                        })
                    })
                    .unwrap(),
            )
        })
        .sum()
}

pub fn run(input: &str) {
    let mut input = input.to_owned();
    let parsed = parse(&mut input);
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
        let mut input = INPUT.to_owned();
        let parsed = parse(&mut input);
        assert_eq!(157, part1(&parsed));
    }

    #[test]
    fn test_part2() {
        let mut input = INPUT.to_owned();
        let parsed = parse(&mut input);
        assert_eq!(70, part2(&parsed));
    }
}
