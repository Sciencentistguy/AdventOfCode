use std::{hint::unreachable_unchecked, time::Instant};

use eyre::Result;
use multimap::MultiMap;

type Input<'a> = MultiMap<Cave<'a>, Cave<'a>>;

#[derive(Hash, PartialEq, Eq, Clone, Copy, Debug)]
enum Cave<'a> {
    Start,
    End,
    Small(&'a str),
    Large(&'a str),
}

impl<'a> From<&'a str> for Cave<'a> {
    fn from(s: &'a str) -> Self {
        match s {
            "start" => Self::Start,
            "end" => Self::End,
            _ if s.chars().all(|c| c.is_lowercase()) => Self::Small(s),
            _ => Self::Large(s),
        }
    }
}

impl Cave<'_> {
    fn is_small(&self) -> bool {
        !matches!(self, Self::Large(_))
    }
}

fn parse(input: &str) -> Input {
    let mut out = MultiMap::new();
    for line in input.lines() {
        let line = line.trim();
        let s = line.split('-').collect::<Vec<_>>();
        out.insert(s[0].into(), s[1].into());
        out.insert(s[1].into(), s[0].into());
    }
    out
}

fn part1(input: &Input) -> usize {
    #[inline]
    fn is_valid(visited_caves: &[Cave], current_cave: Cave) -> bool {
        !current_cave.is_small() || !visited_caves.contains(&current_cave)
    }

    get_paths_recursive(input, Cave::Start, &Vec::new(), is_valid)
}

fn part2(input: &Input) -> usize {
    #[inline]
    fn is_valid(visited_caves: &[Cave], current_cave: Cave) -> bool {
        visited_caves
            .iter()
            .filter(|&&cave| matches!(cave, Cave::Start))
            .count()
            < 2
            && (!current_cave.is_small()
                || !visited_caves.contains(&current_cave)
                || !(visited_caves
                    .iter()
                    .filter(|c| c.is_small())
                    .any(|c| visited_caves.iter().filter(|&a| a == c).count() >= 2)))
    }

    get_paths_recursive(input, Cave::Start, &Vec::new(), is_valid)
}

fn get_paths_recursive<F>(
    cave_system: &MultiMap<Cave, Cave>,
    current_cave: Cave,
    visited_caves: &[Cave],
    is_path_valid: F,
) -> usize
where
    F: Fn(&[Cave], Cave) -> bool + Copy,
{
    if matches!(current_cave, Cave::End) {
        return 1;
    } else if !is_path_valid(visited_caves, current_cave) {
        return 0;
    }

    let mut new_seen = Vec::with_capacity(visited_caves.len() + 1);
    for x in visited_caves {
        new_seen.push(*x);
    }
    new_seen.push(current_cave);

    cave_system
        .get_vec(&current_cave)
        .unwrap()
        .iter()
        .map(|p| get_paths_recursive(cave_system, *p, &new_seen, is_path_valid))
        .sum()
}

pub fn run(input: String) -> Result<()> {
    let parsed = parse(input.as_str());
    let start = Instant::now();
    let part1 = part1(&parsed);
    let p1_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 12 (part 1) is {}. Took {:?}",
        part1, p1_time
    );
    let start = Instant::now();
    let part2 = part2(&parsed);
    let p2_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 12 (part 2) is {}. Took {:?}",
        part2, p2_time
    );

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "start-A
                         start-b
                         A-c
                         A-b
                         b-d
                         A-end
                         b-end ";

    #[test]
    fn part1_test() {
        let parsed = parse(INPUT);
        println!("{:?}", parsed);
        let part1 = part1(&parsed);
        assert_eq!(part1, 10);
    }
}
