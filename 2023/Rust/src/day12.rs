use bstr::BString;
use itertools::Itertools;
use ndarray::Array2;
use tap::Tap;

type Parsed = Vec<(BString, Vec<u64>)>;

type Cache = Array2<Option<u64>>;

fn count(pattern: &[u8], groups: &[u64], cache: &mut Cache) -> u64 {
    // seen before
    if let Some(cached) = cache[(pattern.len(), groups.len())] {
        return cached;
    }

    if groups.is_empty() {
        cache[(pattern.len(), groups.len())] = Some(!pattern.iter().any(|&x| x == b'#') as u64);
        return cache[(pattern.len(), groups.len())].unwrap();
    }

    // mark as seen
    cache[(pattern.len(), groups.len())] = Some(0);

    let size = *groups.first().unwrap() as usize;

    if pattern.len() <= size {
        return cache[(pattern.len(), groups.len())].unwrap();
    }
    if pattern[size] != b'#' && !pattern.iter().take(size).any(|&c| c == b'.') {
        *cache[(pattern.len(), groups.len())].as_mut().unwrap() +=
            count(&pattern[size + 1..], &groups[1..], cache);
    }

    if *pattern.first().unwrap() == b'#' {
        cache[(pattern.len(), groups.len())].unwrap()
    } else {
        *cache[(pattern.len(), groups.len())].as_mut().unwrap() +=
            count(&pattern[1..], groups, cache);
        cache[(pattern.len(), groups.len())].unwrap()
    }
}

fn each_line(line: &str, multiplier: usize) -> (BString, Vec<u64>) {
    let (a, b) = line.split_once(' ').unwrap();
    let pattern = itertools::repeat_n(a, multiplier)
        .join("?")
        .into_bytes()
        .tap_mut(|x| x.push(b'.'))
        .into();
    let groups = itertools::repeat_n(b, multiplier)
        .join(",")
        .split(',')
        .map(|x| x.parse().unwrap())
        .collect();

    (pattern, groups)
}

pub fn parse(input: &str) -> (Parsed, Parsed) {
    let p1 = input.lines().map(|x| each_line(x, 1)).collect();
    let p2 = input.lines().map(|x| each_line(x, 5)).collect();
    (p1, p2)
}

pub fn solve(parsed: &Parsed) -> u64 {
    parsed
        .iter()
        .map(|(pattern, groups)| {
            let mut cache: Cache = Array2::from_elem((pattern.len() + 2, groups.len() + 1), None);
            count(pattern, groups, &mut cache)
        })
        .sum()
}

pub fn part1(parsed: &Parsed) -> u64 {
    solve(parsed)
}

pub fn part2(parsed: &Parsed) -> u64 {
    solve(parsed)
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed.0));
    println!("Part 2: {}", part2(&parsed.1));
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &str = "???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&dbg!(parse(INPUT)).0), 21);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&dbg!(parse(INPUT)).1), 525152);
    }
}
