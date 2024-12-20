use fxhash::FxHashMap as HashMap;
use fxhash::FxHashSet as HashSet;

type Parsed<'a> = (HashSet<&'a str>, Vec<&'a str>);
type Solution = usize;

pub fn parse(input: &str) -> Parsed {
    let (patterns, designs) = input.split_once("\n\n").unwrap();
    (patterns.split(", ").collect(), designs.lines().collect())
}

//depth first search to find which designs can be made by concatenating the patterns
fn dfs<'a>(patterns: &HashSet<&'a str>, design: &str, idx: usize) -> bool {
    let mut queue = vec![(design, idx)];
    while let Some((design, idx)) = queue.pop() {
        if idx == design.len() {
            return true;
        }

        for pattern in patterns {
            // for pattern in patterns {
                if design[idx..].starts_with(pattern) {
                    queue.push((design, idx + pattern.len()));
                }
            // }
        }
    }
    false
}

fn recurse_possible(
    pattern: &str,
    position: usize,
    towels: &HashSet<&str>,
    max_towel_size: usize,
    memoize: &mut HashMap<usize, usize>,
) -> usize {
    if let Some(memoized) = memoize.get(&position) {
        return *memoized;
    }

    let mut count = 0;
    let sub_patterns = (0..max_towel_size)
        .filter_map(|step| {
            if step + position < pattern.len() {
                Some(&pattern[position..=position + step])
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    for sub_pattern in sub_patterns {
        if towels.contains(&sub_pattern) {
            if position + sub_pattern.len() == pattern.len() {
                count += 1;
            } else {
                count += recurse_possible(
                    pattern,
                    position + sub_pattern.len(),
                    towels,
                    max_towel_size,
                    memoize,
                );
            }
        }
    }

    memoize.insert(position, count);
    count
}

pub fn part1((patterns, designs): &Parsed) -> Solution {
    designs
        .iter()
        .filter(|design| dfs(patterns, design, 0))
        .count()
}

pub fn part2((patterns, designs): &Parsed) -> Solution {
    let max = patterns.iter().map(|x| x.len()).max().unwrap();
    designs
        .iter()
        .map(|design| {
            recurse_possible(design, 0, patterns, max, &mut HashMap::default())
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

    const INPUT: &str = "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb
";

    const P1_SOLUTION: Solution = 6;
    const P2_SOLUTION: Solution = 16;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
