use common::Vec2D;
use fxhash::FxHashMap as HashMap;
use fxhash::FxHashSet as HashSet;
use itertools::Itertools;

type Parsed = (HashMap<Vec2D<usize>, char>, usize, usize);
type Solution = usize;

pub fn parse(input: &str) -> Parsed {
    let lines: Vec<_> = input.lines().collect();
    let max_y = lines.len();
    let max_x = lines[0].len();
    (
        lines
            .iter()
            .enumerate()
            .flat_map(|(y, line)| {
                line.bytes()
                    .enumerate()
                    .filter_map(move |(x, c)| (c != b'.').then_some((Vec2D::new(x, y), c as char)))
            })
            .collect(),
        max_x,
        max_y,
    )
}

pub fn part1((antennae, max_x, max_y): &Parsed) -> Solution {
    let max_x = *max_x;
    let max_y = *max_y;
    let mut found = HashSet::default();

    for ((&a, a_freq), (&b, b_freq)) in antennae.iter().tuple_combinations() {
        if a_freq == b_freq {
            let c1 = (b * 2).checked_sub(a);
            let c2 = (a * 2).checked_sub(b);

            if let Some(c1) = c1
                && c1.x < max_x
                && c1.y < max_y
            {
                found.insert(c1);
            }
            if let Some(c2) = c2
                && c2.x < max_x
                && c2.y < max_y
            {
                found.insert(c2);
            }
        }
    }

    found.len()
}

pub fn part2((antennae, max_x, max_y): &Parsed) -> Solution {
    let max_x = *max_x;
    let max_y = *max_y;
    let mut found: HashSet<_> = antennae.keys().copied().collect(); // All antennae are antinodes in part 2

    for ((&a, a_freq), (&b, b_freq)) in antennae.iter().tuple_combinations() {
        if a_freq == b_freq {
            let diff = a.signed_sub(b);

            for i in 1.. {
                if let Some(p1) = a.checked_add_signed(diff * i)
                    && p1.x < max_x
                    && p1.y < max_y
                {
                    found.insert(p1);
                } else {
                    break;
                }
            }

            for i in 1.. {
                if let Some(p2) = b.checked_sub_signed(diff * i)
                    && p2.x < max_x
                    && p2.y < max_y
                {
                    found.insert(p2);
                } else {
                    break;
                };
            }
        }
    }

    found.len()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............";

    const P1_SOLUTION: Solution = 14;
    const P2_SOLUTION: Solution = 34;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
