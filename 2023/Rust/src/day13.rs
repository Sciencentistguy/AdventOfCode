use std::ops::Index;

#[derive(Debug, PartialEq)]
enum Reflection {
    Vertical(usize),
    Horizontal(usize),
}

pub struct Map<'a> {
    string: &'a str,
    cols: usize,
    rows: usize,
}

impl Index<(usize, usize)> for Map<'_> {
    type Output = u8;

    fn index(&self, index @ (row, column): (usize, usize)) -> &Self::Output {
        if (row < self.rows) & (column < self.cols) {
            let index = row * (self.cols + 1) + column;
            &self.string.as_bytes()[index]
        } else {
            panic!("Error: index {index:?} out of range");
        }
    }
}

impl<'a> Map<'a> {
    fn find_reflection(&self, num_smudges: usize) -> Option<Reflection> {
        fn reflection_indices(middle: usize, limit: usize) -> impl Iterator<Item = (usize, usize)> {
            let max_offset = (limit - middle).min(middle);
            (0..max_offset).map(move |o| (middle - 1 - o, middle + o))
        }

        fn find_smudge_inner(
            changed_amount: usize,
            outer_limit: usize,
            inner_limit: usize,
            coord_fn: impl Fn(usize, usize) -> u8,
        ) -> Option<usize> {
            (1..outer_limit).find(|&outer| {
                reflection_indices(outer, outer_limit).try_fold(0, |acc, (o1, o2)| {
                    let new_acc = (0..inner_limit)
                        .filter(|inner| coord_fn(o1, *inner) != coord_fn(o2, *inner))
                        .count()
                        + acc;
                    (new_acc <= changed_amount).then_some(new_acc)
                }) == Some(changed_amount)
            })
        }

        find_smudge_inner(num_smudges, self.rows, self.cols, |r, c| self[(r, c)])
            .map(Reflection::Horizontal)
            .or_else(|| {
                find_smudge_inner(num_smudges, self.cols, self.rows, |c, r| self[(r, c)])
                    .map(Reflection::Vertical)
            })
    }
}

pub fn parse(input: &str) -> Vec<Map> {
    input
        .split("\n\n")
        .map(|string| {
            let rows = string.lines().count();
            let cols = string.lines().map(|x| x.len()).max().unwrap();
            Map { string, cols, rows }
        })
        .collect()
}

pub fn part1(maps: &[Map]) -> usize {
    maps.iter()
        .map(|map| match map.find_reflection(0).unwrap() {
            Reflection::Vertical(x) => x,
            Reflection::Horizontal(x) => 100 * x,
        })
        .sum()
}

pub fn part2(maps: &[Map]) -> usize {
    maps.iter()
        .map(|map| match map.find_reflection(1).unwrap() {
            Reflection::Vertical(x) => x,
            Reflection::Horizontal(x) => 100 * x,
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
    const INPUT: &str = "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 405);
    }
    #[test]
    fn test_part2() {
        // assert_eq!(part2(&parse(INPUT)), 2);
    }
}
