use itertools::Itertools;

#[derive(Debug)]
struct Point2D {
    x: i64,
    y: i64,
}

impl Point2D {
    fn distance_to(&self, other: &Self) -> usize {
        (self.x - other.x).unsigned_abs() as usize + (self.y - other.y).unsigned_abs() as usize
    }
}

#[derive(Debug)]
pub struct Starmap(Vec<Point2D>);

pub fn parse_with_multiplier(input: &str, multiplier: i64) -> Starmap {
    let lines: Vec<_> = input.lines().collect();

    let n_cols = lines[0].len();

    let mut stars = Vec::new();
    let mut cols_with_star = vec![false; n_cols];

    let mut y = 0;

    for line in &lines {
        let mut row_has_star = false;
        for (x, c) in line.chars().enumerate() {
            if c == '#' {
                stars.push(Point2D { x: x as i64, y });
                row_has_star = true;
                cols_with_star[x] = true;
            }
        }

        y += 1;

        if !row_has_star {
            y += multiplier - 1;
        }
    }

    for (i, has_star) in cols_with_star.iter().enumerate().rev() {
        if !has_star {
            for star in &mut stars {
                if star.x > i as i64 {
                    star.x += multiplier - 1;
                }
            }
        }
    }

    Starmap(stars)
}

pub fn parse(input: &str) -> (Starmap, Starmap) {
    (
        parse_with_multiplier(input, 2),
        parse_with_multiplier(input, 1_000_000),
    )
}

fn common(stars: &Starmap) -> usize {
    stars
        .0
        .iter()
        .combinations(2)
        .map(|v| v[0].distance_to(v[1]))
        .sum()
}

pub fn part1(stars: &Starmap) -> usize {
    common(stars)
}

pub fn part2(stars: &Starmap) -> usize {
    common(stars)
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed.0));
    println!("Part 2: {}", part2(&parsed.1));
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &str = "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#.....
";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&dbg!(parse(INPUT).0)), 374);
    }
}
