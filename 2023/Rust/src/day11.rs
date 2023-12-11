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
pub struct Starmap {
    empty_cols: Vec<i64>,
    empty_rows: Vec<i64>,
    stars: Vec<Point2D>,
}

pub fn parse(input: &str) -> Starmap {
    let mut empty_cols = Vec::new();
    let mut empty_rows = Vec::new();
    let mut stars = Vec::new();
    let lines: Vec<_> = input.lines().collect();
    for (y, line) in lines.iter().enumerate() {
        let num_stars_before_row = stars.len();
        for (x, c) in line.chars().enumerate() {
            if c == '#' {
                stars.push(Point2D {
                    x: x as i64,
                    y: y as i64,
                });
            }
        }
        if stars.len() == num_stars_before_row {
            empty_rows.push(y as i64)
        }
    }
    for x in 0..lines[0].len() {
        // for each col, if we find no '#' we save
        let mut hash_found = false;
        for line in &lines {
            if line.as_bytes()[x] == b'#' {
                hash_found = true;
            }
        }
        if !hash_found {
            empty_cols.push(x as i64);
        }
    }

    Starmap {
        empty_cols,
        empty_rows,
        stars,
    }
}

pub fn part1(maze: &Starmap) -> usize {
    let mut total_distance = 0;
    dbg!(maze.stars.iter().combinations(2).count());
    for v in maze.stars.iter().enumerate().combinations(2) {
        let a = v[0].1;
        let b = v[1].1;
        let log = v[0].0 == 0 && v[1].0 == 7;
        if log {
            dbg!(&v);
        }
        let mut added_distance = 0;
        for dx in a.x..b.x {
            if maze.empty_cols.contains(&dx) {
                added_distance += 1;
            }
        }
        if log {
            dbg!(added_distance);
        }
        for dy in a.y..b.y {
            if maze.empty_rows.contains(&dy) {
                added_distance += 1;
            }
        }
        if log {
            dbg!(added_distance);
        }
        if log {
            dbg!(a.distance_to(b) + added_distance);
        }
        total_distance += a.distance_to(b) + added_distance;
    }
    total_distance
}

// uniquePairs = snd . foldr (\x (acc, xs) -> (x:acc, zip acc (repeat x) ++ xs)) ([], [])

pub fn part2(maze: &Starmap) -> u32 {
    todo!()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
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
        assert_eq!(part1(&dbg!(parse(INPUT))), 374);
    }
    #[test]
    fn test_part2() {
        // assert_eq!(part2(&parse(INPUT)), 2);
    }
}
