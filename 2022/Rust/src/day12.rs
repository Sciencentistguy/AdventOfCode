use arrayvec::ArrayVec;
use pathfinding::prelude::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point2D {
    x: i32,
    y: i32,
}

impl Point2D {
    fn successors(&self, grid: &[Vec<i32>]) -> ArrayVec<Point2D, 4> {
        let &Point2D { x, y } = self;

        let directions = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)];
        let current = grid[y as usize][x as usize];

        directions
            .into_iter()
            .filter(|d| {
                d.0 >= 0 && d.1 >= 0 && d.0 < grid[0].len() as i32 && d.1 < grid.len() as i32
            })
            .filter(|direction| grid[direction.1 as usize][direction.0 as usize] <= current + 1)
            .map(|direction| Point2D {
                x: direction.0,
                y: direction.1,
            })
            .collect()
    }
}

pub struct Parsed {
    map: Vec<Vec<i32>>,
    start: Point2D,
    end: Point2D,
}

pub fn parse(inpt: &str) -> Parsed {
    let mut start = None;
    let mut end = None;
    Parsed {
        map: inpt
            .lines()
            .enumerate()
            .map(|(y, line)| {
                line.bytes()
                    .enumerate()
                    .map(|(x, c)| match c {
                        b'S' => {
                            start = Some(Point2D {
                                x: x as _,
                                y: y as _,
                            });
                            0
                        }
                        b'E' => {
                            end = Some(Point2D {
                                x: x as _,
                                y: y as _,
                            });
                            25
                        }
                        _ => (c - 97) as _,
                    })
                    .collect()
            })
            .collect(),
        end: end.unwrap(),
        start: start.unwrap(),
    }
}

fn distance(map: &[Vec<i32>], start: Point2D, end: Point2D) -> Option<usize> {
    bfs(&start, |p| p.successors(map), |p| *p == end).map(|x| x.len() - 1)
}

pub fn part1(parsed: &Parsed) -> usize {
    distance(&parsed.map, parsed.start, parsed.end).unwrap()
    // bfs(
    // &parsed.start,
    // |p| p.successors(&parsed.map),
    // |p| *p == parsed.end,
    // )
    // .unwrap()
    // .len()
    // - 1
}

pub fn part2(parsed: &Parsed) -> usize {
    let mut start_positions = Vec::new();

    for y in 0..parsed.map.len() {
        for x in 0..parsed.map[y].len() {
            if parsed.map[y][x] == 0 {
                start_positions.push(Point2D {
                    x: x as _,
                    y: y as _,
                });
            }
        }
    }

    start_positions
        .into_iter()
        .filter_map(|start| distance(&parsed.map, start, parsed.end))
        .min()
        .unwrap()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    const INPUT: &str = "Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 31);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 29);
    }
}
