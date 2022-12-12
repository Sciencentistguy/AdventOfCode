use std::collections::{HashSet, VecDeque};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point2D {
    x: usize,
    y: usize,
}

pub struct Parsed {
    map: Vec<Vec<usize>>,
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

fn search(queue: &mut VecDeque<(Point2D, usize)>, end: Point2D, map: &[Vec<usize>]) -> usize {
    let row_len = map[0].len();
    let col_len = map.len();

    let mut seen = HashSet::with_capacity(map.len() * map[0].len());
    while let Some((p @ Point2D { x, y }, distance)) = queue.pop_front() {
        if seen.contains(&p) {
            continue;
        }
        seen.insert(p);

        if p == end {
            return distance;
        }

        let height = map[y][x];

        for (dx, dy) in [(0, 1), (1, 0), (0, -1), (-1, 0)] {
            let new_x = (x as isize + dx) as usize;
            let new_y = (y as isize + dy) as usize;

            if new_x < row_len && new_y < col_len {
                let new_height = map[new_y][new_x];
                if new_height <= height + 1 {
                    queue.push_back((Point2D { x: new_x, y: new_y }, distance + 1))
                }
            }
        }
    }
    unreachable!()
}

pub fn part1(parsed: &Parsed) -> usize {
    let mut queue = VecDeque::new();
    queue.push_back((parsed.start, 0));
    search(&mut queue, parsed.end, &parsed.map)
}

pub fn part2(parsed: &Parsed) -> usize {
    let mut start_positions = VecDeque::new();

    for y in 0..parsed.map.len() {
        for x in 0..parsed.map[y].len() {
            if parsed.map[y][x] == 0 {
                start_positions.push_back((
                    Point2D {
                        x: x as _,
                        y: y as _,
                    },
                    0,
                ));
            }
        }
    }
    search(&mut start_positions, parsed.end, &parsed.map)
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
