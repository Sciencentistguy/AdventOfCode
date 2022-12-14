use std::collections::HashSet;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Point2D {
    x: usize,
    y: usize,
}

impl Point2D {
    const fn below(&self) -> Self {
        Self {
            x: self.x,
            y: self.y + 1,
        }
    }

    const fn below_right(&self) -> Self {
        Self {
            x: self.x + 1,
            y: self.y + 1,
        }
    }

    const fn below_left(&self) -> Self {
        Self {
            x: self.x - 1,
            y: self.y + 1,
        }
    }
}

const STARTING_POINT: Point2D = Point2D { x: 500, y: 0 };

pub fn parse(inpt: &str) -> HashSet<Point2D> {
    let mut hs = HashSet::new();
    for line in inpt.lines() {
        let pairs = line
            .split(" -> ")
            .map(|x| {
                let (a, b) = x.split_once(',').unwrap();
                (a.parse().unwrap(), b.parse().unwrap())
            })
            .map(|(x, y)| Point2D { x, y })
            .collect::<Vec<_>>();
        for [from, to] in pairs.array_windows() {
            if from.x == to.x {
                let first = from.y.min(to.y);
                let last = from.y.max(to.y);
                for y in first..=last {
                    hs.insert(Point2D { x: from.x, y });
                }
            } else {
                let first = from.x.min(to.x);
                let last = from.x.max(to.x);
                for x in first..=last {
                    hs.insert(Point2D { x, y: from.y });
                }
            }
        }
    }
    hs
}

pub fn part1(room: &HashSet<Point2D>) -> usize {
    let mut room = room.to_owned();
    let mut count = 0;
    let mut len = room.len();

    let y_max = room.iter().map(|p| p.y).max().unwrap();

    loop {
        let resting_point = {
            let mut current_peice = STARTING_POINT;
            loop {
                if current_peice.y > y_max {
                    break None;
                }
                // current_peice.y += 1;
                if !room.contains(&current_peice.below()) {
                    current_peice = current_peice.below();
                    continue;
                }
                if !room.contains(&current_peice.below_left()) {
                    current_peice = current_peice.below_left();
                    continue;
                }
                if !room.contains(&current_peice.below_right()) {
                    current_peice = current_peice.below_right();
                    continue;
                }

                break Some(current_peice);
            }
        };

        if let Some(point) = resting_point {
            room.insert(point);
        }

        if len == room.len() {
            return count;
        }
        len = room.len();
        count += 1;
    }
}

pub fn part2(room: &HashSet<Point2D>) -> u64 {
    let mut room = room.to_owned();
    let mut count = 0;

    let y_max = room.iter().map(|p| p.y).max().unwrap() + 1;

    loop {
        count += 1;
        let point = {
            let mut current_peice = STARTING_POINT;
            loop {
                if current_peice.y == y_max {
                    break current_peice;
                }
                // current_peice.y += 1;
                if !room.contains(&current_peice.below()) {
                    current_peice = current_peice.below();
                    continue;
                }
                if !room.contains(&current_peice.below_left()) {
                    current_peice = current_peice.below_left();
                    continue;
                }
                if !room.contains(&current_peice.below_right()) {
                    current_peice = current_peice.below_right();
                    continue;
                }

                break current_peice;
            }
        };

        if point == STARTING_POINT {
            return count;
        }

        room.insert(point);
    }
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 24);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 93);
    }
}
