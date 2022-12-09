use rustc_hash::FxHashSet;

#[derive(Clone, Copy)]
pub enum Direction {
    Left,
    Right,
    Down,
    Up,
}

impl Direction {
    #[inline]
    const fn to_dydx(self) -> (i32, i32) {
        match self {
            Direction::Left => (0, -1),
            Direction::Right => (0, 1),
            Direction::Down => (1, 0),
            Direction::Up => (-1, 0),
        }
    }
}

pub struct Instruction {
    direction: Direction,
    steps: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct Point2D {
    x: i32,
    y: i32,
}

impl Point2D {
    const START: Point2D = Point2D { x: 0, y: 0 };

    const fn is_touching(self, other: Self) -> bool {
        self.x == other.x && (self.y - other.y).abs() == 1
            || self.y == other.y && (self.x - other.x).abs() == 1
            || ((self.y - other.y).abs() == 1 && (self.x - other.x).abs() == 1)
    }

    const fn is_in_same_row_or_column(self, other: Self) -> bool {
        self.x == other.x || self.y == other.y
    }

    fn follow(&mut self, head: Self) {
        let tail = self;

        if !tail.is_touching(head) && !tail.is_in_same_row_or_column(head) {
            if tail.x < head.x {
                tail.x += 1;
            } else {
                tail.x -= 1;
            }
            if tail.y < head.y {
                tail.y += 1;
            } else {
                tail.y -= 1;
            }
        }

        if tail.x <= head.x - 2 {
            tail.x = head.x - 1;
        } else if tail.x >= head.x + 2 {
            tail.x = head.x + 1;
        } else if tail.y <= head.y - 2 {
            tail.y = head.y - 1;
        } else if tail.y >= head.y + 2 {
            tail.y = head.y + 1;
        }
    }
}

pub fn parse(inpt: &str) -> Vec<Instruction> {
    inpt.lines()
        .map(|line| {
            let steps = line[2..].parse().unwrap();
            let direction = match line.as_bytes()[0] {
                b'L' => Direction::Left,
                b'R' => Direction::Right,
                b'D' => Direction::Down,
                b'U' => Direction::Up,
                _ => panic!("Unknown direction"),
            };
            Instruction { direction, steps }
        })
        .collect()
}

fn common<const N: usize>(instructions: &[Instruction]) -> usize {
    let mut seen = FxHashSet::default();
    seen.reserve(8000);

    let mut head = Point2D::START;
    let mut rope = [Point2D::START; N];

    for Instruction { direction, steps } in instructions {
        let (dx, dy) = direction.to_dydx();

        for _ in 0..*steps {
            head.x += dx;
            head.y += dy;

            // The first knot follows head
            rope[0].follow(head);

            // Each following knot follows the previous knot
            for i in 1..rope.len() {
                let previous_knot = rope[i - 1];

                rope[i].follow(previous_knot);
            }

            // Track the last knot (the tail)
            seen.insert(unsafe { *rope.last().unwrap_unchecked() });
        }
    }

    seen.len()
}

pub fn part1(parsed: &[Instruction]) -> usize {
    common::<1>(parsed)
}

pub fn part2(parsed: &[Instruction]) -> usize {
    common::<9>(parsed)
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

    const INPUT: &str = "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2";

    const INPUT2: &str = "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 13);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 1);
        assert_eq!(part2(&parse(INPUT2)), 36);
    }
}
