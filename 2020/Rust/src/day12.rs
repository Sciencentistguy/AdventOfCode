use std::time::{Duration, Instant};

#[derive(Debug)]
struct Instruction {
    pub direction: Direction,
    pub amount: i64,
}

#[derive(Debug, PartialEq, Eq)]
enum Direction {
    North,
    South,
    East,
    West,
    Left,
    Right,
    Forward,
}

fn parse_input(input: &str) -> (Vec<Instruction>, Duration) {
    let start = Instant::now();
    let out = input
        .lines()
        .map(|l| l.as_bytes())
        .map(|line| {
            let direction = match line[0] {
                b'N' => Direction::North,
                b'S' => Direction::South,
                b'E' => Direction::East,
                b'W' => Direction::West,
                b'R' => Direction::Right,
                b'L' => Direction::Left,
                b'F' => Direction::Forward,
                _ => unreachable!("invalid input"),
            };
            let amount = unsafe { std::str::from_utf8_unchecked(&line[1..]) }
                .parse()
                .unwrap();

            Instruction { direction, amount }
        })
        .collect();
    let end = Instant::now();
    (out, end - start)
}

fn manhattan_distance(x1: i64, y1: i64, x2: i64, y2: i64) -> i64 {
    (x1 - x2).abs() + (y1 - y2).abs()
}

fn solve_part1(input: &[Instruction]) -> (i64, Duration) {
    let start = Instant::now();
    let mut x = 0;
    let mut y = 0;
    let mut direction = Direction::East;

    for instruction in input {
        match instruction {
            Instruction {
                direction: Direction::North,
                amount,
            } => {
                y += amount;
            }
            Instruction {
                direction: Direction::South,
                amount,
            } => {
                y -= amount;
            }
            Instruction {
                direction: Direction::East,
                amount,
            } => {
                x += amount;
            }
            Instruction {
                direction: Direction::West,
                amount,
            } => {
                x -= amount;
            }
            Instruction {
                direction: Direction::Right,
                amount,
            } => {
                for _ in 0..(amount / 90) {
                    direction = match direction {
                        Direction::North => Direction::East,
                        Direction::East => Direction::South,
                        Direction::South => Direction::West,
                        Direction::West => Direction::North,
                        _ => unreachable!(),
                    }
                }
            }
            Instruction {
                direction: Direction::Left,
                amount,
            } => {
                for _ in 0..(amount / 90) {
                    direction = match direction {
                        Direction::North => Direction::West,
                        Direction::East => Direction::North,
                        Direction::South => Direction::East,
                        Direction::West => Direction::South,
                        _ => unreachable!(),
                    }
                }
            }
            Instruction {
                direction: Direction::Forward,
                amount,
            } => match direction {
                Direction::North => y += amount,
                Direction::South => y -= amount,
                Direction::East => x += amount,
                Direction::West => x -= amount,
                _ => unreachable!(),
            },
        }
    }

    let out = manhattan_distance(0, 0, x, y);

    let end = Instant::now();
    (out, end - start)
}

fn solve_part2(input: &[Instruction]) -> (i64, Duration) {
    let start = Instant::now();
    let mut ship_x = 0;
    let mut ship_y = 0;
    let mut waypoint_x = 10;
    let mut waypoint_y = 1;

    for instruction in input {
        match instruction {
            Instruction {
                direction: Direction::North,
                amount,
            } => waypoint_y += amount,
            Instruction {
                direction: Direction::South,
                amount,
            } => waypoint_y -= amount,
            Instruction {
                direction: Direction::East,
                amount,
            } => waypoint_x += amount,
            Instruction {
                direction: Direction::West,
                amount,
            } => waypoint_x -= amount,
            Instruction {
                direction: Direction::Right,
                amount,
            } => {
                for _ in 0..(amount / 90) {
                    let tmp = waypoint_y;
                    waypoint_y = -waypoint_x;
                    waypoint_x = tmp;
                }
            }
            Instruction {
                direction: Direction::Left,
                amount,
            } => {
                for _ in 0..(amount / 90) {
                    let tmp = waypoint_x;
                    waypoint_x = -waypoint_y;
                    waypoint_y = tmp;
                }
            }
            Instruction {
                direction: Direction::Forward,
                amount,
            } => {
                ship_x += waypoint_x * amount;
                ship_y += waypoint_y * amount;
            }
        }
    }

    let out = manhattan_distance(0, 0, ship_x, ship_y);

    let end = Instant::now();
    (out, end - start)
}

pub fn run(input: String) {
    let (parsed_input, time) = parse_input(&input);
    println!("Day 12, parsing input took {}ns", time.as_nanos());
    let (p1, time) = solve_part1(&parsed_input);
    println!("Day 12, part 1: {}. Took {}ns", p1, time.as_nanos());
    let (p2, time) = solve_part2(&parsed_input);
    println!("Day 12, part 2: {}. Took {}ns", p2, time.as_nanos());
}
