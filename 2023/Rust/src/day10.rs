use std::collections::VecDeque;

const GRID_SIZE: usize = 140;

#[derive(Clone, Copy, Debug, PartialEq)]
enum Direction {
    North,
    East,
    South,
    West,
}

const COMPASS_DIRECTIONS: [Direction; 4] = [
    Direction::North,
    Direction::East,
    Direction::South,
    Direction::West,
];

#[derive(Clone, Copy, Debug, PartialEq)]
struct Pipe(Direction, Direction);

impl Pipe {
    fn traverse(self, edge: Direction) -> Option<Direction> {
        if edge == self.0 {
            Some(self.1)
        } else if edge == self.1 {
            Some(self.0)
        } else {
            None
        }
    }

    fn from_char(c: char) -> Option<Self> {
        use Direction::*;
        Some(match c {
            '|' => Self(North, South),
            '-' => Self(East, West),
            'L' => Self(North, East),
            'J' => Self(North, West),
            '7' => Self(South, West),
            'F' => Self(East, South),
            _ => return None,
        })
    }
}

#[derive(Debug, PartialEq)]
struct Edge {
    pos: usize,
    direction: Direction,
    steps: u32,
}

impl Edge {
    fn cross_edge(&self) -> Self {
        use Direction::*;
        match self.direction {
            North => Self {
                pos: self.pos - GRID_SIZE,
                direction: South,
                steps: self.steps,
            },
            East => Self {
                pos: self.pos + 1,
                direction: West,
                steps: self.steps,
            },
            South => Self {
                pos: self.pos + GRID_SIZE,
                direction: North,
                steps: self.steps,
            },
            West => Self {
                pos: self.pos - 1,
                direction: East,
                steps: self.steps,
            },
        }
    }

    fn traverse_pipe(&self, pipe: Pipe) -> Option<Self> {
        pipe.traverse(self.direction).map(|edge| Self {
            pos: self.pos,
            direction: edge,
            steps: self.steps + 1,
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Corner {
    TopLeft,
    TopRight,
    BottomLeft,
    BottomRight,
}

impl Corner {
    fn move_crosses(self, direction: Direction) -> Option<Direction> {
        use Corner::*;
        use Direction::*;
        match (self, direction) {
            (TopLeft, East) | (TopRight, West) => Some(North),
            (TopLeft, South) | (BottomLeft, North) => Some(West),
            (TopRight, South) | (BottomRight, North) => Some(East),
            (BottomLeft, East) | (BottomRight, West) => Some(South),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq)]
struct CornerPosition {
    pos: usize,
    corner: Corner,
}

impl CornerPosition {
    fn adjacent_positions<'a>(&'a self, pipe: &'a Option<Pipe>) -> impl Iterator<Item = Self> + 'a {
        use Corner::*;
        use Direction::*;

        // calculate adjacent locations, accounting for OOB
        let north = self.pos.checked_sub(GRID_SIZE);
        let east = ((self.pos % GRID_SIZE) != (GRID_SIZE - 1)).then_some(self.pos + 1);
        let west = self.pos.checked_sub(1);
        let south = ((self.pos / GRID_SIZE) != (GRID_SIZE - 1)).then_some(self.pos + GRID_SIZE);

        COMPASS_DIRECTIONS.iter().filter_map(move |direction| {
            // check for move blocked by part of pipe
            if let Some(cross) = self.corner.move_crosses(*direction) {
                if let Some(pipe) = pipe {
                    if pipe.0 == cross || pipe.1 == cross {
                        return None;
                    }
                }
            }

            // calculate destination
            match (direction, self.corner) {
                (North, TopLeft) => north.map(|pos| Self {
                    pos,
                    corner: BottomLeft,
                }),
                (North, TopRight) => north.map(|pos| Self {
                    pos,
                    corner: BottomRight,
                }),
                (North, BottomLeft) | (West, TopRight) => Some(Self {
                    pos: self.pos,
                    corner: TopLeft,
                }),
                (North, BottomRight) | (East, TopLeft) => Some(Self {
                    pos: self.pos,
                    corner: TopRight,
                }),
                (East, TopRight) => east.map(|pos| Self {
                    pos,
                    corner: TopLeft,
                }),
                (East, BottomLeft) | (South, TopRight) => Some(Self {
                    pos: self.pos,
                    corner: BottomRight,
                }),
                (East, BottomRight) => east.map(|pos| Self {
                    pos,
                    corner: BottomLeft,
                }),
                (South, TopLeft) | (West, BottomRight) => Some(Self {
                    pos: self.pos,
                    corner: BottomLeft,
                }),
                (South, BottomLeft) => south.map(|pos| Self {
                    pos,
                    corner: TopLeft,
                }),
                (South, BottomRight) => south.map(|pos| Self {
                    pos,
                    corner: TopRight,
                }),
                (West, TopLeft) => west.map(|pos| Self {
                    pos,
                    corner: TopRight,
                }),
                (West, BottomLeft) => west.map(|pos| Self {
                    pos,
                    corner: BottomRight,
                }),
            }
        })
    }
}

#[derive(Debug, PartialEq)]
struct CornerVisitTracker([u8; GRID_SIZE * GRID_SIZE]);

impl CornerVisitTracker {
    fn new() -> Self {
        Self([0; GRID_SIZE * GRID_SIZE])
    }

    fn count_unvisited(&self) -> usize {
        self.0
            .iter()
            .filter_map(|&val| (val == 0).then_some(1))
            .count()
    }

    fn visit(&mut self, pos: &CornerPosition) -> bool {
        let value = match pos.corner {
            Corner::TopLeft => 1,
            Corner::TopRight => 2,
            Corner::BottomLeft => 4,
            Corner::BottomRight => 8,
        };
        let visited = self.0[pos.pos] & value == value;
        self.0[pos.pos] |= value;
        visited
    }
}

pub struct Maze {
    start: usize,
    grid: [Option<Pipe>; GRID_SIZE * GRID_SIZE],
}

impl Maze {
    fn replacement_start_pipe(&self) -> Option<Pipe> {
        let pos = self.start;
        let mut edges = COMPASS_DIRECTIONS.iter().filter_map(|edge| {
            let cross = Edge {
                pos,
                direction: *edge,
                steps: 0,
            }
            .cross_edge();
            let Some(pipe) = self.grid[cross.pos] else {
                return None;
            };
            cross.traverse_pipe(pipe).map(|_| edge)
        });

        let first = edges.next();
        let second = edges.next();
        match (first, second) {
            (Some(first), Some(second)) => Some(Pipe(*first, *second)),
            _ => None,
        }
    }
}

pub fn parse(input: &str) -> Maze {
    let mut start = 0;
    let mut grid = [None; GRID_SIZE * GRID_SIZE];

    for (y, line) in input.lines().enumerate() {
        let row_start = y * GRID_SIZE;
        for (x, ch) in line.chars().enumerate() {
            let pos = row_start + x;
            grid[pos] = Pipe::from_char(ch);
            if ch == 'S' {
                start = pos;
            }
        }
    }

    Maze { start, grid }
}

pub fn part1(maze: &Maze) -> u32 {
    let mut visited = [false; GRID_SIZE * GRID_SIZE];
    let mut queue = VecDeque::new();

    for edge in COMPASS_DIRECTIONS {
        queue.push_back(
            Edge {
                pos: maze.start,
                direction: edge,
                steps: 0,
            }
            .cross_edge(),
        );
    }

    while let Some(pos) = queue.pop_front() {
        if let Some(pipe) = maze.grid[pos.pos] {
            if let Some(next) = pos.traverse_pipe(pipe).map(|p| p.cross_edge()) {
                if visited[next.pos] {
                    return next.steps + 1;
                }
                visited[next.pos] = true;
                queue.push_back(next);
            }
        }
    }

    unreachable!()
}

pub fn part2(maze: &Maze) -> usize {
    let mut visited = CornerVisitTracker::new();
    let mut queue = VecDeque::new();

    let start_pipe = maze.replacement_start_pipe();

    queue.push_back(CornerPosition {
        pos: 0,
        corner: Corner::TopLeft,
    });

    while let Some(pos) = queue.pop_front() {
        if !visited.visit(&pos) {
            let pipe = if pos.pos == maze.start {
                start_pipe
            } else {
                maze.grid[pos.pos]
            };
            queue.extend(pos.adjacent_positions(&pipe));
        }
    }

    visited.count_unvisited()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}
