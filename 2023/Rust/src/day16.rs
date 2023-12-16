use bitvec::prelude::*;
use rayon::prelude::*;

#[derive(Clone, Copy)]
pub enum Optic {
    None,
    HorizontalSplitter,
    VerticalSplitter,
    ForwardMirror,
    BackwardMirror,
}

impl Optic {
    fn from_char(c: char) -> Self {
        match c {
            '.' => Self::None,
            '-' => Self::HorizontalSplitter,
            '|' => Self::VerticalSplitter,
            '/' => Self::ForwardMirror,
            '\\' => Self::BackwardMirror,
            _ => unreachable!(),
        }
    }

    fn next_op(self, dir: Direction) -> Operation {
        use Direction::*;
        use Operation::*;
        match self {
            Optic::None => Operation::Follow(dir),
            Optic::HorizontalSplitter => match dir {
                Up | Down => Split(Left, Right),
                Left | Right => Follow(dir),
            },
            Optic::VerticalSplitter => match dir {
                Up | Down => Follow(dir),
                Left | Right => Split(Up, Down),
            },
            Optic::ForwardMirror | Optic::BackwardMirror => Follow(dir.reflect_in(self)),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl Direction {
    fn reflect_in(self, mirror: Optic) -> Self {
        use Direction::*;

        match mirror {
            Optic::ForwardMirror => match self {
                Up => Right,
                Down => Left,
                Left => Down,
                Right => Up,
            },
            Optic::BackwardMirror => match self {
                Up => Left,
                Down => Right,
                Left => Up,
                Right => Down,
            },
            _ => panic!("Cannot reflect in an optic that is not a mirror"),
        }
    }
}

impl Direction {
    pub fn apply(self, r: usize, c: usize) -> Option<(usize, usize)> {
        match self {
            Direction::Up => r.checked_sub(1).map(|r| (r, c)),
            Direction::Down => Some((r + 1, c)),
            Direction::Left => c.checked_sub(1).map(|c| (r, c)),
            Direction::Right => Some((r, c + 1)),
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Operation {
    Follow(Direction),
    Split(Direction, Direction),
}

fn energise(
    input: &[Vec<Optic>],
    starting_row: usize,
    starting_col: usize,
    starting_direction: Direction,
) -> usize {
    let num_rows = input.len();
    let num_cols = input[0].len();

    let mut seen = vec![vec![bitarr![u8, Lsb0; 0; 4]; num_cols]; num_rows];
    seen[0][0].set(starting_direction as usize, true);

    // State is stored in tuples of (row, col, direction)
    let mut stack = Vec::new();
    stack.push((starting_row, starting_col, starting_direction));

    while let Some((row, col, dir)) = stack.pop() {
        match input[row][col].next_op(dir) {
            Operation::Follow(dir) => {
                follow(num_rows, num_cols, row, col, dir, &mut seen, &mut stack);
            }
            Operation::Split(dir1, dir2) => {
                follow(num_rows, num_cols, row, col, dir1, &mut seen, &mut stack);
                follow(num_rows, num_cols, row, col, dir2, &mut seen, &mut stack);
            }
        }
    }

    seen.into_iter()
        .flat_map(|x| x.into_iter())
        .filter(|x| x.into_inner()[0] != 0)
        .count()
}

fn follow(
    num_rows: usize,
    num_cols: usize,
    row: usize,
    col: usize,
    dir: Direction,
    seen: &mut [Vec<BitArray<[u8; 1]>>],
    queue: &mut Vec<(usize, usize, Direction)>,
) {
    let Some((row, col)) = dir.apply(row, col) else {
        return;
    };

    if row >= num_rows || col >= num_cols {
        return;
    }

    if seen[row][col][dir as usize] {
        return;
    }

    seen[row][col].set(dir as usize, true);
    queue.push((row, col, dir));
}

pub fn parse(input: &str) -> Vec<Vec<Optic>> {
    input
        .lines()
        .map(|x| x.chars().map(Optic::from_char).collect())
        .collect()
}

pub fn part1(cavern: &[Vec<Optic>]) -> usize {
    energise(cavern, 0, 0, Direction::Right)
}

pub fn part2(cavern: &[Vec<Optic>]) -> usize {
    let num_rows = cavern.len();
    let num_cols = cavern[0].len();

    (0..num_rows)
        .into_par_iter()
        .map(|r| {
            energise(cavern, r, 0, Direction::Right).max(energise(
                cavern,
                r,
                num_cols - 1,
                Direction::Left,
            ))
        })
        .chain((0..num_cols).into_par_iter().map(|c| {
            energise(cavern, 0, c, Direction::Down).max(energise(
                cavern,
                num_rows - 1,
                c,
                Direction::Up,
            ))
        }))
        .max()
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
    const INPUT: &str = r".|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....
";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 46);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 52);
    }
}
