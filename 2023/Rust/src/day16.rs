#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Direction {
    Up,
    Down,
    Left,
    Right,
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

pub fn parse(input: &str) -> Vec<&[u8]> {
    input.lines().map(|x| x.as_bytes()).collect()
}

fn energise(
    input: &[&[u8]],
    starting_row: usize,
    starting_col: usize,
    starting_direction: Direction,
) -> usize {
    use Direction::*;
    use Operation::*;

    let num_rows = input.len();
    let num_cols = input[0].len();

    let mut seen = vec![vec![0u8; num_cols]; num_rows];
    seen[0][0] |= 1 << (starting_direction as u32);

    // State is stored in tuples of (row, col, direction)
    let mut stack = Vec::new();
    stack.push((starting_row, starting_col, starting_direction));

    while let Some((row, col, dir)) = stack.pop() {
        let next = match input[row][col] {
            b'.' => Follow(dir),
            b'-' => match dir {
                Up | Down => Split(Left, Right),
                Left | Right => Follow(dir),
            },
            b'|' => match dir {
                Up | Down => Follow(dir),
                Left | Right => Split(Up, Down),
            },
            b'/' => match dir {
                Up => Follow(Right),
                Down => Follow(Left),
                Left => Follow(Down),
                Right => Follow(Up),
            },

            b'\\' => match dir {
                Up => Follow(Left),
                Down => Follow(Right),
                Left => Follow(Up),
                Right => Follow(Down),
            },

            _ => unreachable!(),
        };

        match next {
            Follow(dir) => {
                follow(num_rows, num_cols, row, col, dir, &mut seen, &mut stack);
            }
            Split(dir1, dir2) => {
                follow(num_rows, num_cols, row, col, dir1, &mut seen, &mut stack);
                follow(num_rows, num_cols, row, col, dir2, &mut seen, &mut stack);
            }
        }
    }

    seen.into_iter()
        .flat_map(|x| x.into_iter())
        .filter(|&x| x != 0)
        .count()
}

fn follow(
    rows: usize,
    cols: usize,
    row: usize,
    col: usize,
    dir: Direction,
    seen: &mut [Vec<u8>],
    queue: &mut Vec<(usize, usize, Direction)>,
) {
    let Some((row, col)) = dir.apply(row, col) else {
        return;
    };

    if row >= rows || col >= cols {
        return;
    }

    if seen[row][col] & (1 << dir as u32) != 0 {
        return;
    }

    seen[row][col] |= 1 << dir as u32;
    queue.push((row, col, dir));
}

pub fn part1(cavern: &[&[u8]]) -> usize {
    energise(cavern, 0, 0, Direction::Right)
}

pub fn part2(cavern: &[&[u8]]) -> usize {
    let rows = cavern.len();
    let cols = cavern[0].len();

    let mut ret = 0;
    for r in 0..rows {
        ret = ret.max(energise(cavern, r, 0, Direction::Right));
        ret = ret.max(energise(cavern, r, cols - 1, Direction::Left));
    }

    for c in 0..cols {
        ret = ret.max(energise(cavern, 0, c, Direction::Down));
        ret = ret.max(energise(cavern, rows - 1, c, Direction::Up));
    }

    ret
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
        // assert_eq!(part2(&parse(INPUT)), 2);
    }
}
