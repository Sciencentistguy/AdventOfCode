type Solution = usize;

const ROBOT: u8 = b'@';
const WALL: u8 = b'#';
const BOX: u8 = b'O';
const AIR: u8 = b'.';

const BOX_LEFT: u8 = b'[';
const BOX_RIGHT: u8 = b']';

pub fn part1(input: &[u8]) -> Solution {
    let dim = input.iter().position(|b| b == &b'\n').unwrap() + 1;

    // Robot index
    let mut rob = input.iter().position(|b| b == &ROBOT).unwrap() as i32;

    let move_map = |b| match b {
        b'<' => -1,
        b'>' => 1,
        b'^' => -(dim as i32),
        b'v' => dim as i32,
        _ => unreachable!(),
    };

    let mut grid = input[..dim * (dim - 1)].to_vec();
    let moves = &input[dim * (dim - 1) + 1..];

    for &m in moves.iter().filter(|&m| *m != b'\n') {
        let dir = move_map(m);
        let mut i = rob + dir;

        // Step in direction until air is reached, then loop backwards moving the boxes and robot
        'outer: loop {
            match grid[i as usize] {
                AIR => loop {
                    grid[i as usize] = grid[(i - dir) as usize];
                    if grid[i as usize] == ROBOT {
                        grid[(i - dir) as usize] = AIR;
                        rob = i;
                        break 'outer;
                    }
                    i -= dir;
                },
                BOX => i += dir,
                WALL => break,
                _ => unreachable!(),
            }
        }
    }

    grid.iter()
        .enumerate()
        .filter(|(_, t)| **t == BOX)
        .fold(0, |acc, (i, _)| {
            let y_score = 100 * (i / dim);
            let x_score = i % dim;
            acc + y_score + x_score
        })
}

pub fn part2(input: &[u8]) -> Solution {
    fn move_boxes(grid: &mut [u8], left: i32, i: i32, dir: i32) -> bool {
        // Store indices of left part of boxes to be moved
        let mut swaps = [0; 32];
        let mut s_len = 0;

        if rec(grid, &mut swaps, &mut s_len, left, dir) {
            // Check was successful. Now the boxes get moved.
            for &s in swaps.iter().take(s_len) {
                grid.swap(s as usize, (s - dir) as usize);
                grid.swap((s + 1) as usize, (s + 1 - dir) as usize);
            }
            // Move robot
            grid.swap(i as usize, (i - dir) as usize);
            return true;
        }
        false
    }

    /// Recursive call that returns true if a move can be made.
    fn rec(grid: &mut [u8], swaps: &mut [i32], s_len: &mut usize, left: i32, dir: i32) -> bool {
        let left = left + dir;

        // check if we've seen it before
        if swaps[..*s_len].contains(&left) {
            return true;
        }

        if match (grid[left as usize], grid[(left + 1) as usize]) {
            (AIR, AIR) => true,
            (WALL, _) | (_, WALL) => false,
            (BOX_LEFT, _) => rec(grid, swaps, s_len, left, dir),
            (AIR, BOX_LEFT) => rec(grid, swaps, s_len, left + 1, dir),
            (BOX_RIGHT, AIR) => rec(grid, swaps, s_len, left - 1, dir),
            (BOX_RIGHT, BOX_LEFT) => {
                rec(grid, swaps, s_len, left - 1, dir) && rec(grid, swaps, s_len, left + 1, dir)
            }
            (a, b) => unreachable!("Unexpected pattern: \"{}{}\"", a as char, b as char),
        } {
            swaps[*s_len] = left;
            *s_len += 1;
            return true;
        }
        false
    }

    let height = input.iter().position(|b| b == &b'\n').unwrap();
    let width = height * 2;

    // Robot index
    let mut rob = 0;

    // Faster direction lookup
    let move_map = |b| match b {
        b'<' => -1,
        b'>' => 1,
        b'^' => -(width as i32),
        b'v' => width as i32,
        _ => unreachable!(),
    };

    // Mutable version of traversable area
    let mut grid = vec![0; height * width];

    // Build grid from input
    let mut gi = 0;
    let mut i = 0;
    let i_max = (height + 1) * height;
    loop {
        match input[i] {
            AIR => {
                grid[gi] = AIR;
                grid[gi + 1] = AIR;
                gi += 2;
            }
            BOX => {
                grid[gi] = BOX_LEFT;
                grid[gi + 1] = BOX_RIGHT;
                gi += 2;
            }
            WALL => {
                grid[gi] = WALL;
                grid[gi + 1] = WALL;
                gi += 2;
            }
            ROBOT => {
                rob = gi as i32;
                grid[gi] = ROBOT;
                grid[gi + 1] = AIR;
                gi += 2;
            }
            _ => (),
        }
        i += 1;
        if i == i_max {
            break;
        }
    }

    let moves = &input[height * (height + 1) + 1..];

    for &m in moves.iter().filter(|&m| *m != b'\n') {
        let dir = move_map(m);

        // Index of next robot position
        let mut i = rob + dir;

        // Horizontal moves are handled same as part 1
        if dir.abs() == 1 {
            // Step in direction until air is reached, then loop backwards moving the boxes and robot
            'outer: loop {
                match grid[i as usize] {
                    AIR => loop {
                        grid[i as usize] = grid[(i - dir) as usize];
                        if grid[i as usize] == ROBOT {
                            grid[(i - dir) as usize] = AIR;
                            rob = i;
                            break 'outer;
                        }
                        i -= dir;
                    },
                    BOX_LEFT | BOX_RIGHT => i += 2 * dir,
                    WALL => break,
                    _ => unreachable!(),
                }
            }
            continue;
        }

        if match grid[i as usize] {
            AIR => {
                grid.swap(i as usize, rob as usize);
                true
            }
            WALL => false,
            BOX_LEFT => move_boxes(&mut grid, i, i, dir),
            BOX_RIGHT => move_boxes(&mut grid, i - 1, i, dir),
            _ => unreachable!(),
        } {
            rob = i;
        }
    }

    grid.iter()
        .enumerate()
        .filter(|(_, t)| **t == BOX_LEFT)
        .fold(0, |acc, (i, _)| {
            let y_score = 100 * (i / width);
            let x_score = i % width;
            acc + y_score + x_score
        })
}

pub fn run(input: &str) {
    println!("Part 1: {}", part1(input.as_bytes()));
    println!("Part 2: {}", part2(input.as_bytes()));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
";

    const P1_SOLUTION: Solution = 10092;
    const P2_SOLUTION: Solution = 9021;

    #[test]
    fn test_part1() {
        assert_eq!(part1(INPUT.as_bytes()), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(INPUT.as_bytes()), P2_SOLUTION);
    }
}
