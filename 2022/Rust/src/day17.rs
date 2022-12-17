use std::collections::HashMap;

pub enum Draft {
    Left,
    Right,
}

const NUM_P1_ITERATIONS: usize = 2022;
const WIDTH: usize = 7;
const MAX_HEIGHT: usize = 8000;
type Rock = [u8; 16];
type Grid = [u8; WIDTH * MAX_HEIGHT];

const ROCK_A: Rock = [1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];
const ROCK_B: Rock = [0, 1, 0, 0, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0];
const ROCK_C: Rock = [1, 1, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0];
const ROCK_D: Rock = [1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0];
const ROCK_E: Rock = [1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

fn does_rock_fit(rock: &Rock, grid: &Grid, x0: usize, y0: usize) -> bool {
    // x0, y0 is the lower left corner of the rock.
    (0..16).all(|i| {
        let x = i % 4;
        let y = i / 4;
        rock[i] == 0 || grid[(y0 + y) * WIDTH + (x0 + x)] == 0
    })
}

fn paint_rock(rock: &Rock, grid: &mut Grid, x0: usize, y0: usize) {
    // x0, y0 is the lower left corner of the rock.
    #[allow(clippy::needless_range_loop)]
    for i in 0..16 {
        let x = i % 4;
        let y = i / 4;
        if rock[i] == 1 {
            grid[(y0 + y) * WIDTH + (x0 + x)] = 1;
        }
    }
}

fn hash_top_18_rows(grid_part: &[u8]) -> u128 {
    (0..(18 * WIDTH)).fold(0u128, |v, i| {
        if grid_part[i] == 1 {
            v + (1u128 << i)
        } else {
            v
        }
    })
}

#[derive(Hash, PartialEq, Eq)]
struct Configuration {
    rock: usize,
    draft: usize,
    grid: u128,
}

pub fn parse(inpt: &str) -> Vec<Draft> {
    assert!(inpt.is_ascii());

    inpt.trim()
        .bytes()
        .map(|x| match x {
            b'>' => Draft::Right,
            b'<' => Draft::Left,
            _ => panic!("Invalid input"),
        })
        .collect()
}

const ROCKS: [((i32, usize), [u8; 16]); 5] = [
    ((4, 1), ROCK_A),
    ((3, 3), ROCK_B),
    ((3, 3), ROCK_C),
    ((1, 4), ROCK_D),
    ((2, 2), ROCK_E),
];

pub fn part1(drafts: &[Draft]) -> usize {
    let mut grid: Grid = [0u8; WIDTH * MAX_HEIGHT];
    let mut draft_source = drafts.iter().enumerate().cycle();
    let mut floor = 0;
    for (i, ((width, height), rock)) in (0..1000000000000).zip(ROCKS.iter().cycle()) {
        let mut y = floor + 3;
        let mut x: i32 = 2;
        loop {
            let (_, draft) = draft_source.next().unwrap();
            match draft {
                Draft::Left => {
                    if x > 0 && does_rock_fit(rock, &grid, (x - 1) as usize, y) {
                        x -= 1;
                    }
                }
                Draft::Right => {
                    if x + width < WIDTH as i32 && does_rock_fit(rock, &grid, (x + 1) as usize, y) {
                        x += 1
                    }
                }
            }
            if y == 0 {
                break;
            } // Lands on bottom.
            if does_rock_fit(rock, &grid, x as usize, y - 1) {
                y -= 1;
            } else {
                break;
            }
        }
        paint_rock(rock, &mut grid, x as usize, y);

        floor = floor.max(y + height);

        if i == NUM_P1_ITERATIONS - 1 {
            return floor;
        }
    }

    unreachable!()
}

pub fn part2(drafts: &[Draft]) -> usize {
    let mut grid: Grid = [0u8; WIDTH * MAX_HEIGHT];
    let mut draft_source = drafts.iter().enumerate().cycle();
    let mut floor = 0;
    // reserve enough space to not have to allocate
    let mut seen_configurations: HashMap<Configuration, (usize, usize)> =
        HashMap::with_capacity(2319usize.next_power_of_two());
    let mut p2: Option<usize> = None;
    for (i, (rock_index, ((width, height), rock))) in
        (0..1000000000000).zip(ROCKS.iter().enumerate().cycle())
    {
        let mut y = floor + 3;
        let mut x: i32 = 2;
        let draft_index = loop {
            let (draft_index, draft) = draft_source.next().unwrap();
            match draft {
                Draft::Left => {
                    if x > 0 && does_rock_fit(rock, &grid, (x - 1) as usize, y) {
                        x -= 1;
                    }
                }
                Draft::Right => {
                    if x + width < WIDTH as i32 && does_rock_fit(rock, &grid, (x + 1) as usize, y) {
                        x += 1
                    }
                }
            }
            if y == 0 {
                break draft_index;
            } // Lands on bottom.
            if does_rock_fit(rock, &grid, x as usize, y - 1) {
                y -= 1;
            } else {
                break draft_index;
            }
        };
        paint_rock(rock, &mut grid, x as usize, y);

        floor = floor.max(y + height);

        if floor > 18 {
            let start = floor - 17;
            let configuration = Configuration {
                rock: rock_index,
                draft: draft_index,
                grid: hash_top_18_rows(&grid[start * WIDTH..(start + 18) * WIDTH]),
            };
            if let Some((iteration, floor_height)) = seen_configurations.get(&configuration) {
                let iteration_period = i - iteration;
                if 1000000000000 % iteration_period == i % iteration_period {
                    let floor_period = floor - floor_height;
                    let num_remaining = (1000000000000 - i) / iteration_period;
                    p2 = Some(floor + num_remaining * floor_period - 1);
                }
            } else {
                seen_configurations.insert(configuration, (i, floor));
            }
        }

        if i > NUM_P1_ITERATIONS && let Some(p2) = p2 {
            return p2;
        }
    }
    unreachable!()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 3068);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 1514285714288);
    }
}
