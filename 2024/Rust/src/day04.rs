use std::collections::HashSet;
use std::mem::MaybeUninit;

use common::Vec2D;
use ndarray::{Array2, Axis};

type Parsed = Array2<u8>;
type Solution = u64;

fn m_and_s(lhs: u8, rhs: u8) -> bool {
    matches!((lhs, rhs), (b'M', b'S') | (b'S', b'M'))
}

pub fn parse(input: &str) -> Parsed {
    let lines = input.lines().collect::<Vec<_>>();

    let mut arr: Array2<MaybeUninit<u8>> = Array2::uninit((lines[0].len(), lines.len()));

    #[cfg(debug_assertions)]
    let mut indices = HashSet::new();

    for (i, line) in lines.iter().enumerate() {
        for (j, ch) in line.bytes().enumerate() {
            #[cfg(debug_assertions)]
            indices.insert((j, i));

            unsafe { arr[(j, i)].as_mut_ptr().write(ch) };
        }
    }

    #[cfg(debug_assertions)]
    {
        // check that all indices have been written
        for x in 0..arr.len_of(Axis(0)) {
            for y in 0..arr.len_of(Axis(1)) {
                debug_assert!(indices.contains(&(x, y)));
            }
        }
    }

    unsafe { arr.assume_init() }
}

const WORD: [u8; 4] = [b'X', b'M', b'A', b'S'];
const DIRECTIONS: [Vec2D<isize>; 8] = [
    Vec2D { x: 1, y: 0 },   // right
    Vec2D { x: -1, y: 0 },  // left
    Vec2D { x: 0, y: 1 },   // down
    Vec2D { x: 0, y: -1 },  // up
    Vec2D { x: 1, y: 1 },   // down-right
    Vec2D { x: -1, y: -1 }, // up-left
    Vec2D { x: 1, y: -1 },  // down-left
    Vec2D { x: -1, y: 1 },  // up-right
];

fn check_word(map: &Parsed, mut current: Vec2D<usize>, direction: Vec2D<isize>) -> bool {
    for ch in WORD {
        if let Some(&map_ch) = map.get(current) {
            if map_ch != ch {
                return false;
            }
        } else {
            return false;
        }
        current.wrapping_add_assign_signed(direction);
    }
    true
}

pub fn part1(map: &Parsed) -> Solution {
    let mut count = 0;

    for (coord, &letter) in map.indexed_iter() {
        const FIRST: u8 = *WORD.first().unwrap();
        if letter == FIRST {
            for direction in DIRECTIONS {
                if check_word(map, coord.into(), direction) {
                    count += 1;
                }
            }
        }
    }

    count
}

pub fn part2(parsed: &Parsed) -> Solution {
    let mut count = 0;

    for (coord, &letter) in parsed.indexed_iter() {
        let coord: Vec2D<_> = coord.into();
        if matches!(letter, b'A') {
            const UP_LEFT: Vec2D<isize> = Vec2D { x: -1, y: -1 };
            const UP_RIGHT: Vec2D<isize> = Vec2D { x: 1, y: -1 };
            const DOWN_LEFT: Vec2D<isize> = Vec2D { x: -1, y: 1 };
            const DOWN_RIGHT: Vec2D<isize> = Vec2D { x: 1, y: 1 };

            let Some(top_left) = coord.checked_add_signed(UP_LEFT) else {
                continue;
            };
            let Some(bottom_right) = coord.checked_add_signed(DOWN_RIGHT) else {
                continue;
            };
            let Some(top_right) = coord.checked_add_signed(UP_RIGHT) else {
                continue;
            };
            let Some(bottom_left) = coord.checked_add_signed(DOWN_LEFT) else {
                continue;
            };

            if let Some(&top_left_letter) = parsed.get(top_left) {
                if let Some(&bottom_right_letter) = parsed.get(bottom_right) {
                    if let Some(&top_right_letter) = parsed.get(top_right) {
                        if let Some(&bottom_left_letter) = parsed.get(bottom_left) {
                            if m_and_s(top_left_letter, bottom_right_letter)
                                && m_and_s(top_right_letter, bottom_left_letter)
                            {
                                count += 1;
                            }
                        }
                    }
                }
            }
        }
    }

    count
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX
";

    const P1_SOLUTION: Solution = 18;
    const P2_SOLUTION: Solution = 9;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
