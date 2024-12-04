use std::collections::HashMap;

use common::Vec2D;

type Parsed = HashMap<Vec2D<isize>, Letter>;
type Solution = u64;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Letter {
    X,
    M,
    A,
    S,
}

fn m_and_s(lhs: Letter, rhs: Letter) -> bool {
    match (lhs, rhs) {
        (Letter::M, Letter::S) => true,
        (Letter::S, Letter::M) => true,
        _ => false,
    }
}

pub fn parse(input: &str) -> Parsed {
    input
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars()
                .enumerate()
                .map(move |(x, c)| ((x as isize, y as isize).into(), c))
        })
        .filter_map(|(pos, c)| match c {
            'X' => Some((pos, Letter::X)),
            'M' => Some((pos, Letter::M)),
            'A' => Some((pos, Letter::A)),
            'S' => Some((pos, Letter::S)),
            _ => None,
        })
        .collect()
}

const WORD: [Letter; 4] = [Letter::X, Letter::M, Letter::A, Letter::S];
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

fn wordsearch(map: &Parsed) -> u64 {
    let mut count = 0;

    for (&coord, &letter) in map.iter() {
        if letter == *WORD.first().unwrap() {
            for direction in &DIRECTIONS {
                if check_word(map, coord, *direction) {
                    count += 1;
                }
            }
        }
    }

    count
}

fn check_word(map: &Parsed, start: Vec2D<isize>, direction: Vec2D<isize>) -> bool {
    let mut current_coord = start;
    for ch in WORD {
        if let Some(&map_ch) = map.get(&current_coord) {
            if map_ch != ch {
                return false;
            }
        } else {
            return false;
        }
        current_coord += direction;
    }
    true
}

pub fn part1(parsed: &Parsed) -> Solution {
    wordsearch(parsed)
}

pub fn part2(parsed: &Parsed) -> Solution {
    let mut count = 0;

    for (&coord, &letter) in parsed.iter() {
        if letter == Letter::A {
            // if top-left of us there is a letter, bottom right needs to be the other letter
            // if top-right of us there is a letter, bottom left needs to be the other letter

            let top_left = coord + (-1, -1).into();
            let top_right = coord + (1, -1).into();
            let bottom_left = coord + (-1, 1).into();
            let bottom_right = coord + (1, 1).into();

            if let Some(&top_left_letter) = parsed.get(&top_left) {
                if let Some(&bottom_right_letter) = parsed.get(&bottom_right) {
                    if let Some(&top_right_letter) = parsed.get(&top_right) {
                        if let Some(&bottom_left_letter) = parsed.get(&bottom_left) {
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
