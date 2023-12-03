use std::ops::Range;

use once_cell::sync::Lazy;
use regex::Regex;
use smallvec::SmallVec;

#[derive(Debug, PartialEq, Eq)]
struct Coord {
    x: usize,
    y: usize,
}

#[derive(Debug)]
struct Number {
    value: u32,
    y: usize,
    x_range: Range<usize>,
}

#[derive(Debug)]
struct Symbol {
    c: char,
    position: Coord,
}

#[derive(Debug)]
pub struct Schematic {
    numbers: Vec<Number>,
    symbols: Vec<Symbol>,
}

pub fn parse(inpt: &str) -> Schematic {
    static PARSE_REGEX: Lazy<Regex> = Lazy::new(|| Regex::new(r"\d+").unwrap());
    let numbers = inpt
        .trim()
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            PARSE_REGEX.find_iter(line).map(move |m| Number {
                y,
                x_range: m.start()..m.end(),
                value: m.as_str().parse().unwrap(),
            })
        })
        .collect::<Vec<_>>();
    let symbols = inpt
        .lines()
        .enumerate()
        .flat_map(|(y, line)| {
            line.chars().enumerate().filter_map(move |(x, c)| {
                if !matches!(c, '.' | '0'..='9') {
                    Some(Symbol {
                        c,
                        position: Coord { x, y },
                    })
                } else {
                    None
                }
            })
        })
        .collect::<Vec<_>>();
    Schematic { numbers, symbols }
}

pub fn part1(schematic: &Schematic) -> u32 {
    let mut ret = 0;
    'outer: for number in &schematic.numbers {
        let x_start = number.x_range.start.saturating_sub(1);
        let x_end = number.x_range.end;

        let y_start = number.y.saturating_sub(1);
        let y_end = number.y + 1;

        for &Symbol {
            position: Coord { x, y },
            ..
        } in &schematic.symbols
        {
            if y_start <= y && y <= y_end && x_start <= x && x <= x_end {
                ret += number.value;
                continue 'outer;
            }
        }
    }
    ret
}

pub fn part2(schematic: &Schematic) -> u32 {
    schematic
        .symbols
        .iter()
        .filter_map(
            |&Symbol {
                 c,
                 position: Coord { x, y },
             }| {
                if c != '*' {
                    return None;
                }
                let numbers = schematic
                    .numbers
                    .iter()
                    .filter_map(|number| {
                        let x_start = number.x_range.start.saturating_sub(1);
                        let x_end = number.x_range.end;
                        let y_start = number.y.saturating_sub(1);
                        let y_end = number.y + 1;

                        if y_start <= y && y <= y_end && x_start <= x && x <= x_end {
                            Some(number.value)
                        } else {
                            None
                        }
                    })
                    .collect::<SmallVec<_, 4>>();
                if numbers.len() == 2 {
                    Some(numbers.iter().product::<u32>())
                } else {
                    None
                }
            },
        )
        .sum()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &str = "467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..
";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&dbg!(parse(INPUT))), 4361);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&dbg!(parse(INPUT))), 467835);
    }
}
