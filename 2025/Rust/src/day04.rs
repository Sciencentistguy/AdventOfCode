use std::hint::unreachable_unchecked;

use ndarray::Array2;

use common::Neighbours;

type Parsed = ndarray::Array2<Entry>;
type Solution = u64;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
pub enum Entry {
    Roll,
    Space,
}

pub fn parse(input: &str) -> Parsed {
    let lines = input.trim().lines().collect::<Vec<_>>();

    let width = lines[0].len();
    let height = lines.len();
    let mut ret = Array2::uninit((width, height));
    for (x, line) in lines.iter().enumerate() {
        for (y, ch) in line.bytes().enumerate() {
            let ch = match ch {
                b'@' => Entry::Roll,
                b'.' => Entry::Space,
                _ => unsafe { unreachable_unchecked() },
            };
            unsafe {
                std::ptr::write(ret.get_mut_ptr((x, y)).unwrap().cast::<_>(), ch);
            }
        }
    }
    unsafe { ret.assume_init() }
}

#[inline(never)]
pub fn part1(input: &Parsed) -> Solution {
    let mut counter = 0;

    for ((x, y), pos) in input.indexed_iter() {
        if pos != &Entry::Roll {
            continue;
        }

        if input
            .neighbours((x, y))
            .filter(|x| *x == Entry::Roll)
            .count()
            < 4
        {
            counter += 1;
        }
    }
    counter
}

pub fn part2(input: &Parsed) -> Solution {
    let mut input = input.clone();

    let mut count = 0;
    let mut diff = u64::MAX;
    while diff > 0 {
        diff = 0;
        for x in 0..input.ncols() {
            for y in 0..input.nrows() {
                if unsafe { input.get((x, y)).unwrap_unchecked() } != &Entry::Roll {
                    continue;
                }

                let neighbour_count = input
                    .neighbours((x, y))
                    .filter(|x| *x == Entry::Roll)
                    .count();

                let ch = unsafe { input.get_mut((x, y)).unwrap_unchecked() };
                if *ch == Entry::Roll && neighbour_count < 4 {
                    *ch = Entry::Space;
                    count += 1;
                    diff += 1;
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

    const INPUT: &str = "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.
";

    const P1_SOLUTION: Solution = 13;
    const P2_SOLUTION: Solution = 43;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
