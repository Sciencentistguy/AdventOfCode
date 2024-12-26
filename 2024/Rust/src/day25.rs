use std::mem::MaybeUninit;

use fxhash::FxHashSet as HashSet;

type Parsed = (Vec<[usize; 5]>, Vec<[usize; 5]>);
type Solution = u64;

pub fn parse(input: &str) -> Parsed {
    let mut keys = Vec::new();
    let mut locks = Vec::new();

    for x in input.split("\n\n") {
        // Safety: The input is 5x7. Invalid input is not considered
        let arr = {
            let mut arr: [MaybeUninit<&[u8; 5]>; 7] = MaybeUninit::uninit_array();
            for (x, line) in x.lines().enumerate() {
                unsafe {
                    arr[x]
                        .as_mut_ptr()
                        .write(line.as_bytes().try_into().unwrap_unchecked())
                };
            }
            unsafe { MaybeUninit::array_assume_init(arr) }
        };

        let key = arr[6].iter().all(|&x| x == b'#');

        let mut res = [0; 5];
        for j in 0..5 {
            for i in 0..7 {
                if arr[i][j] == b'#' {
                    res[j] += 1;
                }
            }
        }
        res = res.map(|x| x - 1); // The top or bottom row is always full, but ignored in the counts

        if key {
            keys.push(res);
        } else {
            locks.push(res);
        }
    }
    (keys, locks)
}

fn try_together(key: &[usize; 5], lock: &[usize; 5]) -> bool {
    key.iter().zip(lock.iter()).all(|(&a, &b)| a + b < 6)
}

pub fn part1((keys, locks): &Parsed) -> Solution {
    let mut successes = HashSet::default();
    for lock in locks {
        for key in keys {
            if try_together(key, lock) {
                successes.insert((key, lock));
            }
        }
    }
    successes.len() as u64
}

pub fn part2(parsed: &Parsed) -> Solution {
    todo!()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "#####
.####
.####
.####
.#.#.
.#...
.....

#####
##.##
.#.##
...##
...#.
...#.
.....

.....
#....
#....
#...#
#.#.#
#.###
#####

.....
.....
#.#..
###..
###.#
###.#
#####

.....
.....
.....
#....
#.#..
#.#.#
#####
";

    const P1_SOLUTION: Solution = 3;
    // const P2_SOLUTION: Solution = todo!();

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        // assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
