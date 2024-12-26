use fxhash::FxHashSet as HashSet;

type Parsed = (Vec<[usize; 5]>, Vec<[usize; 5]>);
type Solution = u64;

pub fn parse(input: &str) -> Parsed {
    let mut keys = Vec::new();
    let mut locks = Vec::new();

    for x in input.split("\n\n") {
        let mut arr = [[false; 5]; 7];
        for (i, line) in x.lines().enumerate() {
            for (j, c) in line.chars().enumerate() {
                arr[i][j] = c == '#';
            }
        }

        let key = if arr[0].iter().all(|x| *x) {
            false
        } else if arr[6].iter().all(|x| *x) {
            true
        } else {
            unreachable!("Neither top nor bottom row is entirely filled: {x}");
        };

        let mut res = [0; 5];
        for j in 0..5 {
            for i in 0..7 {
                if arr[i][j] {
                    res[j] += 1;
                }
            }
        }
        res = res.map(|x| x - 1);
        if key {
            keys.push(res);
        } else {
            locks.push(res);
        }
    }
    (keys, locks)
}

fn try_together(key: &[usize; 5], lock: &[usize; 5]) -> bool {
    for i in 0..5 {
        if key[i] + lock[i] >= 6 {
            return false;
        }
    }
    true
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
