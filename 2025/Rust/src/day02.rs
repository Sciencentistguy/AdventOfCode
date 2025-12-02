use bstr::ByteSlice;
use common::ByteSplit;
use rayon::prelude::*;

type Parsed = Vec<(u64, u64)>;
type Solution = u64;

pub fn parse(input: &str) -> Parsed {
    input
        .trim()
        .as_bytes()
        .byte_split(b',')
        .map(|pair| unsafe {
            let (a, b) = pair.byte_split_once(b'-').unwrap_unchecked();
            (
                a.to_str_unchecked().parse().unwrap(),
                b.to_str_unchecked().parse().unwrap(),
            )
        })
        .collect()
}

pub fn part1(parsed: &Parsed) -> Solution {
    parsed
        .par_iter()
        .flat_map(|(a, b)| *a..=*b)
        .filter(|x| match x {
            10..=99 => x % 11 == 0,
            1000..=9999 => x % 101 == 0,
            100000..=999999 => x % 1001 == 0,
            10000000..=99999999 => x % 10001 == 0,
            1000000000..=9999999999 => x % 100001 == 0,
            _ => false,
        })
        .sum::<u64>()
}

#[rustfmt::skip]
pub fn part2(parsed: &Parsed) -> Solution {
    parsed
        .par_iter()
        .flat_map(|(a, b)| *a..=*b)
        .filter(|id| match id {
            10..=99 => id % 11 == 0,
            100..=999 => id % 111 == 0,
            1000..=9999 => id % 101 == 0 || id % 1111 == 0,
            10000..=99999 => id % 11111 == 0,
            100000..=999999 => id % 1001 == 0 || id % 10101 == 0 || id % 111111 == 0,
            1000000..=9999999 => id % 1111111 == 0,
            10000000..=99999999 => id % 10001 == 0 || id % 1010101 == 0 || id % 11111111 == 0,
            100000000..=999999999 => id % 1001001 == 0 || id % 111111111 == 0,
            1000000000..=9999999999 => id % 100001 == 0 || id % 101010101 == 0 || id % 1111111111 == 0,
            _ => false,
        })
        .sum::<u64>()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124";

    const P1_SOLUTION: Solution = 1227775554;
    const P2_SOLUTION: Solution = 4174379265;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
