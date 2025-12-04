use bstr::ByteSlice;
use common::ByteSplit;
use rayon::prelude::*;
use std::simd::prelude::*;

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

const LANES: usize = 64;
type Sv = Simd<u64, LANES>;

pub fn part1(parsed: &Parsed) -> Solution {
    parsed
        .par_iter()
        .map(|(a, b)| {
            let mut sum: u64 = 0;

            let mut chunks = (*a..=*b).array_chunks::<LANES>();

            // full SIMD chunks
            for arr in &mut chunks {
                let vec = Sv::from_array(arr);

                // prepare splats
                const V10: Sv = Sv::splat(10);
                const V99: Sv = Sv::splat(99);
                const V1000: Sv = Sv::splat(1000);
                const V9999: Sv = Sv::splat(9999);
                const V100000: Sv = Sv::splat(100_000);
                const V999999: Sv = Sv::splat(999_999);
                const V10000000: Sv = Sv::splat(10_000_000);
                const V99999999: Sv = Sv::splat(99_999_999);
                const V1000000000: Sv = Sv::splat(1_000_000_000);
                const V9999999999: Sv = Sv::splat(9_999_999_999u64);

                // These masks are used to identify which lanes meet the criteria. each value in the
                // SIMD vector is tested against the range and modulus conditions, and the resulting
                // masks indicate which lanes satisfy the conditions for each category.
                // As each category is mutually exclusive, we can combine the masks using a bitwise OR

                // compute masks for each category and modulus test
                let mask_2 = (vec.simd_ge(V10) & vec.simd_le(V99))
                    & (vec % Sv::splat(11)).simd_eq(Sv::splat(0));
                let mask_4 = (vec.simd_ge(V1000) & vec.simd_le(V9999))
                    & (vec % Sv::splat(101)).simd_eq(Sv::splat(0));
                let mask_6 = (vec.simd_ge(V100000) & vec.simd_le(V999999))
                    & (vec % Sv::splat(1001)).simd_eq(Sv::splat(0));
                let mask_8 = (vec.simd_ge(V10000000) & vec.simd_le(V99999999))
                    & (vec % Sv::splat(10001)).simd_eq(Sv::splat(0));
                let mask_10 = (vec.simd_ge(V1000000000) & vec.simd_le(V9999999999))
                    & (vec % Sv::splat(100001)).simd_eq(Sv::splat(0));

                let mask = mask_2 | mask_4 | mask_6 | mask_8 | mask_10;

                // Any lane that meets the criteria will have its value retained; others will be set to zero.
                let selected = mask.select(vec, Sv::splat(0));
                // We can then sum the selected values to get the total for this chunk.
                sum += selected.reduce_sum();
            }

            // scalar tail
            for v in chunks.into_remainder() {
                match v {
                    10..=99 if v % 11 == 0 => sum += v,
                    1000..=9999 if v % 101 == 0 => sum += v,
                    100000..=999999 if v % 1001 == 0 => sum += v,
                    10000000..=99999999 if v % 10001 == 0 => sum += v,
                    1000000000..=9_999_999_999u64 if v % 100001 == 0 => sum += v,
                    _ => {}
                }
            }

            sum
        })
        .sum::<u64>()
}

#[rustfmt::skip]
pub fn part2(parsed: &Parsed) -> Solution {
    // I could not for the life of me get a SIMD version of this to be faster than this parallel version.
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
