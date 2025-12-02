use std::hint::unreachable_unchecked;
use std::simd::prelude::*;
use std::simd::simd_swizzle;

use bstr::ByteSlice;
use common::ByteSplit;

type Parsed = Vec<i16>;
type Solution = u64;

pub fn parse(input: &str) -> Parsed {
    input
        .trim()
        .as_bytes()
        .byte_lines()
        .map(|x| {
            let sign = match x[0] {
                b'L' => 1,
                b'R' => -1,
                _ => unsafe { unreachable_unchecked() },
            };
            sign * unsafe { x[1..].to_str_unchecked() }.parse::<i16>().unwrap()
        })
        .collect()
}

pub fn part1(parsed: &Parsed) -> Solution {
    let mut count = 0;
    let mut current_sum: i16 = 50;

    const LANES: usize = 64;

    let (chunks, rest) = parsed.as_chunks::<LANES>();

    for chunk in chunks {
        let mut vec = Simd::<i16, LANES>::from_array(*chunk);

        const ZERO: Simd<i16, LANES> = Simd::splat(0);

        // Generate prefix sums via iterative doubling
        // after this, vec[i] contains sum of elements up to and including i in the chunk
        // i.e. [a, a+b, a+b+c, ...]

        // shift by 1
        let shifted = simd_swizzle!(
            ZERO,
            vec,
            [
                0, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83,
                84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103,
                104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
                120, 121, 122, 123, 124, 125, 126
            ]
        );
        vec += shifted;

        // shift by 2
        let shifted = simd_swizzle!(
            ZERO,
            vec,
            [
                0, 0, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82,
                83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102,
                103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118,
                119, 120, 121, 122, 123, 124, 125
            ]
        );
        vec += shifted;

        // shift by 4
        let shifted = simd_swizzle!(
            ZERO,
            vec,
            [
                0, 0, 0, 0, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81,
                82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101,
                102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117,
                118, 119, 120, 121, 122, 123
            ]
        );
        vec += shifted;

        // shift by 8
        let shifted = simd_swizzle!(
            ZERO,
            vec,
            [
                0, 0, 0, 0, 0, 0, 0, 0, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78,
                79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99,
                100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115,
                116, 117, 118, 119
            ]
        );
        vec += shifted;

        // shift by 16
        let shifted = simd_swizzle!(
            ZERO,
            vec,
            [
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 65, 66, 67, 68, 69, 70, 71, 72,
                73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93,
                94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111
            ]
        );
        vec += shifted;

        // shift by 32
        let shifted = simd_swizzle!(
            ZERO,
            vec,
            [
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81,
                82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95
            ]
        );
        vec += shifted;

        // now vec contains prefix sums within the chunk

        // Add previous total sum to get global running sums
        vec += Simd::splat(current_sum);

        // Update global running sum from last lane
        current_sum = vec[LANES - 1];

        // Check divisibility by 100 and count matches (this is the bit that is much faster with SIMD)
        let mask = (vec % Simd::splat(100)).simd_eq(Simd::splat(0));
        count += mask.to_bitmask().count_ones() as u64;
    }

    // Handle remaining elements
    for &rotation in rest {
        current_sum += rotation;
        if current_sum % 100 == 0 {
            count += 1;
        }
    }
    count
}

pub fn part2(parsed: &Parsed) -> Solution {
    let mut count: u64 = 0;
    let mut current_sum: i16 = 50;

    const LANES: usize = 64;

    let (chunks, rest) = parsed.as_chunks::<LANES>();

    for chunk in chunks {
        let orig = Simd::<i16, LANES>::from_array(*chunk);

        // same as above: prefix sums via iterative doubling
        let mut pref = orig;
        const ZERO32: Simd<i16, LANES> = Simd::splat(0);

        let shifted = simd_swizzle!(
            ZERO32,
            pref,
            [
                0, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83,
                84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103,
                104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119,
                120, 121, 122, 123, 124, 125, 126
            ]
        );
        pref += shifted;

        let shifted = simd_swizzle!(
            ZERO32,
            pref,
            [
                0, 0, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82,
                83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102,
                103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118,
                119, 120, 121, 122, 123, 124, 125
            ]
        );
        pref += shifted;

        let shifted = simd_swizzle!(
            ZERO32,
            pref,
            [
                0, 0, 0, 0, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81,
                82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101,
                102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117,
                118, 119, 120, 121, 122, 123
            ]
        );
        pref += shifted;

        let shifted = simd_swizzle!(
            ZERO32,
            pref,
            [
                0, 0, 0, 0, 0, 0, 0, 0, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78,
                79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99,
                100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115,
                116, 117, 118, 119
            ]
        );
        pref += shifted;

        let shifted = simd_swizzle!(
            ZERO32,
            pref,
            [
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 65, 66, 67, 68, 69, 70, 71, 72,
                73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93,
                94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111
            ]
        );
        pref += shifted;

        let shifted = simd_swizzle!(
            ZERO32,
            pref,
            [
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81,
                82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95
            ]
        );
        pref += shifted;

        // add global running sum to get per-lane end values
        let pref_with_global = pref + Simd::splat(current_sum);
        let end_vec = pref_with_global;
        let start_vec = pref_with_global - orig;

        // helper: euclidean floor division
        let euclid_div = |a: Simd<i16, _>| {
            let q = a / Simd::splat(100);
            let r = a % Simd::splat(100);
            let neg_r = r.simd_lt(Simd::splat(0));
            // subtract 1 where remainder negative
            q - neg_r.select(Simd::splat(1), Simd::splat(0))
        };

        // Count the number of times we crossed a multiple of 100, lanewise

        let q_end = euclid_div(end_vec);
        let q_start = euclid_div(start_vec);
        let pos_counts = q_end - q_start;

        let q_start_m1 = euclid_div(start_vec - Simd::splat(1));
        let q_end_m1 = euclid_div(end_vec - Simd::splat(1));
        let neg_counts = q_start_m1 - q_end_m1;

        let pos_mask = orig.simd_gt(Simd::splat(0));
        let neg_mask = orig.simd_lt(Simd::splat(0));
        let counts_vec = pos_mask.select(pos_counts, neg_mask.select(neg_counts, Simd::splat(0)));

        // sum lanes and accumulate
        let chunk_sum = counts_vec.reduce_sum();
        count += chunk_sum as u64;

        // update global running sum from last lane
        current_sum = end_vec[LANES - 1];
    }

    // scalar tail
    for &rotation in rest {
        let start = current_sum;
        let end = start + rotation;
        if rotation > 0 {
            count += (end.div_euclid(100) - start.div_euclid(100)) as u64;
        } else if rotation < 0 {
            count += ((start - 1).div_euclid(100) - (end - 1).div_euclid(100)) as u64;
        }
        current_sum = end;
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

    const INPUT: &str = "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
";

    const P1_SOLUTION: Solution = 3;
    const P2_SOLUTION: Solution = 6;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
