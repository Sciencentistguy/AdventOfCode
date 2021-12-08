mod permutations;

use std::{hint::unreachable_unchecked, time::Instant};

use eyre::Result;

struct InputLine<'a> {
    signal_patterns: Vec<&'a str>,
    output: Vec<&'a str>,
}

fn parse(input: &str) -> Vec<InputLine> {
    input
        .lines()
        .map(|line| {
            let mut it = line.split('|');
            let first = it.next().unwrap();
            let last = it.next().unwrap();
            debug_assert!(it.next().is_none());
            let signal_patterns = first.trim().split_whitespace().collect();
            let output = last.trim().split_whitespace().collect();
            InputLine {
                signal_patterns,
                output,
            }
        })
        .collect()
}

fn part1(input: &[InputLine]) -> u64 {
    let mut c = 0;
    for InputLine {
        signal_patterns: _,
        output,
    } in input
    {
        for seg in output {
            match seg.len() {
                2 | 4 | 3 | 7 => c += 1,
                _ => {}
            }
        }
    }
    c
}

fn part2(input: &[InputLine]) -> u64 {
    let valid_numbers: [&[u8]; 10] = [
        &[b'a', b'b', b'c', b'e', b'f', b'g'],
        &[b'c', b'f'],
        &[b'a', b'c', b'd', b'e', b'g'],
        &[b'a', b'c', b'd', b'f', b'g'],
        &[b'b', b'c', b'd', b'f'],
        &[b'a', b'b', b'd', b'f', b'g'],
        &[b'a', b'b', b'd', b'e', b'f', b'g'],
        &[b'a', b'c', b'f'],
        &[b'a', b'b', b'c', b'd', b'e', b'f', b'g'],
        &[b'a', b'b', b'c', b'd', b'f', b'g'],
    ];

    let mut c = 0;
    let mut cs: Vec<u8> = Vec::with_capacity(10);
    for InputLine {
        signal_patterns,
        output,
    } in input
    {
        for p in permutations::PERMUTATIONS {
            let mut swaps = [0; u8::MAX as usize];
            for (k, v) in p.iter().zip(b'a'..=b'g') {
                swaps[*k as usize] = v;
            }
            let is_valid = signal_patterns.iter().all(|&s| {
                valid_numbers.contains({
                    cs.clear();
                    cs.extend_from_slice(s.as_bytes());
                    cs.sort_unstable();
                    for c in &mut cs {
                        *c = swaps[*c as usize];
                    }
                    cs.sort_unstable();
                    &cs.as_slice()
                })
            });

            if is_valid {
                c += output
                    .iter()
                    .rev()
                    .enumerate()
                    .map(|(i, &s)| {
                        10u64.pow(i as u32)
                            * chars_to_u64({
                                cs.clear();
                                cs.extend_from_slice(s.as_bytes());
                                cs.sort_unstable();
                                for c in &mut cs {
                                    *c = swaps[*c as usize];
                                }
                                cs.sort_unstable();
                                &cs
                            })
                    })
                    .sum::<u64>();
                continue;
            }
        }
    }
    c
}

fn chars_to_u64(input: &[u8]) -> u64 {
    match input {
        [b'a', b'b', b'c', b'e', b'f', b'g'] => 0,
        [b'c', b'f'] => 1,
        [b'a', b'c', b'd', b'e', b'g'] => 2,
        [b'a', b'c', b'd', b'f', b'g'] => 3,
        [b'b', b'c', b'd', b'f'] => 4,
        [b'a', b'b', b'd', b'f', b'g'] => 5,
        [b'a', b'b', b'd', b'e', b'f', b'g'] => 6,
        [b'a', b'c', b'f'] => 7,
        [b'a', b'b', b'c', b'd', b'e', b'f', b'g'] => 8,
        [b'a', b'b', b'c', b'd', b'f', b'g'] => 9,
        _ => unsafe {
            if cfg!(debug_assertions) {
                unreachable!()
            }
            unreachable_unchecked()
        },
    }
}

pub fn run(input: String) -> Result<()> {
    let parsed = parse(input.as_str());
    let start = Instant::now();
    let part1 = part1(&parsed);
    let p1_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 08 (part 1) is {}. Took {:?}",
        part1, p1_time
    );
    let start = Instant::now();
    let part2 = part2(&parsed);
    let p2_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 08 (part 2) is {}. Took {:?}",
        part2, p2_time
    );

    Ok(())
}
