use std::{collections::HashMap, time::Instant};

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
    let mut c = 0;
    for InputLine {
        signal_patterns,
        output,
    } in input
    {
        let mut mappings = HashMap::with_capacity(7);
        let sets: Vec<_> = [signal_patterns, output]
            .iter()
            .flat_map(|x| x.iter())
            .map(|d| d.as_bytes())
            .collect();
        let mut one = None;
        let mut four = None;
        for &set in &sets {
            match set.len() {
                2 => {
                    mappings.insert(set, 1);
                    one = Some(set);
                }
                4 => {
                    mappings.insert(set, 4);
                    four = Some(set)
                }
                3 => {
                    mappings.insert(set, 7);
                }
                7 => {
                    mappings.insert(set, 8);
                }
                _ => {}
            };
        }

        for &set in &sets {
            let match_1 = count_same(set, one.unwrap());
            let match_4 = count_same(set, four.unwrap());
            match set.len() {
                5 => {
                    match (match_1, match_4) {
                        (1, 2) => mappings.insert(set, 2),
                        (2, 3) => mappings.insert(set, 3),
                        (1, 3) => mappings.insert(set, 5),
                        _ => {
                            println!("5 {:?}", (match_1, match_4));
                            None
                        }
                    };
                }
                6 => {
                    match (match_1, match_4) {
                        (2, 3) => mappings.insert(set, 0),
                        (1, 3) => mappings.insert(set, 6),
                        (2, 4) => mappings.insert(set, 9),
                        _ => {
                            println!("6 {:?}", (match_1, match_4));
                            None
                        }
                    };
                }
                _ => {}
            }
        }

        for (i, &d) in output.iter().rev().enumerate() {
            c += 10u64.pow(i as u32) * mappings.get(d.as_bytes()).unwrap();
        }
    }
    c
}

fn count_same<T>(a: &[T], b: &[T]) -> usize
where
    T: std::cmp::PartialEq,
{
    a.iter().filter(|x| b.contains(x)).count()
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
