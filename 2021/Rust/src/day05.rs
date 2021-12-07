use std::{collections::HashMap, time::Instant};

use eyre::Result;

type Line = ((i64, i64), (i64, i64));

fn parse(input: &str) -> Result<Vec<Line>> {
    Ok(input
        .lines()
        .map(|line| {
            let mut parts = line.split(" -> ");
            let mut a = parts.next().unwrap().split(',').map(|x| x.parse().unwrap());
            let mut b = parts.next().unwrap().split(',').map(|x| x.parse().unwrap());
            (
                (a.next().unwrap(), a.next().unwrap()),
                (b.next().unwrap(), b.next().unwrap()),
            )
        })
        .collect())
}

fn part1(input: &[Line]) -> i64 {
    let mut lines = HashMap::with_capacity(100_000);

    for &((x1, y1), (x2, y2)) in input {
        if !(x1 == x2 || y1 == y2) {
            continue;
        }
        for x in x1.min(x2)..=x1.max(x2) {
            for y in y1.min(y2)..=y1.max(y2) {
                *lines.entry((x, y)).or_insert(0) += 1;
            }
        }
    }

    lines.values().filter(|&&p| p > 1).count() as i64
}

fn part2(input: &[Line]) -> i64 {
    let mut lines = HashMap::with_capacity(170_000);
    for &((mut x1, mut y1), (x2, y2)) in input {
        if x1 == x2 || y1 == y2 {
            for x in x1.min(x2)..=x1.max(x2) {
                for y in y1.min(y2)..=y1.max(y2) {
                    *lines.entry((x, y)).or_insert(0) += 1;
                }
            }
        } else {
            let dx = match x2 > x1 {
                true => 1,
                false => -1,
            };
            let dy = match y2 > y1 {
                true => 1,
                false => -1,
            };

            while x1 != x2 + dx {
                *lines.entry((x1, y1)).or_insert(0) += 1;
                x1 += dx;
                y1 += dy;
            }
        }
    }

    lines.values().filter(|&&p| p > 1).count() as i64
}

pub fn run(input: String) -> Result<()> {
    let parsed = parse(input.as_str())?;
    let start = Instant::now();
    let part1 = part1(&parsed);
    let p1_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 05 (part 1) is {}. Took {:?}",
        part1, p1_time
    );
    let start = Instant::now();
    let part2 = part2(&parsed);
    let p2_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 05 (part 2) is {}. Took {:?}",
        part2, p2_time
    );

    Ok(())
}
