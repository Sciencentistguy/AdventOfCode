use std::time::Instant;

use eyre::Result;
use nohash_hasher::IntMap;

fn parse(input: &str) -> Vec<Vec<u64>> {
    input
        .lines()
        .map(|l| l.chars().map(|x| x.to_digit(10).unwrap() as u64).collect())
        .collect()
}

fn part1(input: &[Vec<u64>]) -> u64 {
    let mut c = 0;
    let x_size = input[0].len();
    let y_size = input.len();
    for (y, row) in input.iter().enumerate() {
        for (x, val) in row.iter().enumerate() {
            let surrounding_locations = [
                (x.wrapping_sub(1), y),
                (x + 1, y),
                (x, y.wrapping_sub(1)),
                (x, y + 1),
            ];

            let surroundings = surrounding_locations.map(|(x2, y2)| {
                if x2 < x_size && y2 < y_size {
                    input[y2][x2]
                } else {
                    u64::MAX
                }
            });

            if surroundings.into_iter().all(|x| val < &x) {
                c += val + 1;
            }
        }
    }
    c
}

fn part2(input: &[Vec<u64>]) -> u64 {
    let mut basins = IntMap::default();
    basins.reserve(256);
    let x_size = input[0].len();
    let y_size = input.len();

    for (y, row) in input.iter().enumerate() {
        for (x, &val) in row.iter().enumerate() {
            if val != 9 {
                let basin = find_basin_size(x, y, x_size, y_size, input);
                *basins.entry(basin).or_insert(0) += 1;
            }
        }
    }

    let mut vals: Vec<_> = basins.values().into_iter().collect();
    vals.sort_unstable();
    vals.into_iter().rev().take(3).product()
}

fn find_basin_size(x_1: usize, y_1: usize, x_max: usize, y_max: usize, input: &[Vec<u64>]) -> u64 {
    for (x_2, y_2) in [
        (x_1.wrapping_sub(1), y_1),
        (x_1 + 1, y_1),
        (x_1, y_1.wrapping_sub(1)),
        (x_1, y_1 + 1),
    ] {
        if x_2 < x_max && y_2 < y_max && input[y_2][x_2] < input[y_1][x_1] {
            return find_basin_size(x_2, y_2, x_max, y_max, input);
        }
    }
    (x_1 * y_max + y_1) as u64
}

pub fn run(input: String) -> Result<()> {
    let parsed = parse(input.as_str());
    let start = Instant::now();
    let part1 = part1(&parsed);
    let p1_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 09 (part 1) is {}. Took {:?}",
        part1, p1_time
    );
    let start = Instant::now();
    let part2 = part2(&parsed);
    let p2_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 09 (part 2) is {}. Took {:?}",
        part2, p2_time
    );

    Ok(())
}
