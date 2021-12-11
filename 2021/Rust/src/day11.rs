use std::{hint::unreachable_unchecked, time::Instant};

use eyre::Result;

fn parse(input: &str) -> [[i64; 10]; 10] {
    let mut out: [[i64; 10]; 10] = Default::default();

    for (i, line) in input.lines().enumerate() {
        for (j, c) in line.chars().enumerate() {
            out[i][j] = c.to_digit(10).unwrap() as _;
        }
    }

    out
}

fn part1(input: &[[i64; 10]; 10]) -> i64 {
    const FLOOR_SIZE: usize = 10;

    let mut floor = input.to_owned();
    let mut out = 0;

    for _ in 0..100 {
        for row in &mut floor {
            for energy_level in row {
                *energy_level += 1;
            }
        }

        for y in 0..FLOOR_SIZE {
            for x in 0..FLOOR_SIZE {
                if floor[y][x] > 9 {
                    out += recursively_flash(x, y, &mut floor);
                }
            }
        }

        for row in &mut floor {
            for energy_level in row {
                if *energy_level == -1 {
                    *energy_level = 0;
                }
            }
        }
    }

    out
}

fn part2(input: &[[i64; 10]; 10]) -> i64 {
    const FLOOR_SIZE: usize = 10;

    let mut floor = input.to_owned();

    for day in 1.. {
        for row in &mut floor {
            for energy_level in row {
                *energy_level += 1;
            }
        }

        for y in 0..FLOOR_SIZE {
            for x in 0..FLOOR_SIZE {
                if floor[y][x] > 9 {
                    recursively_flash(x, y, &mut floor);
                }
            }
        }

        for row in &mut floor {
            for energy_level in row {
                if *energy_level == -1 {
                    *energy_level = 0;
                }
            }
        }

        if floor.iter().flat_map(|x| x.iter()).all(|&x| x == 0) {
            return day;
        }
    }
    // `for day in 1..` will never exit
    unsafe { unreachable_unchecked() }
}

fn recursively_flash(centre_x: usize, centre_y: usize, floor: &mut [[i64; 10]; 10]) -> i64 {
    let mut flash_count = 1;

    floor[centre_y][centre_x] = -1;

    for dy in -1..=1 {
        for dx in -1..=1 {
            let x = (centre_x as isize + dx) as usize;
            let y = (centre_y as isize + dy) as usize;
            if x < 10 && y < 10 && floor[y][x] != -1 {
                floor[y][x] += 1;
                if floor[y][x] > 9 {
                    flash_count += recursively_flash(x, y, floor);
                }
            }
        }
    }
    flash_count
}

pub fn run(input: String) -> Result<()> {
    let parsed = parse(input.as_str());
    let start = Instant::now();
    let part1 = part1(&parsed);
    let p1_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 11 (part 1) is {}. Took {:?}",
        part1, p1_time
    );
    let start = Instant::now();
    let part2 = part2(&parsed);
    let p2_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 11 (part 2) is {}. Took {:?}",
        part2, p2_time
    );

    Ok(())
}
