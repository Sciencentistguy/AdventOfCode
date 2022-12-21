use std::{error::Error, process::ExitCode};

use aoc_2022::*;
use clap::Parser;
use emergence::AoC;

fn main() -> Result<ExitCode, Box<dyn Error>> {
    let args = Opt::parse();

    #[cfg(miri)]
    let aoc = AoC::with_path(2022, "/home/jamie/.aoc")?;

    #[cfg(not(miri))]
    let aoc = AoC::new(2022)?;

    match args.day {
        1 => day01::run(&aoc.read_or_fetch(args.day)?),
        2 => day02::run(&aoc.read_or_fetch(args.day)?),
        3 => day03::run(&aoc.read_or_fetch(args.day)?),
        4 => day04::run(&aoc.read_or_fetch(args.day)?),
        5 => day05::run(&aoc.read_or_fetch(args.day)?),
        6 => day06::run(&aoc.read_or_fetch(args.day)?),
        7 => day07::run(&aoc.read_or_fetch(args.day)?),
        8 => day08::run(&aoc.read_or_fetch(args.day)?),
        9 => day09::run(&aoc.read_or_fetch(args.day)?),
        10 => day10::run(&aoc.read_or_fetch(args.day)?),
        11 => day11::run(&aoc.read_or_fetch(args.day)?),
        12 => day12::run(&aoc.read_or_fetch(args.day)?),
        13 => day13::run(&aoc.read_or_fetch(args.day)?),
        14 => day14::run(&aoc.read_or_fetch(args.day)?),
        15 => day15::run(&aoc.read_or_fetch(args.day)?),
        16 => day16::run(&aoc.read_or_fetch(args.day)?),
        17 => day17::run(&aoc.read_or_fetch(args.day)?),
        18 => day18::run(&aoc.read_or_fetch(args.day)?),

        20 => day20::run(&aoc.read_or_fetch(args.day)?),
        21 => day21::run(&aoc.read_or_fetch(args.day)?),

        day => {
            eprintln!("Day {day} does not exist / is not implemented yet");
            return Ok(ExitCode::FAILURE);
        }
    }

    Ok(ExitCode::SUCCESS)
}

#[derive(Parser)]
struct Opt {
    day: usize,
}
