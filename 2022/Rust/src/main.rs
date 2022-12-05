use std::{error::Error, process::ExitCode};

use clap::Parser;
use emergence::AoC;

fn main() -> Result<ExitCode, Box<dyn Error>> {
    let args = Opt::parse();

    let aoc = AoC::new(2022)?;

    match args.day {
        1 => aoc_2022::day01::run(&aoc.read_or_fetch(args.day)?),
        2 => aoc_2022::day02::run(&aoc.read_or_fetch(args.day)?),
        3 => aoc_2022::day03::run(&aoc.read_or_fetch(args.day)?),
        4 => aoc_2022::day04::run(&aoc.read_or_fetch(args.day)?),
        5 => aoc_2022::day05::run(&aoc.read_or_fetch(args.day)?),

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
