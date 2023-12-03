use std::process::ExitCode;

use color_eyre::eyre::Result;
use emergence::AoC;
use clap::Parser;

mod day01;
mod day02;
mod day03;

fn main() -> Result<ExitCode> {
    color_eyre::install()?;

    let args = Args::parse();

    let aoc = AoC::new(2023)?;

    match args.day {
        1 => day01::run(&aoc.read_or_fetch(1)?),
        2 => day02::run(&aoc.read_or_fetch(2)?),
        3 => day03::run(&aoc.read_or_fetch(3)?),
        day => {
            eprintln!("Day {day} does not exist / is not implemented yet");
            return Ok(ExitCode::FAILURE);
        }
    }

    Ok(ExitCode::SUCCESS)
}

#[derive(Parser)]
struct Args {
    day: usize,
}
