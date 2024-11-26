#![feature(isqrt)]

use std::process::ExitCode;

use clap::Parser;
use emergence::AoC;

mod day20;
mod day23;

type Result<T> = color_eyre::Result<T>;

fn main() -> Result<ExitCode> {
    color_eyre::install()?;

    let args = Args::parse();

    let aoc = AoC::new(2015)?;

    match args.day {
        20 => day20::run(&aoc.read_or_fetch(20)?),
        23 => day23::run(&aoc.read_or_fetch(23)?),

        day => {
            eprintln!("Day {day} does not exist / is not implemented yet");
            return Ok(ExitCode::FAILURE);
        }
    }?;

    Ok(ExitCode::SUCCESS)
}

#[derive(Parser)]
struct Args {
    day: usize,
}
