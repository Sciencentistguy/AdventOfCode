use std::process::ExitCode;

use color_eyre::eyre::Result;
use emergence::AoC;
use clap::Parser;

mod day01;

fn main() -> Result<ExitCode> {
    color_eyre::install()?;

    let args = Args::parse();

    let aoc = AoC::new(2023)?;

    match args.day {
        1 => day01::run(&aoc.read_or_fetch(1)?),
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
