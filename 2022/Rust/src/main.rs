use std::{error::Error, process::ExitCode};

use clap::Parser;
use emergence::AoC;

mod day01;

fn main() -> Result<ExitCode, Box<dyn Error>> {
    let args = Opt::parse();

    let aoc = AoC::new(2022)?;

    match args.day {
        1 => day01::run(aoc.read_or_fetch(1)?),

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
