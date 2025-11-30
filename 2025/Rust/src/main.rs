use std::process::ExitCode;

use clap::Parser;
use emergence::AoC;

type Result<T> = color_eyre::Result<T>;

mod day01;

fn main() -> Result<ExitCode> {
    let opt = Opt::parse();

    let aoc = AoC::new(2025)?;

    match opt.day {
        1 => day01::run(&aoc.read_or_fetch(1)?),
        _ => {
            eprintln!();
            return Ok(ExitCode::FAILURE);
        }
    }

    Ok(ExitCode::SUCCESS)
}

#[derive(Parser)]
struct Opt {
    day: u32,
}
