#![feature(portable_simd)]
#![feature(iter_array_chunks)]

use std::process::ExitCode;

use clap::Parser;
use emergence::AoC;

type Result<T> = color_eyre::Result<T>;

mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;
mod day09;

fn main() -> Result<ExitCode> {
    let opt = Opt::parse();

    let aoc = AoC::new(2025)?;

    match opt.day {
        1 => day01::run(&aoc.read_or_fetch(1)?),
        2 => day02::run(&aoc.read_or_fetch(2)?),
        3 => day03::run(&aoc.read_or_fetch(3)?),
        4 => day04::run(&aoc.read_or_fetch(4)?),
        5 => day05::run(&aoc.read_or_fetch(5)?),
        6 => day06::run(&aoc.read_or_fetch(6)?),
        7 => day07::run(&aoc.read_or_fetch(7)?),
        8 => day08::run(&aoc.read_or_fetch(8)?),
        9 => day09::run(&aoc.read_or_fetch(9)?),
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
