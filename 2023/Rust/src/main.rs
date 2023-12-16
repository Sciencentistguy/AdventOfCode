#![feature(array_chunks)]
#![feature(array_windows)]
#![feature(inline_const)]
#![feature(pattern)]
#![feature(never_type)]

use std::process::ExitCode;

use clap::Parser;
use color_eyre::eyre::Result;
use emergence::AoC;

mod common;

mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;
mod day09;
mod day10;
mod day11;
mod day12;
mod day13;

fn main() -> Result<ExitCode> {
    color_eyre::install()?;

    let args = Args::parse();

    let aoc = AoC::new(2023)?;

    match args.day {
        1 => day01::run(&aoc.read_or_fetch(1)?),
        2 => day02::run(&aoc.read_or_fetch(2)?),
        3 => day03::run(&aoc.read_or_fetch(3)?),
        4 => day04::run(&aoc.read_or_fetch(4)?),
        5 => day05::run(&aoc.read_or_fetch(5)?),
        6 => day06::run(&aoc.read_or_fetch(6)?),
        7 => day07::run(&aoc.read_or_fetch(7)?),
        8 => day08::run(&aoc.read_or_fetch(8)?),
        9 => day09::run(&aoc.read_or_fetch(9)?),
        10 => day10::run(&aoc.read_or_fetch(10)?),
        11 => day11::run(&aoc.read_or_fetch(11)?),
        12 => day12::run(&aoc.read_or_fetch(12)?),
        13 => day13::run(&aoc.read_or_fetch(13)?),
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
