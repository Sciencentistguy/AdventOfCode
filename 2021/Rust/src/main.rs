#![feature(array_windows)]

use emergence::AoC;
use eyre::{Context, Result};

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
mod day14;
mod day15;
mod day16;

fn main() -> Result<()> {
    let aoc = AoC::new(2021)?;

    day01::run(aoc.read_or_fetch(1).wrap_err("failed to fetch")?)?;
    day02::run(aoc.read_or_fetch(2).wrap_err("failed to fetch")?)?;
    day03::run(aoc.read_or_fetch(3).wrap_err("failed to fetch")?)?;
    day04::run(aoc.read_or_fetch(4).wrap_err("failed to fetch")?)?;
    day05::run(aoc.read_or_fetch(5).wrap_err("failed to fetch")?)?;
    day06::run(aoc.read_or_fetch(6).wrap_err("failed to fetch")?)?;
    day07::run(aoc.read_or_fetch(7).wrap_err("failed to fetch")?)?;
    day08::run(aoc.read_or_fetch(8).wrap_err("failed to fetch")?)?;
    day09::run(aoc.read_or_fetch(9).wrap_err("failed to fetch")?)?;
    day10::run(aoc.read_or_fetch(10).wrap_err("failed to fetch")?)?;
    day11::run(aoc.read_or_fetch(11).wrap_err("failed to fetch")?)?;
    day12::run(aoc.read_or_fetch(12).wrap_err("failed to fetch")?)?;
    day13::run(aoc.read_or_fetch(13).wrap_err("failed to fetch")?)?;
    day14::run(aoc.read_or_fetch(14).wrap_err("failed to fetch")?)?;
    day15::run(aoc.read_or_fetch(15).wrap_err("failed to fetch")?)?;
    day16::run(aoc.read_or_fetch(16).wrap_err("failed to fetch")?)?;

    Ok(())
}
