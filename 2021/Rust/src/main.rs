#![feature(array_windows)]

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

fn main() -> Result<()> {
    day01::run(emergence::fetch(2021, 1).wrap_err("failed to fetch")?)?;
    day02::run(emergence::fetch(2021, 2).wrap_err("failed to fetch")?)?;
    day03::run(emergence::fetch(2021, 3).wrap_err("failed to fetch")?)?;
    day04::run(emergence::fetch(2021, 4).wrap_err("failed to fetch")?)?;
    day05::run(emergence::fetch(2021, 5).wrap_err("failed to fetch")?)?;
    day06::run(emergence::fetch(2021, 6).wrap_err("failed to fetch")?)?;
    day07::run(emergence::fetch(2021, 7).wrap_err("failed to fetch")?)?;
    day08::run(emergence::fetch(2021, 8).wrap_err("failed to fetch")?)?;
    day09::run(emergence::fetch(2021, 9).wrap_err("failed to fetch")?)?;
    day10::run(emergence::fetch(2021, 10).wrap_err("failed to fetch")?)?;
    day11::run(emergence::fetch(2021, 11).wrap_err("failed to fetch")?)?;
    day12::run(emergence::fetch(2021, 12).wrap_err("failed to fetch")?)?;
    day13::run(emergence::fetch(2021, 13).wrap_err("failed to fetch")?)?;
    day14::run(emergence::fetch(2021, 14).wrap_err("failed to fetch")?)?;
    day15::run(emergence::fetch(2021, 15).wrap_err("failed to fetch")?)?;

    Ok(())
}
