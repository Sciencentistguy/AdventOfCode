#![allow(dead_code)]
#![allow(unused_variables)]

use emergence::AoC;
use eyre::Result;

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
mod day17;

fn main() -> Result<()> {
    let aoc = AoC::new(2020)?;

    day01::run(aoc.read_or_fetch(1)?);
    day02::run(aoc.read_or_fetch(2)?);
    day03::run(aoc.read_or_fetch(3)?);
    day04::run(aoc.read_or_fetch(4)?);
    day05::run(aoc.read_or_fetch(5)?);
    day06::run(aoc.read_or_fetch(6)?);
    day07::run(aoc.read_or_fetch(7)?);
    day08::run(aoc.read_or_fetch(8)?);
    day09::run(aoc.read_or_fetch(9)?);
    day10::run(aoc.read_or_fetch(10)?);
    day11::run(aoc.read_or_fetch(11)?);
    day12::run(aoc.read_or_fetch(12)?);
    day13::run(aoc.read_or_fetch(13)?);
    day14::run(aoc.read_or_fetch(14)?);
    // day15::run(aoc.read_or_fetch(15)?);
    day16::run(aoc.read_or_fetch(16)?);
    day17::run(aoc.read_or_fetch(17)?);
    Ok(())
}
