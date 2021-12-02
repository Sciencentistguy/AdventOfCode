#![allow(dead_code)]
#![allow(unused_variables)]

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
    day01::run(emergence::fetch(2020, 1)?);
    day02::run(emergence::fetch(2020, 2)?);
    day03::run(emergence::fetch(2020, 3)?);
    day04::run(emergence::fetch(2020, 4)?);
    day05::run(emergence::fetch(2020, 5)?);
    day06::run(emergence::fetch(2020, 6)?);
    day07::run(emergence::fetch(2020, 7)?);
    day08::run(emergence::fetch(2020, 8)?);
    day09::run(emergence::fetch(2020, 9)?);
    day10::run(emergence::fetch(2020, 10)?);
    day11::run(emergence::fetch(2020, 11)?);
    day12::run(emergence::fetch(2020, 12)?);
    day13::run(emergence::fetch(2020, 13)?);
    day14::run(emergence::fetch(2020, 14)?);
    // day15::run(emergence::fetch(2020, 15)?);
    day16::run(emergence::fetch(2020, 16)?);
    day17::run(emergence::fetch(2020, 17)?);
    Ok(())
}
