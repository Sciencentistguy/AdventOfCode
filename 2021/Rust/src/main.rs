use eyre::{Context, Result};

mod day01;
mod day02;
mod day03;
mod day04;
mod day05;
mod day06;
mod day07;
mod day08;

fn main() -> Result<()> {
    day01::run(emergence::fetch(2021, 1).wrap_err("failed to fetch")?)?;
    day02::run(emergence::fetch(2021, 2).wrap_err("failed to fetch")?)?;
    day03::run(emergence::fetch(2021, 3).wrap_err("failed to fetch")?)?;
    day04::run(emergence::fetch(2021, 4).wrap_err("failed to fetch")?)?;
    day05::run(emergence::fetch(2021, 5).wrap_err("failed to fetch")?)?;
    day06::run(emergence::fetch(2021, 6).wrap_err("failed to fetch")?)?;
    day07::run(emergence::fetch(2021, 7).wrap_err("failed to fetch")?)?;
    day08::run(emergence::fetch(2021, 8).wrap_err("failed to fetch")?)?;

    Ok(())
}
