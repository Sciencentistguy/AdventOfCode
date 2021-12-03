use eyre::{Context, Result};

mod day01;
mod day02;
mod day03;

fn main() -> Result<()> {
    day01::run(emergence::fetch(2021, 1).wrap_err("failed to fetch")?)?;
    day02::run(emergence::fetch(2021, 2).wrap_err("failed to fetch")?)?;
    day03::run(emergence::fetch(2021, 3).wrap_err("failed to fetch")?)?;

    Ok(())
}
