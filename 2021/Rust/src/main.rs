use eyre::{Context, Result};

mod day01;
mod day02;

fn main() -> Result<()> {
    day01::run(emergence::fetch(2021, 1).wrap_err("failed to fetch")?)?;
    day02::run(emergence::fetch(2021, 2).wrap_err("failed to fetch")?)?;

    Ok(())
}
