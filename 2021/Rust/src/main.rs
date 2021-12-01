use eyre::{Result, Context};

mod day01;

fn main() -> Result<()> {
    day01::run(emergence::fetch(2021, 1).wrap_err("failed to fetch")?)?;

    Ok(())
}
