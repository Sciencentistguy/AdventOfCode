use std::error::Error;

use color_eyre::eyre::Result;
use emergence::AoC;

fn main() -> Result<()> {
    color_eyre::install()?;

    let aoc = AoC::new(2023)?;

    let d1 = aoc.read_or_fetch(1)?;

    todo!();

    Ok(())
}
