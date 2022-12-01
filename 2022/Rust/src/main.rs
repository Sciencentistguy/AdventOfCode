use std::error::Error;

use emergence::AoC;

mod day01;

fn main() -> Result<(), Box<dyn Error>> {
    let aoc = AoC::new(2022)?;

    day01::run(aoc.read_or_fetch(1)?);

    Ok(())
}
