use color_eyre::eyre::Result;
use emergence::AoC;

mod day01;

fn main() -> Result<()> {
    color_eyre::install()?;

    let aoc = AoC::new(2023)?;

    day01::run(&aoc.read_or_fetch(1)?);



    todo!();

    Ok(())
}
