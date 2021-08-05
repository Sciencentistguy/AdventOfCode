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

fn open_input(day: u8) -> std::io::Result<String> {
    let mut path = std::env::current_dir()?;
    path.pop();
    path.push("Inputs");
    path.push(format!("day_{:02}.txt", day));
    println!("{:?}", path);
    std::fs::read_to_string(path)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    day01::run(open_input(1)?);
    day02::run(open_input(2)?);
    day03::run(open_input(3)?);
    day04::run(open_input(4)?);
    day05::run(open_input(5)?);
    day06::run(open_input(6)?);
    day07::run(open_input(7)?);
    day08::run(open_input(8)?);
    day09::run(open_input(9)?);
    day10::run(open_input(10)?);
    Ok(())
}
