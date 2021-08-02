mod day01;
mod day02;

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
    Ok(())
}
