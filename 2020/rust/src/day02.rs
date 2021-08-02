use std::time::{Duration, Instant};

use nom::{
    bytes::complete::take_while,
    character::{
        complete::{anychar, char},
        is_digit,
    },
    combinator::map_res,
    IResult,
};

pub struct Password<'a> {
    begin: u64,
    end: u64,
    letter: u8,
    string: &'a str,
}

impl Password<'_> {
    fn valid_part1(&self) -> bool {
        let count = self.string.bytes().filter(|c| self.letter == *c).count() as u64;
        count >= self.begin && count <= self.end
    }

    fn valid_part2(&self) -> bool {
        (self.string.as_bytes()[(self.begin - 1) as usize] == self.letter)
            != (self.string.as_bytes()[(self.end - 1) as usize] == self.letter)
    }
}

fn p(input: &str) -> IResult<&str, Password> {
    let mut parse_num = map_res(take_while(|c| is_digit(c as u8)), &str::parse::<u64>);
    let (input, num1) = parse_num(input)?;
    let (input, _) = char('-')(input)?;
    let (input, num2) = parse_num(input)?;
    let (input, _) = char(' ')(input)?;
    let (input, letter) = anychar(input)?;
    let (input, _) = char(':')(input)?;
    let (input, _) = char(' ')(input)?;

    nom::IResult::Ok((
        "",
        Password {
            begin: num1,
            end: num2,
            letter: letter as u8,
            string: input,
        },
    ))
}

fn parse_input(input: &str) -> (Vec<Password>, Duration) {
    let start = Instant::now();
    let res = input.lines().map(p).map(|x| x.unwrap().1).collect();
    let end = Instant::now();
    (res, end - start)
}

fn solve_part1(input: &[Password<'_>]) -> (usize, Duration) {
    let start = Instant::now();
    let res = input.iter().filter(|x| x.valid_part1()).count();
    let end = Instant::now();
    (res, end - start)
}

fn solve_part2(input: &[Password<'_>]) -> (usize, Duration) {
    let start = Instant::now();
    let res = input.iter().filter(|x| x.valid_part2()).count();
    let end = Instant::now();
    (res, end - start)
}

pub fn run(input: String) {
    let (parsed_input, time) = parse_input(&input);
    println!("Day 02, parsing input took {}ns", time.as_nanos());
    let (p1, time) = solve_part1(&parsed_input);
    println!("Day 02, part 1: {}. Took {}ns", p1, time.as_nanos());
    let (p2, time) = solve_part2(&parsed_input);
    println!("Day 02, part 2: {}. Took {}ns", p2, time.as_nanos());
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &str = "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
";

    #[test]
    fn day02_part1() {
        let parsed = parse_input(INPUT).0;
        assert_eq!(solve_part1(&parsed).0, 2);
    }

    #[test]
    fn day02_part2() {
        let parsed = parse_input(INPUT).0;
        assert_eq!(solve_part2(&parsed).0, 1);
    }
}
