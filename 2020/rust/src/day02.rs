use aoc_runner_derive::*;
use nom::{
    bytes::complete::take_while,
    character::{
        complete::{anychar, char},
        is_digit,
    },
    combinator::map_res,
    IResult,
};

pub struct PasswordWithSpec {
    begin: u64,
    end: u64,
    letter: u8,
    string: String,
}

impl PasswordWithSpec {
    fn valid_part1(&self) -> bool {
        let count = self.string.bytes().filter(|c| self.letter == *c).count() as u64;
        count >= self.begin && count <= self.end
    }

    fn valid_part2(&self) -> bool {
        (self.string.as_bytes()[(self.begin - 1) as usize] == self.letter)
            != (self.string.as_bytes()[(self.end - 1) as usize] == self.letter)
    }
}

fn p(input: &str) -> IResult<&str, PasswordWithSpec> {
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
        PasswordWithSpec {
            begin: num1,
            end: num2,
            letter: letter as u8,
            string: input.to_owned(), // cargo-aoc is bad and doesn't support lifetimes in function
                                      // signatures
        },
    ))
}

#[aoc_generator(day02)]
pub fn parse_input(input: &str) -> Vec<PasswordWithSpec> {
    input.lines().map(p).map(|x| x.unwrap().1).collect()
}

#[aoc(day02, part1)]
pub fn solve_part1(input: &[PasswordWithSpec]) -> usize {
    input.iter().filter(|x| x.valid_part1()).count()
}

#[aoc(day02, part2)]
pub fn solve_part2(input: &[PasswordWithSpec]) -> usize {
    input.iter().filter(|x| x.valid_part2()).count()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn day02_part1_example() {
        let input = "1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
";
        let parsed = parse_input(input);
        let p1 = solve_part1(&parsed);
        assert_eq!(p1, 2);
    }
}
