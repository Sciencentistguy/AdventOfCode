use smallvec::SmallVec;

use self::parse::pDraws;

#[derive(Debug)]
pub struct Game {
    id: u32,
    draws: Draws,
}

type Draws = SmallVec<Draw, 8>;

impl Game {
    fn reds(&self) -> impl Iterator<Item = u32> + '_ {
        self.draws.iter().map(|draw| draw.red)
    }

    fn greens(&self) -> impl Iterator<Item = u32> + '_ {
        self.draws.iter().map(|draw| draw.green)
    }

    fn blues(&self) -> impl Iterator<Item = u32> + '_ {
        self.draws.iter().map(|draw| draw.blue)
    }

    #[inline]
    fn maximum_draw(&self) -> Draw {
        Draw {
            red: self.reds().max().unwrap_or(0),
            green: self.greens().max().unwrap_or(0),
            blue: self.blues().max().unwrap_or(0),
        }
    }
}

#[derive(Debug, Default)]
struct Draw {
    red: u32,
    green: u32,
    blue: u32,
}

impl Draw {
    fn power(&self) -> u32 {
        self.red * self.green * self.blue
    }
}

#[derive(Debug, Clone, Copy)]
enum Colour {
    Red,
    Green,
    Blue,
}

#[allow(non_snake_case)]
mod parse {
    use super::{Colour, Draw, Draws};
    use nom::{
        branch::alt,
        bytes::complete::tag,
        character::complete::{digit1, space0},
        error::{ErrorKind, ParseError},
        IResult, InputLength, Parser,
    };
    use smallvec::SmallVec;

    fn pDraw(i: &str) -> IResult<&str, Draw> {
        let (i, draws): (&str, SmallVec<_, 3>) = smallSepList(tag(", "), pSingleDraw)(i)?;
        let mut ret = Draw::default();
        for draw in draws {
            match draw.0 {
                Colour::Red => ret.red = draw.1,
                Colour::Green => ret.green = draw.1,
                Colour::Blue => ret.blue = draw.1,
            };
        }
        Ok((i, ret))
    }

    fn pSingleDraw(i: &str) -> IResult<&str, (Colour, u32)> {
        let (i, x) = digit1(i)?;
        let x = x.parse().unwrap();
        let (i, _) = space0(i)?;
        let (i, tag) = alt((tag("red"), tag("green"), tag("blue")))(i)?;
        let tag = match tag {
            "red" => Colour::Red,
            "green" => Colour::Green,
            "blue" => Colour::Blue,
            _ => unreachable!(),
        };

        Ok((i, (tag, x)))
    }

    pub(super) fn pDraws(i: &str) -> IResult<&str, Draws> {
        smallSepList(tag("; "), pDraw)(i)
    }

    fn smallSepList<T, Input, Error, const N: usize>(
        mut sep: impl Fn(Input) -> IResult<Input, Input, Error>,
        mut f: impl Fn(Input) -> IResult<Input, T, Error>,
    ) -> impl FnMut(Input) -> IResult<Input, SmallVec<T, N>, Error>
    where
        Input: Clone,
        Input: InputLength,
        Error: ParseError<Input>,
    {
        use nom::Err;

        move |mut i: Input| {
            let mut res = SmallVec::<T, N>::new();

            // Parse the first element
            match f.parse(i.clone()) {
                Err(e) => return Err(e),
                Ok((i1, o)) => {
                    res.push(o);
                    i = i1;
                }
            }

            loop {
                let len = i.input_len();
                match sep.parse(i.clone()) {
                    Err(Err::Error(_)) => return Ok((i, res)),
                    Err(e) => return Err(e),
                    Ok((i1, _)) => {
                        // infinite loop check: the parser must always consume
                        if i1.input_len() == len {
                            return Err(Err::Error(Error::from_error_kind(
                                i1,
                                ErrorKind::SeparatedList,
                            )));
                        }

                        match f.parse(i1.clone()) {
                            Err(Err::Error(_)) => return Ok((i, res)),
                            Err(e) => return Err(e),
                            Ok((i2, o)) => {
                                res.push(o);
                                i = i2;
                            }
                        }
                    }
                }
            }
        }
    }
}

pub fn parse(inpt: &str) -> Vec<Game> {
    inpt.trim()
        .split('\n')
        .enumerate()
        .map(|(id, x)| {
            let x = x.trim();
            let i = x.find(':').unwrap();
            let x = &x[i + 1..].trim();

            Game {
                id: id as u32 + 1,
                draws: pDraws(x.trim()).unwrap().1,
            }
        })
        .collect()
}

pub fn part1(games: &[Game]) -> u32 {
    const RED_COUNT: u32 = 12;
    const GREEN_COUNT: u32 = 13;
    const BLUE_COUNT: u32 = 14;
    games
        .iter()
        .filter_map(|game| {
            for draw in &game.draws {
                if draw.red > RED_COUNT || draw.green > GREEN_COUNT || draw.blue > BLUE_COUNT {
                    return None;
                }
            }
            Some(game.id)
        })
        .sum()
}

pub fn part2(games: &[Game]) -> u32 {
    games.iter().map(|game| game.maximum_draw().power()).sum()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &str = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
    Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
    Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
    Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
    Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 8);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 2286);
    }
}
