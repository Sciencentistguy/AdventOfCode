use memchr::memchr;

#[derive(Debug)]
pub struct Game {
    id: u32,
    red: u32,
    green: u32,
    blue: u32,
}
impl Game {
    fn power(&self) -> u32 {
        self.red * self.green * self.blue
    }
}

pub fn parse(inpt: &str) -> Vec<Game> {
    inpt.trim()
        .lines()
        .enumerate()
        .map(|(id, line)| {
            let idx = memchr(b':', line.as_bytes()).unwrap();
            let line = &line[idx + 1..];
            let (red, green, blue) = line
                .split([';', ','])
                .map(|x| x.trim().split_once(' ').unwrap())
                .map(|(num, colour)| {
                    let num = num.parse::<u32>().unwrap();
                    (num, colour)
                })
                .fold(
                    (0, 0, 0),
                    |(mut red, mut green, mut blue), (num, colour)| {
                        match colour {
                            "red" => red = red.max(num),
                            "green" => green = green.max(num),
                            "blue" => blue = blue.max(num),
                            _ => unreachable!(),
                        };
                        (red, green, blue)
                    },
                );
            Game {
                id: id as u32 + 1,
                red,
                green,
                blue,
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
            if game.red > RED_COUNT || game.green > GREEN_COUNT || game.blue > BLUE_COUNT {
                return None;
            }
            Some(game.id)
        })
        .sum()
}

pub fn part2(games: &[Game]) -> u32 {
    games.iter().map(|game| game.power()).sum()
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
