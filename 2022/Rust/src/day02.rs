pub struct Game {
    opponent: Play,
    strategy: Strategy,
}

#[derive(Clone, Copy)]
pub enum Play {
    Rock = 1,
    Paper = 2,
    Scissors = 3,
}

#[derive(Clone, Copy)]
pub enum Strategy {
    X,
    Y,
    Z,
}

impl Play {
    fn score_against(self, other: Self) -> usize {
        match (other, self) {
            (Play::Scissors, Play::Rock)
            | (Play::Rock, Play::Paper)
            | (Play::Paper, Play::Scissors) => 6,
            (Play::Scissors, Play::Scissors)
            | (Play::Rock, Play::Rock)
            | (Play::Paper, Play::Paper) => 3,
            _ => 0,
        }
    }

    const fn loses_to(self) -> Self {
        match self {
            Play::Rock => Play::Paper,
            Play::Paper => Play::Scissors,
            Play::Scissors => Play::Rock,
        }
    }

    const fn draws(self) -> Self {
        self
    }

    const fn beats(self) -> Self {
        self.loses_to().loses_to()
    }
}

pub fn parse(input: &str) -> Vec<Game> {
    input
        .lines()
        .map(|line| {
            let mut chars = line.chars();
            let opponent = chars.next().unwrap();
            let _ = chars.next().unwrap();
            let strategy = chars.next().unwrap();

            let opponent = match opponent {
                'A' => Play::Rock,
                'B' => Play::Paper,
                'C' => Play::Scissors,
                _ => unreachable!(),
            };
            let strategy = match strategy {
                'X' => Strategy::X,
                'Y' => Strategy::Y,
                'Z' => Strategy::Z,
                _ => unreachable!(),
            };
            Game { opponent, strategy }
        })
        .collect()
}

pub fn part1(parsed: &[Game]) -> usize {
    parsed
        .iter()
        .map(|&Game { opponent, strategy }| {
            let to_play = match strategy {
                Strategy::X => Play::Rock,
                Strategy::Y => Play::Paper,
                Strategy::Z => Play::Scissors,
            };

            to_play as usize + to_play.score_against(opponent)
        })
        .sum()
}

pub fn part2(parsed: &[Game]) -> usize {
    parsed
        .iter()
        .map(|&Game { opponent, strategy }| {
            let to_play = match strategy {
                Strategy::X => opponent.beats(),
                Strategy::Y => opponent.draws(),
                Strategy::Z => opponent.loses_to(),
            };

            (to_play as usize) + to_play.score_against(opponent)
        })
        .sum()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "A Y
B X
C Z";

    #[test]
    fn test_part1() {
        let parsed = parse(INPUT);
        assert_eq!(15, part1(&parsed));
    }

    #[test]
    fn test_part2() {
        let parsed = parse(INPUT);
        assert_eq!(12, part2(&parsed));
    }
}
