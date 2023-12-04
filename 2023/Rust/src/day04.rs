use collect_slice::CollectSlice;

#[cfg(not(test))]
const WINNING_SIZE: usize = 10;
#[cfg(not(test))]
const SCRATCH_SIZE: usize = 25;

#[cfg(test)]
const WINNING_SIZE: usize = 5;
#[cfg(test)]
const SCRATCH_SIZE: usize = 8;

#[derive(Default)]
pub struct Card {
    winning_numbers: [u8; WINNING_SIZE],
    scratch_numbers: [u8; SCRATCH_SIZE],
}

pub fn parse(inpt: &str) -> Vec<Card> {
    inpt.lines()
        .map(|line| {
            let mut card = Card::default();
            let line = &line[line.find(':').unwrap() + 1..];
            let (before, after) = line.split_once('|').unwrap();
            before
                .trim()
                .split_ascii_whitespace()
                .map(|x| x.trim().parse().unwrap())
                .collect_slice_checked(&mut card.winning_numbers);
            after
                .trim()
                .split_ascii_whitespace()
                .map(|x| x.trim().parse().unwrap())
                .collect_slice_checked(&mut card.scratch_numbers);
            card
        })
        .collect()
}

impl Card {
    fn num_matches(&self) -> usize {
        self.scratch_numbers
            .iter()
            .filter(|x| self.winning_numbers.contains(x))
            .count()
    }

    fn value(&self) -> usize {
        match self.num_matches() as u32 {
            0 => 0,
            x => 2usize.pow(x - 1),
        }
    }
}

pub fn part1(games: &[Card]) -> usize {
    games.iter().map(|game| game.value()).sum()
}

pub fn part2(games: &[Card]) -> usize {
    let mut counts = vec![1; games.len()];

    for (index, card) in games.iter().enumerate() {
        let value = card.num_matches();
        for i in 0..value {
            counts[index + i + 1] += counts[index];
        }
    }

    counts.iter().sum()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &str = "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
    Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
    Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
    Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
    Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
    Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 13);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 30);
    }
}
