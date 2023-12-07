use tap::Tap;

struct Rules {
    card_strengths_map: CardStrengthsMap,
    part2_rules_mode: bool,
}

impl Rules {
    const fn new(card_values: CardStrengthsMap, part2_rules_mode: bool) -> Self {
        Self {
            card_strengths_map: card_values,
            part2_rules_mode,
        }
    }

    fn get_card_strength(&self, label: u8) -> usize {
        self.card_strengths_map[(label - b'2') as usize].expect("the card type should be one of the expected ones")
    }
}

type Hands = Vec<(Hand, usize)>;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub struct Hand {
    kind: Categorisation,
    strength: u64,
}

impl Hand {
    fn parse(record: &str, rules: &Rules) -> Self {
        let card_strengths: Vec<_> = record.bytes().map(|c| rules.get_card_strength(c)).collect();

        Self {
            kind: Categorisation::parse(record, rules),
            strength: Self::sum_strengths(&card_strengths),
        }
    }

    fn sum_strengths(s: &[usize]) -> u64 {
        s.iter()
            .rev()
            .enumerate()
            .map(|(i, s)| (*s as u64) << (i * 8))
            .sum()
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Categorisation {
    HighCard,
    OnePair,
    TwoPairs,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}

impl Categorisation {
    fn parse(hand: &str, rules: &Rules) -> Self {
        let mut counts = [0; 13];
        let mut max = 0;
        let mut max_idx = 0;

        for c in hand.chars() {
            let strength = rules.get_card_strength(c as u8);
            counts[strength] += 1;

            if counts[strength] >= max && !(rules.part2_rules_mode && c == 'J') {
                max = counts[strength];
                max_idx = strength;
            }
        }

        if rules.part2_rules_mode {
            let joker_strength = rules.get_card_strength(b'J');
            let joker_count = counts[joker_strength];
            counts[joker_strength] = 0;
            counts[max_idx] += joker_count;
        }

        let mut num_pairs = 0;
        let mut triple_found = false;
        for c in counts {
            match c {
                5 => return Categorisation::FiveOfAKind,
                4 => return Categorisation::FourOfAKind,
                3 => triple_found = true,
                2 => num_pairs += 1,
                _ => (),
            }
        }

        match num_pairs {
            0 => {
                if triple_found {
                    Categorisation::ThreeOfAKind
                } else {
                    Categorisation::HighCard
                }
            }
            1 => {
                if triple_found {
                    Categorisation::FullHouse
                } else {
                    Categorisation::OnePair
                }
            }
            2 => Categorisation::TwoPairs,
            _ => unreachable!(),
        }
    }
}

// "23456789TJQKA".bytes().map(|x| x - b'2').max() + 1
type CardStrengthsMap = [Option<usize>; 35];

const fn generate_card_strengths_map(label_order: &[u8; 13]) -> CardStrengthsMap {
    // Using an array because it is small enough that it is faster than a hashmap
    let mut map: CardStrengthsMap = [None; 35];

    let mut i = 0;
    while i < 13 {
        map[(label_order[i] - b'2') as usize] = Some(i);
        i += 1;
    }

    map
}

pub fn parse(input: &str) -> (Hands, Hands) {
    let part1_interpretation = input
        .lines()
        .map(|line| {
            let (hand, bet) = line.split_once(' ').unwrap();
            (
                Hand::parse(
                    hand,
                    &Rules::new(
                        const { generate_card_strengths_map(b"23456789TJQKA") },
                        false,
                    ),
                ),
                bet.parse().unwrap(),
            )
        })
        .collect::<Vec<_>>()
        .tap_mut(|x| x.sort_unstable_by(|(a, _), (b, _)| a.cmp(b)));

    let part2_interpretation = input
        .lines()
        .map(|line| {
            let (hand, bet) = line.split_once(' ').unwrap();
            (
                Hand::parse(
                    hand,
                    &Rules::new(
                        const { generate_card_strengths_map(b"J23456789TQKA") },
                        true,
                    ),
                ),
                bet.parse().unwrap(),
            )
        })
        .collect::<Vec<_>>()
        .tap_mut(|x| x.sort_unstable_by(|(a, _), (b, _)| a.cmp(b)));

    (part1_interpretation, part2_interpretation)
}

fn solve(hands: &[(Hand, usize)]) -> usize {
    hands
        .iter()
        .enumerate()
        .map(|(n, (_, bet))| (n + 1) * bet)
        .sum()
}

pub fn part1(hands: &[(Hand, usize)]) -> usize {
    solve(hands)
}

pub fn part2(hands: &[(Hand, usize)]) -> usize {
    solve(hands)
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed.0));
    println!("Part 2: {}", part2(&parsed.1));
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &str = "32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT).0), 6440);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT).1), 5905);
    }
}
