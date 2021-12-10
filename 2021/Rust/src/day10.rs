use std::time::Instant;

use eyre::Result;

fn parse(input: &str) -> Vec<Vec<Bracket>> {
    input
        .lines()
        .map(|x| x.bytes().map(|x| x.into()).collect())
        .collect()
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum BracketKind {
    Round,
    Curly,
    Square,
    Angle,
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum BracketDirection {
    Open,
    Close,
}

struct Bracket {
    kind: BracketKind,
    direction: BracketDirection,
}

impl BracketKind {
    const fn error_correction_score(&self) -> u64 {
        match self {
            BracketKind::Round => 3,
            BracketKind::Square => 57,
            BracketKind::Curly => 1197,
            BracketKind::Angle => 25137,
        }
    }
    const fn autocomplete_score(&self) -> u64 {
        match self {
            BracketKind::Round => 1,
            BracketKind::Square => 2,
            BracketKind::Curly => 3,
            BracketKind::Angle => 4,
        }
    }
}

impl From<u8> for Bracket {
    fn from(c: u8) -> Self {
        match c {
            b'(' => Self {
                kind: BracketKind::Round,
                direction: BracketDirection::Open,
            },
            b'[' => Self {
                kind: BracketKind::Square,
                direction: BracketDirection::Open,
            },
            b'{' => Self {
                kind: BracketKind::Curly,
                direction: BracketDirection::Open,
            },
            b'<' => Self {
                kind: BracketKind::Angle,
                direction: BracketDirection::Open,
            },
            b')' => Self {
                kind: BracketKind::Round,
                direction: BracketDirection::Close,
            },
            b']' => Self {
                kind: BracketKind::Square,
                direction: BracketDirection::Close,
            },
            b'}' => Self {
                kind: BracketKind::Curly,
                direction: BracketDirection::Close,
            },
            b'>' => Self {
                kind: BracketKind::Angle,
                direction: BracketDirection::Close,
            },
            _ => unreachable!("Invalid bracket character: {}", char::from(c)),
        }
    }
}

fn get_line_score(line: &[Bracket], stack: &mut Vec<BracketKind>) -> u64 {
    let mut score = 0;
    for &Bracket { kind, direction } in line {
        match direction {
            BracketDirection::Open => stack.push(kind),
            BracketDirection::Close => {
                let expected_kind = match stack.pop() {
                    Some(x) => x,
                    None => {
                        unreachable!("?")
                    }
                };
                if expected_kind != kind {
                    score += kind.error_correction_score();
                }
            }
        }
    }
    score
}

fn part1(input: &[Vec<Bracket>]) -> u64 {
    let mut score = 0;
    let mut stack = Vec::new();
    for line in input {
        stack.clear();
        score += get_line_score(line, &mut stack);
    }
    score
}

fn part2(input: &[Vec<Bracket>]) -> u64 {
    let mut scores = Vec::with_capacity(input.len());
    let mut stack = Vec::new();

    for line in input {
        let mut score = 0;
        stack.clear();
        if get_line_score(line, &mut stack) != 0 {
            continue;
        }
        for kind in stack.iter().rev() {
            score *= 5;
            score += kind.autocomplete_score();
        }
        scores.push(score);
    }

    scores.sort_unstable();
    scores[(scores.len() - 1) / 2]
}

pub fn run(input: String) -> Result<()> {
    let parsed = parse(input.as_str());
    let start = Instant::now();
    let part1 = part1(&parsed);
    let p1_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 10 (part 1) is {}. Took {:?}",
        part1, p1_time
    );
    let start = Instant::now();
    let part2 = part2(&parsed);
    let p2_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 10 (part 2) is {}. Took {:?}",
        part2, p2_time
    );

    Ok(())
}
