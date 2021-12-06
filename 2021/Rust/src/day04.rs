use std::time::Instant;

use eyre::Result;
use ndarray::Array2;

#[derive(Debug, Clone)]
struct Board(Array2<Entry>);

impl Board {
    fn is_finished(&self) -> bool {
        for row in self.0.rows() {
            if row.iter().all(|Entry { enabled, number: _ }| *enabled) {
                return true;
            }
        }
        for col in self.0.columns() {
            if col.iter().all(|Entry { enabled, number: _ }| *enabled) {
                return true;
            }
        }
        false
    }
}

#[derive(Debug, Clone)]
struct Entry {
    enabled: bool,
    number: u64,
}

type Parsed = (Vec<u64>, Vec<Board>);

fn parse(input: &str) -> Result<Parsed> {
    let numbers_line = input.lines().next().unwrap();
    let numbers = numbers_line
        .split(',')
        .map(|x| x.parse())
        .collect::<Result<Vec<u64>, _>>()?;

    let boards = input
        .lines()
        .skip(1)
        .collect::<Vec<_>>()
        .split(|x| x.is_empty())
        .filter(|x| !x.is_empty())
        .map(|x| x.to_vec())
        .map(|board| {
            board
                .into_iter()
                .flat_map(|x| x.split_whitespace())
                .map(|x| {
                    x.parse().map(|x| Entry {
                        enabled: false,
                        number: x,
                    })
                })
                .collect::<Result<Vec<Entry>, _>>()
                .map(|x| Board(Array2::from_shape_vec((5, 5), x).unwrap()))
        })
        .collect::<Result<Vec<Board>, _>>()?;
    Ok((numbers, boards))
}

fn apply_number(number: u64, board: &mut Board) {
    for Entry { enabled, number: i } in &mut board.0 {
        if *i == number {
            *enabled = true;
        }
    }
}

fn part1((numbers, boards): &Parsed) -> Result<u64> {
    let mut boards = boards.clone();

    for &number in numbers {
        for board in &mut boards {
            apply_number(number, board);
            if board.is_finished() {
                return Ok(board
                    .0
                    .iter()
                    .filter_map(
                        |&Entry { enabled, number }| if enabled { None } else { Some(number) },
                    )
                    .sum::<u64>()
                    * number);
            }
        }
    }
    unreachable!("At least one board should win")
}

fn part2((numbers, boards): &Parsed) -> u64 {
    let mut boards = boards.clone();
    for &number in numbers {
        for b in &mut boards {
            apply_number(number, b);
        }
        if boards.len() == 1 && boards[0].is_finished() {
            return boards[0]
                .0
                .iter()
                .filter_map(|&Entry { enabled, number }| if enabled { None } else { Some(number) })
                .sum::<u64>()
                * number;
        }
        boards.retain(|x| !x.is_finished());
    }
    unreachable!("All boards should win")
}

pub fn run(input: String) -> Result<()> {
    let parsed = parse(input.as_str())?;
    let start = Instant::now();
    let part1 = part1(&parsed)?;
    let p1_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 03 (part 1) is {}. Took {:?}",
        part1, p1_time
    );
    let start = Instant::now();
    let part2 = part2(&parsed);
    let p2_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 03 (part 2) is {}. Took {:?}",
        part2, p2_time
    );

    Ok(())
}
