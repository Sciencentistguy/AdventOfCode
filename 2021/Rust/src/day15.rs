use std::{
    cmp::Ordering,
    collections::{BinaryHeap, HashMap},
    time::Instant,
};

use eyre::Result;

type Input = Vec<Vec<usize>>;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
struct Point {
    x: usize,
    y: usize,
}

impl Point {
    const ZERO: Self = Self { x: 0, y: 0 };
}

#[derive(Clone, Eq, PartialEq, Debug)]
struct State {
    pos: Point,
    cost: usize,
}

impl Ord for State {
    fn cmp(&self, other: &Self) -> Ordering {
        other
            .cost
            .cmp(&self.cost)
            .then_with(|| self.pos.cmp(&other.pos))
    }
}

impl PartialOrd for State {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn djikstra(map: &Input, start: Point, target: Point) -> usize {
    let y_max = map.len();
    let x_max = map[0].len();

    let mut pqueue = BinaryHeap::with_capacity(map.len() * map[0].len());
    let mut distances = HashMap::with_capacity(map.len() * map[0].len());

    distances.insert(start.clone(), 0);

    pqueue.push(State {
        pos: start,
        cost: 0,
    });

    while let Some(State {
        pos: pos @ Point { x, y },
        cost,
    }) = pqueue.pop()
    {
        if pos == target {
            return cost;
        }

        if let Some(&best_cost) = distances.get(&pos) {
            if cost > best_cost {
                continue;
            }
        }

        for (new_x, new_y) in [
            (x.wrapping_sub(1), y),
            (x + 1, y),
            (x, y.wrapping_sub(1)),
            (x, y + 1),
        ] {
            if new_x >= x_max || new_y >= y_max {
                continue;
            }
            let next = State {
                pos: Point { x: new_x, y: new_y },
                cost: cost + map[new_y][new_x],
            };

            if &next.cost
                < distances
                    .get(&Point { x: new_x, y: new_y })
                    .unwrap_or(&usize::MAX)
            {
                pqueue.push(next.clone());
                distances.insert(next.pos, next.cost);
            }
        }
    }
    unreachable!()
}

fn parse(input: &str) -> Input {
    input
        .lines()
        .map(|line| {
            line.chars()
                .map(|x| x.to_digit(10).expect("invalid digit") as usize)
                .collect()
        })
        .collect()
}

fn part1(input: &Input) -> u64 {
    djikstra(
        input,
        Point::ZERO,
        Point {
            x: input[0].len() - 1,
            y: input.len() - 1,
        },
    ) as _
}

fn part2(input: &Input) -> u64 {
    let y_max = input.len();
    let x_max = input[0].len();
    let mut map = vec![vec![0; x_max * 5]; y_max * 5];

    for y in 0..y_max * 5 {
        for x in 0..x_max * 5 {
            map[y][x] = match input[y % y_max][x % x_max] + (y / y_max) + (x / x_max) {
                val @ 0..=9 => val,
                val => (val % 10) + 1,
            };
        }
    }

    djikstra(
        &map,
        Point::ZERO,
        Point {
            x: x_max * 5 - 1,
            y: y_max * 5 - 1,
        },
    ) as _
}

pub fn run(input: String) -> Result<()> {
    let parsed = parse(input.as_str());
    let start = Instant::now();
    let part1 = part1(&parsed);
    let p1_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 15 (part 1) is {}. Took {:?}",
        part1, p1_time
    );
    let start = Instant::now();
    let part2 = part2(&parsed);
    let p2_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 15 (part 2) is {}. Took {:?}",
        part2, p2_time
    );

    Ok(())
}
