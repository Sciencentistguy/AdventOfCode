use std::{
    sync::atomic::{AtomicIsize, Ordering},
    time::{Duration, Instant},
};

static COL: AtomicIsize = AtomicIsize::new(0);
static ROW: AtomicIsize = AtomicIsize::new(0);

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
enum Seat {
    Floor,
    Empty,
    Occupied,
}

fn parse_input(input: &str) -> (Vec<usize>, Duration) {
    let start = Instant::now();
    COL.store(input.find('\n').unwrap() as isize, Ordering::Relaxed);
    ROW.store(input.lines().count() as isize, Ordering::Relaxed);
    let seat_indices = input
        .as_bytes()
        .iter()
        .enumerate()
        .filter(|(_, s)| s == &&b'L')
        .map(|(i, _)| i - (i / (COL.load(Ordering::Relaxed) as usize + 1)))
        .collect();
    let end = Instant::now();
    (seat_indices, end - start)
}

fn solve_part1(seat_indices: &[usize]) -> (usize, Duration) {
    let start = Instant::now();

    let mut cur =
        vec![Seat::Floor; (ROW.load(Ordering::Relaxed) * COL.load(Ordering::Relaxed)) as usize];
    for i in seat_indices {
        cur[*i] = Seat::Empty;
    }
    let mut prev = cur.clone();

    #[rustfmt::skip]
    let seats: Vec<(usize, Vec<usize>)> = seat_indices
        .iter()
        .map(|&i| (i, (0..9)
            .filter(|&r| r != 4)
            .map(|r| ((i as isize % COL.load(Ordering::Relaxed)) + r % 3 - 1, (i as isize / COL.load(Ordering::Relaxed)) + r / 3 - 1))
            .filter(|(x, y)| *x >= 0 && *y >= 0 && *x < COL.load(Ordering::Relaxed) && *y < ROW.load(Ordering::Relaxed))
            .map(|(x, y)| (y * COL.load(Ordering::Relaxed) + x) as usize)
            .filter(|i| cur[*i] == Seat::Empty)
            .collect(),
        ))
        .collect();

    #[allow(clippy::blocks_in_if_conditions)]
    while {
        for (i, visible) in &seats {
            let occup = visible
                .iter()
                .filter(|i| prev[**i] == Seat::Occupied)
                .count();
            let cur_seat = &mut cur[*i];
            match prev[*i] {
                Seat::Occupied if occup >= 4 => *cur_seat = Seat::Empty,
                Seat::Empty if occup == 0 => {
                    *cur_seat = Seat::Occupied;
                }
                prev_seat => *cur_seat = prev_seat,
            };
        }

        std::mem::swap(&mut cur, &mut prev);
        cur != prev
    } {}

    let count = cur.iter().filter(|s| **s == Seat::Occupied).count();
    let end = Instant::now();
    (count, end - start)
}

fn solve_part2(seat_indices: &[usize]) -> (usize, Duration) {
    let start = Instant::now();

    let mut cur =
        vec![Seat::Floor; (ROW.load(Ordering::Relaxed) * COL.load(Ordering::Relaxed)) as usize];
    for i in seat_indices {
        cur[*i] = Seat::Empty;
    }
    let mut prev = cur.clone();

    #[rustfmt::skip]
    let seats: Vec<(usize, Vec<usize>)> = seat_indices
        .iter()
        .map(|&i| (i, (0..9)
            .filter(|r| r != &4)
            .map(|r| (r % 3 - 1, r / 3 - 1))
            .filter_map(|(rx, ry)| (1..)
                .map(|f| ((i as isize % COL.load(Ordering::Relaxed)) + rx * f, (i as isize / COL.load(Ordering::Relaxed)) + ry * f))
                .take_while(|(x, y)| *x >= 0 && *y >= 0 && *x < COL.load(Ordering::Relaxed) && *y < ROW.load(Ordering::Relaxed))
                .map(|(x, y)| (y * COL.load(Ordering::Relaxed) + x) as usize)
                .find(|i| cur[*i] == Seat::Empty)
            )
            .collect(),
        ))
        .collect();

    #[allow(clippy::blocks_in_if_conditions)]
    while {
        for (i, visible) in &seats {
            let occup = visible
                .iter()
                .filter(|i| prev[**i] == Seat::Occupied)
                .count();
            let (cur_seat, prev_seat) = (&mut cur[*i], prev[*i]);

            match prev_seat {
                Seat::Empty if occup == 0 => {
                    *cur_seat = Seat::Occupied;
                }
                Seat::Occupied if occup >= 5 => *cur_seat = Seat::Empty,
                _ => *cur_seat = prev_seat,
            };
        }

        std::mem::swap(&mut cur, &mut prev);
        cur != prev
    } {}

    let count = cur.iter().filter(|s| **s == Seat::Occupied).count();
    let end = Instant::now();
    (count, end - start)
}

pub fn run(input: String) {
    let (seat_indices, time) = parse_input(&input);
    println!("Day 10, parsing input took {}ns", time.as_nanos());
    let (p1, time) = solve_part1(&seat_indices);
    println!("Day 11, part 1: {}. Took {}ns", p1, time.as_nanos());
    let (p2, time) = solve_part2(&seat_indices);
    println!("Day 11, part 2: {}. Took {}ns", p2, time.as_nanos());
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL";

    #[test]
    fn day11_part1() {
        let seat_indices = parse_input(INPUT).0;
        assert_eq!(solve_part1(&seat_indices).0, 37);
    }

    #[test]
    fn day11_part2() {
        let seat_indices = parse_input(INPUT).0;
        assert_eq!(solve_part2(&seat_indices).0, 37);
    }
}
