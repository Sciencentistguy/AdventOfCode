#![allow(non_snake_case)] // matrices have capital letter names fight me

use std::ops::Mul;

use common::ArraySplit;

use num::{Rational64, Zero};
use rayon::prelude::*;

pub struct Matrix {
    data: [[Rational64; 2]; 2],
}

impl Matrix {
    fn new(a: Rational64, b: Rational64, c: Rational64, d: Rational64) -> Self {
        Self {
            data: [[a, b], [c, d]],
        }
    }

    fn inv(&self) -> Option<Self> {
        let [[a, b], [c, d]] = self.data;

        let det = a * d - b * c;

        if det.is_zero() {
            return None;
        }

        let inv_a = d / det;
        let inv_b = -b / det;
        let inv_c = -c / det;
        let inv_d = a / det;

        Some(Self::new(inv_a, inv_b, inv_c, inv_d))
    }
}

impl Mul<Vector> for Matrix {
    type Output = Vector;

    fn mul(self, rhs: Vector) -> Self::Output {
        let [[a, b], [c, d]] = self.data;
        let [e, f] = rhs;

        [a * e + b * f, c * e + d * f]
    }
}

type Vector = [Rational64; 2];

type Parsed = Vec<(Matrix, Vector)>;
type Solution = i64;

fn parse_button(line: &str) -> (i64, i64) {
    let line = &line[const { "Button B: ".len() }..];
    let (x, y) = line.split_once(", ").unwrap();
    let x = x[const { "X+".len() }..].parse().unwrap();
    let y = y[const { "Y+".len() }..].parse().unwrap();
    (x, y)
}

fn parse_prize(line: &str) -> (i64, i64) {
    let line = &line[const { "Prize: ".len() }..];
    let (x, y) = line.split_once(", ").unwrap();
    let x = x[const { "X=".len() }..].parse().unwrap();
    let y = y[const { "Y=".len() }..].parse().unwrap();
    (x, y)
}

pub fn parse(input: &str) -> Parsed {
    input
        .split("\n\n")
        .map(|block| {
            let [line1, line2, line3] = block.array_lines();
            let (ax, ay) = parse_button(line1);
            let (bx, by) = parse_button(line2);
            let prize = parse_prize(line3);

            let M = Matrix::new(
                Rational64::new(ax, 1),
                Rational64::new(bx, 1),
                Rational64::new(ay, 1),
                Rational64::new(by, 1),
            );
            let prize = [
                Rational64::new(prize.0 as i64, 1),
                Rational64::new(prize.1 as i64, 1),
            ];

            (M, prize)
        })
        .collect()
}

pub fn part1(games: &Parsed) -> Solution {
    games
        .par_iter()
        .filter_map(|(M, prize)| Some(M.inv()? * *prize))
        .filter(|[a, b]| a.is_integer() && b.is_integer())
        .map(|[a, b]| (3 * a.to_integer()) + b.to_integer())
        .sum()
}

pub fn part2(games: &Parsed) -> Solution {
    games
        .par_iter()
        .map(|(M, prize)| {
            const PRIZE_OFFSET: i64 = 10000000000000;
            (M, prize.map(|x| x + PRIZE_OFFSET))
        })
        .filter_map(|(M, prize)| Some(M.inv()? * prize))
        .filter(|[a, b]| a.is_integer() && b.is_integer())
        .map(|[a, b]| (3 * a.to_integer()) + b.to_integer())
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

    const INPUT: &str = "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
";

    const P1_SOLUTION: Solution = 480;
    // const P2_SOLUTION: Solution = todo!();

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    // #[test]
    // fn test_part2() {
    //     assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    // }
}
