use std::collections::{HashSet, VecDeque};
use std::ops::Add;

use rayon::prelude::*;
use z3::{Solver, ast::Int};

type Parsed = Vec<Computer>;
type Solution = u64;

pub struct Computer {
    target_mask: u32,
    // target_lights: Vec<bool>,
    button_masks: Vec<u32>,
    buttons: Vec<Vec<usize>>,
    target_joltage: Vec<u64>,
}

pub fn parse(input: &str) -> Parsed {
    input
        .lines()
        .map(|line| {
            let (lights, line) = line.split_once(' ').unwrap();
            let (buttons, joltage) = line.rsplit_once(' ').unwrap();
            let buttons = buttons
                .split_whitespace()
                .map(|button| {
                    button[1..button.len() - 1]
                        .split(',')
                        .map(|s| s.parse::<usize>().unwrap())
                        .collect::<Vec<_>>()
                })
                .collect::<Vec<_>>();
            let button_masks = buttons
                .iter()
                .map(|btn| btn.iter().fold(0u32, |acc, idx| acc | (1 << idx)))
                .collect::<Vec<_>>();

            let joltage = joltage[1..joltage.len() - 1]
                .split(',')
                .map(|s| s.parse().unwrap())
                .collect();
            let target_mask = lights[1..lights.len() - 1]
                .chars()
                .map(|c| c == '#')
                .enumerate()
                .fold(
                    0u32,
                    |acc, (i, light)| {
                        if light { acc | (1 << i) } else { acc }
                    },
                );
            Computer {
                target_mask,
                button_masks,
                buttons,
                target_joltage: joltage,
            }
        })
        .collect()
}

fn solve_bfs(machine: &Computer) -> i64 {
    let mut queue = VecDeque::new();
    queue.push_back((0u32, 0));
    let mut visited = HashSet::new();
    visited.insert(0);

    while let Some((curr, steps)) = queue.pop_front() {
        if curr == machine.target_mask {
            return steps;
        }

        for &b_mask in &machine.button_masks {
            let nxt = curr ^ b_mask;
            if visited.insert(nxt) {
                queue.push_back((nxt, steps + 1));
            }
        }
    }
    0
}

fn solve_z3(computer: &Computer) -> i64 {
    let solver = z3::Optimize::new();
    let ints = computer
        .buttons
        .iter()
        .enumerate()
        .map(|(i, _)| Int::new_const(format!("b{}", i)))
        .collect::<Vec<_>>();
    let zero = Int::from_i64(0);
    for int in &ints {
        solver.assert(&int.ge(&zero));
    }

    // buttons is a vec of vecs of indices. each button increments the joltage of the indices it affects
    for (idx, &target) in computer.target_joltage.iter().enumerate() {
        let ast = ints
            .iter()
            .enumerate()
            .fold(Int::from_i64(0), |acc, (btn_idx, int)| {
                if computer.buttons[btn_idx].contains(&idx) {
                    acc.add(int)
                } else {
                    acc
                }
            });
        solver.assert(&ast.eq(Int::from_u64(target)));
    }

    // minimize sum(ints)
    let ast = ints.iter().fold(Int::from_i64(0), |acc, int| acc.add(int));
    solver.minimize(&ast);
    match solver.check(&[]) {
        z3::SatResult::Sat => {
            let model = solver.get_model().unwrap();
            let mut sum = 0;
            for int in &ints {
                let val = model.eval(int, true).unwrap().as_i64().unwrap();
                sum += val;
            }
            sum
        }
        _ => 0,
    }
}

pub fn part1(parsed: &Parsed) -> Solution {
    parsed
        .par_iter()
        .map(|machine| solve_bfs(machine) as u64)
        .sum()
}

pub fn part2(parsed: &Parsed) -> Solution {
    parsed
        .par_iter()
        .map(|machine| solve_z3(machine) as u64)
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

    const INPUT: &str = "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
";

    const P1_SOLUTION: Solution = 7;
    const P2_SOLUTION: Solution = 33;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
