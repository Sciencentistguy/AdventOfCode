use common::generate_combs_n;
use rayon::prelude::*;

type Parsed = Vec<Equation>;
type Solution = u64;

#[derive(Debug)]
pub struct Equation {
    test_value: u64,
    operands: Vec<u64>,
}

#[derive(Debug, Clone, Copy)]
enum Operator {
    Add,
    Multiply,
    Concatenate,
}

pub fn parse(input: &str) -> Parsed {
    input
        .lines()
        .map(|line| {
            let (test_value, operands) = line.split_once(':').unwrap();
            let test_value = test_value.parse().unwrap();
            let operands = operands
                .split_ascii_whitespace()
                .map(|operand| operand.trim().parse().unwrap())
                .collect();
            Equation {
                test_value,
                operands,
            }
        })
        .collect()
}

fn concat_u64(a: u64, b: u64) -> u64 {
    let mut multiplier = 1u64;
    let mut temp_b = b;

    while temp_b > 0 {
        multiplier *= 10;
        temp_b /= 10;
    }

    a * multiplier + b
}

fn evaluate(operands: &[u64], operators: &[Operator], target: u64) -> bool {
    let mut result = operands[0];
    for i in 0..operators.len() {
        match operators[i] {
            Operator::Add => result += operands[i + 1],
            Operator::Multiply => result *= operands[i + 1],
            Operator::Concatenate => result = concat_u64(result, operands[i + 1]),
        }
        if result > target {
            return false;
        }
    }
    result == target
}

fn can_make_value(equation: &Equation, operators: &[Operator]) -> bool {
    let operator_strings = generate_combs_n::<_, 16>(operators, equation.operands.len() - 1);

    for operators in operator_strings {
        if evaluate(&equation.operands, &operators, equation.test_value) {
            return true;
        }
    }
    false
}

pub fn part1(parsed: &Parsed) -> Solution {
    parsed
        .par_iter()
        .filter(|equation| can_make_value(equation, &[Operator::Add, Operator::Multiply]))
        .map(|x| x.test_value)
        .sum()
}

pub fn part2(parsed: &Parsed) -> Solution {
    parsed
        .par_iter()
        .filter(|equation| {
            can_make_value(equation, &[
                Operator::Add,
                Operator::Multiply,
                Operator::Concatenate,
            ])
        })
        .map(|x| x.test_value)
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

    const INPUT: &str = "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
";

    const P1_SOLUTION: Solution = 3749;
    const P2_SOLUTION: Solution = 11387;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
