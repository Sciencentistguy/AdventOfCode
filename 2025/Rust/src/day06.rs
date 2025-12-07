use memchr::memchr;
use rayon::prelude::*;

type Solution = u64;
pub struct Parsed {
    line_len: usize,
    operands: Vec<u8>,
    operators: Vec<OperatorCol>,
}

#[derive(Debug, Clone, Copy)]
struct OperatorCol {
    // inner: &'a [u8],
    operator: Operator,
    start: usize,
    len: usize,
}
#[derive(Debug, Clone, Copy)]
enum Operator {
    Add,
    Mul,
}

impl Operator {
    const fn identity(&self) -> u64 {
        match self {
            Operator::Add => 0,
            Operator::Mul => 1,
        }
    }
}

pub fn parse(input: &str) -> Parsed {
    let input = input.as_bytes();
    let first_newline = memchr(b'\n', input).unwrap();
    let line_len = first_newline + 1;
    let operator_line_start = input.len() - line_len;

    // compiler pls vectorise
    let ret = input[0..operator_line_start]
        .iter()
        .map(|b| b.wrapping_sub(b'0'))
        .collect::<Vec<_>>();

    let operator_line = &input[operator_line_start..];

    let mut ret2 = Vec::new();
    let mut column_start = 0;
    let mut column_len = 1;

    while column_start + column_len < operator_line.len() {
        let operator = match operator_line[column_start] {
            b'+' => Operator::Add,
            b'*' => Operator::Mul,
            x => panic!("wrong operator: '{}' ({x})", x as char),
        };

        column_len = operator_line[column_start + column_len..]
            .iter()
            .take_while(|x| **x == b' ')
            .count()
            + 1;

        if operator_line[column_start + column_len] == b'\n' {
            column_len += 1;
        }

        ret2.push(OperatorCol {
            operator,
            start: column_start,
            len: column_len,
        });

        column_start += column_len;
        column_len = 1;
    }

    Parsed {
        line_len,
        operands: ret,
        operators: ret2,
    }
}

pub fn part1(
    Parsed {
        line_len,
        operands,
        operators,
    }: &Parsed,
) -> Solution {
    operators
        .iter()
        .map(
            |&OperatorCol {
                 operator,
                 start: column_start,
                 len: column_len,
             }| {
                let mut acc = operator.identity();

                for line in 0..(operands.len() / line_len) {
                    let mut operand = 0;
                    let mut seen = false;
                    let start = line * line_len + column_start;
                    let end = start + column_len;

                    for &digit in &operands[start..end] {
                        if digit < 10 {
                            operand = operand * 10 + digit as u64;
                            seen = true;
                        } else if seen {
                            break;
                        }
                    }

                    match operator {
                        Operator::Add => acc += operand,
                        Operator::Mul => acc *= operand,
                    }
                }
                acc
            },
        )
        .sum()
}

pub fn part2(
    Parsed {
        line_len,
        operands,
        operators,
    }: &Parsed,
) -> Solution {
    operators
        .iter()
        .map(
            |&OperatorCol {
                 operator,
                 start,
                 len,
             }| {
                let mut acc = operator.identity();

                for column in (0..len - 1).rev() {
                    let mut operand = 0;
                    let mut seen = false;

                    for line in 0..(operands.len() / line_len) {
                        let i = line * line_len + start + column;

                        let digit = operands[i];

                        if digit < 10 {
                            operand = operand * 10 + digit as u64;
                            seen = true;
                        } else if seen {
                            break;
                        }
                    }

                    match operator {
                        Operator::Add => acc += operand,
                        Operator::Mul => acc *= operand,
                    }
                }

                acc
            },
        )
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

    const INPUT: &str = "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  
";

    const P1_SOLUTION: Solution = 4277556;
    const P2_SOLUTION: Solution = 3263827;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
