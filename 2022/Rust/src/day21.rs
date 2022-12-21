use std::{collections::HashMap, ops::Deref};

use itertools::Itertools;

pub struct Table<'a>(HashMap<&'a str, Operation<'a>>);

impl<'a> Deref for Table<'a> {
    type Target = HashMap<&'a str, Operation<'a>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'a> ToOwned for Table<'a> {
    type Owned = Self;

    fn to_owned(&self) -> Self::Owned {
        Self(self.0.clone())
    }
}

#[derive(Clone, Copy)]
pub enum Identifier<'a> {
    Name(&'a str),
    Number(usize),
}

impl<'a> Identifier<'a> {
    fn resolve(&self, table: &Table<'a>) -> usize {
        match self {
            Identifier::Name(name) => table[name].evaluate(table),
            Identifier::Number(number) => *number,
        }
    }

    pub fn as_name(&self) -> Option<&&'a str> {
        if let Self::Name(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn is_humn(&self) -> bool {
        matches!(self, Self::Name("humn"))
    }

    pub fn as_number(&self) -> Option<&usize> {
        if let Self::Number(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

#[derive(Clone)]
pub enum Operation<'a> {
    Literal(Identifier<'a>),
    Add(Identifier<'a>, Identifier<'a>),
    Sub(Identifier<'a>, Identifier<'a>),
    Mul(Identifier<'a>, Identifier<'a>),
    Div(Identifier<'a>, Identifier<'a>),
}

impl<'a> Operation<'a> {
    fn evaluate(&self, table: &Table<'a>) -> usize {
        match self {
            Self::Literal(n) => n.resolve(table),
            Self::Add(a, b) => a.resolve(table) + b.resolve(table),
            Self::Sub(a, b) => a.resolve(table) - b.resolve(table),
            Self::Mul(a, b) => a.resolve(table) * b.resolve(table),
            Self::Div(a, b) => a.resolve(table) / b.resolve(table),
        }
    }

    fn operands(&self, table: &Table) -> (usize, usize) {
        match self {
            Self::Literal(n) => unreachable!(),
            Self::Add(a, b) | Self::Sub(a, b) | Self::Mul(a, b) | Self::Div(a, b) => {
                (a.resolve(table), b.resolve(table))
            }
        }
    }

    fn operand_names(&self, table: &Table) -> (&'a str, &'a str) {
        match self {
            Self::Literal(n) => unreachable!(),
            Self::Add(a, b) | Self::Sub(a, b) | Self::Mul(a, b) | Self::Div(a, b) => {
                (a.as_name().unwrap(), b.as_name().unwrap())
            }
        }
    }
}

impl<'a> Table<'a> {
    fn evaluate(&self, name: &str) -> usize {
        self.0[name].evaluate(self)
    }

    fn get_eq(&self, name: &str) -> String {
        if name == "humn" {
            return "x".to_string();
        }
        match &self[name] {
            Operation::Literal(i) => i.as_number().unwrap().to_string(),
            Operation::Add(m1, m2) => format!(
                "({} + {})",
                self.get_eq(m1.as_name().unwrap()),
                self.get_eq(m2.as_name().unwrap())
            ),
            Operation::Sub(m1, m2) => format!(
                "({} - {})",
                self.get_eq(m1.as_name().unwrap()),
                self.get_eq(m2.as_name().unwrap())
            ),
            Operation::Mul(m1, m2) => format!(
                "({} * {})",
                self.get_eq(m1.as_name().unwrap()),
                self.get_eq(m2.as_name().unwrap())
            ),
            Operation::Div(m1, m2) => format!(
                "({} / {})",
                self.get_eq(m1.as_name().unwrap()),
                self.get_eq(m2.as_name().unwrap())
            ),
        }
    }

    fn operands(&self, name: &str) -> (Identifier, Identifier) {
        match self[name] {
            Operation::Add(m1, m2) => (m1, m2),
            Operation::Sub(m1, m2) => (m1, m2),
            Operation::Mul(m1, m2) => (m1, m2),
            Operation::Div(m1, m2) => (m1, m2),
            _ => unreachable!(),
        }
    }
}

pub fn parse(inpt: &str) -> Table {
    Table(
        inpt.lines()
            .map(|line| {
                let colon = 4;
                let name = line[..colon].trim();
                let char = line.as_bytes()[colon + 2];
                let op = if char.is_ascii_digit() {
                    let number = line[colon + 2..].parse().unwrap();
                    Operation::Literal(Identifier::Number(number))
                } else {
                    let op = line.as_bytes()[11];
                    let first = &line[6..6 + 4];
                    let second = &line[13..];
                    match op {
                        b'+' => Operation::Add(Identifier::Name(first), Identifier::Name(second)),
                        b'-' => Operation::Sub(Identifier::Name(first), Identifier::Name(second)),
                        b'*' => Operation::Mul(Identifier::Name(first), Identifier::Name(second)),
                        b'/' => Operation::Div(Identifier::Name(first), Identifier::Name(second)),
                        _ => unreachable!(),
                    }
                };

                (name, op)
            })
            .collect(),
    )
}

pub fn part1(table: &Table) -> usize {
    table.evaluate("root")
}

pub fn part2(table: &Table) -> String {
    let (a, b) = table.operands("root");

    format!(
        "{} - {}",
        table.get_eq(a.as_name().unwrap()),
        b.resolve(table)
    )
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: Paste the following into sympy:\n{}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 24000);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 45000);
    }
}
