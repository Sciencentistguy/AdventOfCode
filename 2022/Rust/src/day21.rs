use lazy_format::lazy_format;
use rustc_hash::FxHashMap as HashMap;
use std::{
    fmt::Display,
    ops::{Deref, Range, RangeFrom},
};

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

    pub fn as_name(&self) -> Option<&'a str> {
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
}

impl<'a> Table<'a> {
    fn evaluate(&self, name: &str) -> usize {
        self.0[name].evaluate(self)
    }

    fn equation(&'a self, name: &'a str) -> impl Display + 'a {
        use Operation::*;

        lazy_format!(match (self.0[name]) {
            _ if name == "humn" => ("x"),
            Literal(n) => ("{}", n.as_number().unwrap()),
            Add(lhs, rhs) => (
                "({}+{})",
                self.equation(lhs.as_name().unwrap()),
                self.equation(rhs.as_name().unwrap())
            ),
            Sub(lhs, rhs) => (
                "({}-{})",
                self.equation(lhs.as_name().unwrap()),
                self.equation(rhs.as_name().unwrap())
            ),
            Mul(lhs, rhs) => (
                "({}*{})",
                self.equation(lhs.as_name().unwrap()),
                self.equation(rhs.as_name().unwrap())
            ),
            Div(lhs, rhs) => (
                "({}/{})",
                self.equation(lhs.as_name().unwrap()),
                self.equation(rhs.as_name().unwrap())
            ),
        })
    }

    fn operands(&self, name: &str) -> (Identifier, Identifier) {
        use Operation::*;
        match self[name] {
            Add(lhs, rhs) | Sub(lhs, rhs) | Mul(lhs, rhs) | Div(lhs, rhs) => (lhs, rhs),
            _ => unreachable!(),
        }
    }
}

pub fn parse(inpt: &str) -> Table {
    Table(
        inpt.lines()
            .map(|line| {
                use Identifier::*;
                use Operation::*;

                // Thanks for making these fixed width, AoC!
                const NAME: Range<usize> = 0..4;
                const NUMBER: RangeFrom<usize> = 6..;
                const FIRST_ARGUMENT: Range<usize> = 6..10;
                const SECOND_ARGUMENT: Range<usize> = 13..17;
                const OPERATOR: usize = 11;

                let name = line[NAME].trim();
                let op = if line.as_bytes()[NUMBER.start].is_ascii_digit() {
                    let number = line[NUMBER].parse().unwrap();
                    Literal(Number(number))
                } else {
                    let first = &line[FIRST_ARGUMENT];
                    let second = &line[SECOND_ARGUMENT];
                    match line.as_bytes()[OPERATOR] {
                        b'+' => Add(Name(first), Name(second)),
                        b'-' => Sub(Name(first), Name(second)),
                        b'*' => Mul(Name(first), Name(second)),
                        b'/' => Div(Name(first), Name(second)),
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
        "{}-{}",
        table.equation(a.as_name().unwrap()),
        b.resolve(table)
    )
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!(
        "Part 2: Paste the following into sympy:\n{}",
        part2(&parsed)
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 152);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), "((4+(2*(x-3)))/4)-150"); //x==301
    }
}
