use std::collections::VecDeque;

#[derive(Debug, Clone)]
pub struct Monkey {
    items: VecDeque<usize>,
    operation: Operation,
    test_divisible_by: usize,
    if_true: usize,
    if_false: usize,
}

#[derive(Debug, Clone)]
struct Operation {
    operator: Op,
    operand: Operand,
}

#[derive(Debug, Clone, Copy)]
enum Op {
    Add,
    Multiply,
}

#[derive(Debug, Clone, Copy)]
enum Operand {
    Number(usize),
    Old,
}

impl Operation {
    fn apply(&self, old: usize) -> usize {
        match self.operator {
            Op::Add => match self.operand {
                Operand::Number(n) => old + n,
                Operand::Old => old + old,
            },
            Op::Multiply => match self.operand {
                Operand::Number(n) => old * n,
                Operand::Old => old * old,
            },
        }
    }
}

pub fn parse(inpt: &str) -> Vec<Monkey> {
    inpt.split("\n\n")
        .map(|monkey| {
            let mut lines = monkey.lines();
            let _identifier = lines.next().unwrap();

            let starting_items = lines.next().unwrap();
            let starting_items = starting_items
                [memchr::memchr(b':', starting_items.as_bytes()).unwrap() + 2..]
                .split(", ")
                .map(|x| x.parse().unwrap())
                .collect();

            let operation = lines.next().unwrap();
            let operation = &operation[memchr::memchr(b'=', operation.as_bytes()).unwrap() + 6..];
            let operation = Operation {
                operator: match &operation[0..1] {
                    "+" => Op::Add,
                    "*" => Op::Multiply,
                    _ => panic!("Unknown operator"),
                },
                operand: match &operation[2..] {
                    "old" => Operand::Old,
                    x => Operand::Number(x.parse().unwrap()),
                },
            };

            let test_divisible_by = lines.next().unwrap();
            let test_divisible_by = test_divisible_by
                [memchr::memchr(b'y', test_divisible_by.as_bytes()).unwrap() + 2..]
                .parse()
                .unwrap();

            let if_true = lines.next().unwrap();
            let if_true = if_true[memchr::memchr(b'y', if_true.as_bytes()).unwrap() + 2..]
                .parse()
                .unwrap();

            let if_false = lines.next().unwrap();
            let if_false = if_false[memchr::memchr(b'y', if_false.as_bytes()).unwrap() + 2..]
                .parse()
                .unwrap();

            Monkey {
                items: starting_items,
                operation,
                test_divisible_by,
                if_true,
                if_false,
            }
        })
        .collect()
}

fn solve<F, const ROUNDS: usize>(monkeys: &[Monkey], allay: F) -> u64
where
    F: Fn(&mut usize),
{
    let mut monkeys = monkeys.to_owned();

    let mut hitcounts = vec![0; monkeys.len()];

    for _ in 0..ROUNDS {
        // Cannot use iter() here because we need to mutate other monkeys
        for i in 0..monkeys.len() {
            while let Some(mut worry) = monkeys[i].items.pop_front() {
                // look at item - increment hitcount for that monkey
                hitcounts[i] += 1;

                // modify worry level according to rule
                worry = monkeys[i].operation.apply(worry);

                // allay the worry
                allay(&mut worry);

                // throw to approriate monkey
                let target = if worry % monkeys[i].test_divisible_by == 0 {
                    monkeys[i].if_true
                } else {
                    monkeys[i].if_false
                };
                monkeys[target].items.push_back(worry);
            }
        }
    }

    hitcounts.sort_unstable();

    // Top 2 hitcounts
    hitcounts.pop().unwrap() * hitcounts.pop().unwrap()
}

pub fn part1(monkeys: &[Monkey]) -> u64 {
    solve::<_, 20>(monkeys, |x| *x /= 3)
}

pub fn part2(monkeys: &[Monkey]) -> u64 {
    let magic_number: usize = monkeys.iter().map(|x| x.test_divisible_by).product();

    solve::<_, 10_000>(monkeys, |x| *x %= magic_number)
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 10605);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 2713310158);
    }
}
