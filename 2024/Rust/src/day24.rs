use common::ArraySplit;
use fxhash::FxHashMap as HashMap;
use itertools::Itertools;

type Parsed<'a> = HashMap<&'a str, Wire<'a>>;
type Solution = u64;

#[derive(Debug)]
pub enum Production<'a> {
    And(&'a str, &'a str),
    Or(&'a str, &'a str),
    Xor(&'a str, &'a str),
}

#[derive(Debug)]
pub enum Wire<'a> {
    Production(Production<'a>),
    Value(bool),
}

pub fn parse(input: &str) -> Parsed {
    let (init, productions) = input.split_once("\n\n").unwrap();
    let mut wires: Parsed = HashMap::default();
    for line in init.lines() {
        let (name, value) = line.split_once(": ").unwrap();
        let value = match value {
            "0" => Wire::Value(false),
            "1" => Wire::Value(true),
            _ => unreachable!(),
        };
        wires.insert(name, value);
    }

    for line in productions.lines() {
        let [lhs, operator, rhs, _, target] = line.array_split(' ');
        let production = match operator {
            "AND" => Production::And(lhs, rhs),
            "OR" => Production::Or(lhs, rhs),
            "XOR" => Production::Xor(lhs, rhs),
            _ => unreachable!("{line}"),
        };
        wires.insert(target, Wire::Production(production));
    }

    wires
}

pub fn part1(parsed: &Parsed) -> Solution {
    let mut memo: HashMap<&str, bool> = HashMap::default();
    parsed
        .keys()
        .filter(|x| x.starts_with("z"))
        .sorted()
        .rev()
        .map(|target| get(parsed, &mut memo, target))
        .fold(0u64, |acc, bit| (acc << 1) | (bit as u64))
}

fn get<'a>(parsed: &Parsed<'a>, memo: &mut HashMap<&'a str, bool>, wire: &'a str) -> bool {
    if let Some(&value) = memo.get(wire) {
        return value;
    }

    let value = match parsed.get(wire).unwrap() {
        Wire::Value(value) => *value,
        Wire::Production(production) => match production {
            Production::And(lhs, rhs) => get(parsed, memo, lhs) & get(parsed, memo, rhs),
            Production::Or(lhs, rhs) => get(parsed, memo, lhs) | get(parsed, memo, rhs),
            Production::Xor(lhs, rhs) => get(parsed, memo, lhs) ^ get(parsed, memo, rhs),
        },
    };

    memo.insert(wire, value);
    value
}

pub fn part2(parsed: &Parsed) -> Solution {
    todo!()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "x00: 1
x01: 0
x02: 1
x03: 1
x04: 0
y00: 1
y01: 1
y02: 1
y03: 1
y04: 1

ntg XOR fgs -> mjb
y02 OR x01 -> tnw
kwq OR kpj -> z05
x00 OR x03 -> fst
tgd XOR rvg -> z01
vdt OR tnw -> bfw
bfw AND frj -> z10
ffh OR nrd -> bqk
y00 AND y03 -> djm
y03 OR y00 -> psh
bqk OR frj -> z08
tnw OR fst -> frj
gnj AND tgd -> z11
bfw XOR mjb -> z00
x03 OR x00 -> vdt
gnj AND wpb -> z02
x04 AND y00 -> kjc
djm OR pbm -> qhw
nrd AND vdt -> hwm
kjc AND fst -> rvg
y04 OR y02 -> fgs
y01 AND x02 -> pbm
ntg OR kjc -> kwq
psh XOR fgs -> tgd
qhw XOR tgd -> z09
pbm OR djm -> kpj
x03 XOR y03 -> ffh
x00 XOR y04 -> ntg
bfw OR bqk -> z06
nrd XOR fgs -> wpb
frj XOR qhw -> z04
bqk OR frj -> z07
y03 OR x01 -> nrd
hwm AND bqk -> z03
tgd XOR rvg -> z12
tnw OR pbm -> gnj
";

    const P1_SOLUTION: Solution = 2024;
    // const P2_SOLUTION: Solution = todo!();

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        // assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
