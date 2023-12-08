use std::ops::Index;

use fxhash::FxHashMap;
use num::integer::lcm;
use rayon::prelude::*;

type Network<'a> = FxHashMap<&'a str, (&'a str, &'a str)>;

struct Rules(Vec<Step>);

impl Index<usize> for Rules {
    type Output = Step;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index % self.0.len()]
    }
}

pub struct Map<'a> {
    network: Network<'a>,
    rules: Rules,
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum Step {
    L,
    R,
}

impl Step {
    fn from_char(c: char) -> Option<Self> {
        match c {
            'L' => Some(Self::L),
            'R' => Some(Self::R),
            _ => None,
        }
    }
}

pub fn parse(inpt: &str) -> Map {
    let (rules, rest) = inpt.split_once("\n\n").unwrap();
    let rules = rules
        .chars()
        .map(Step::from_char)
        .collect::<Option<Vec<_>>>()
        .map(Rules)
        .unwrap();

    let mut network = Network::default();

    for line in rest.lines() {
        let key = &line[0..3];
        let left = &line[7..10];
        let right = &line[12..15];

        network.insert(key, (left, right));
    }

    Map { rules, network }
}

pub fn part1(map: &Map) -> usize {
    let mut node = "AAA";
    let mut num_steps = 0;

    loop {
        if node == "ZZZ" {
            break num_steps;
        }

        node = match map.rules[num_steps] {
            Step::L => map.network.get(node).unwrap().0,
            Step::R => map.network.get(node).unwrap().1,
        };

        num_steps += 1;
    }
}

pub fn part2(map: &Map) -> usize {
    map.network
        .par_iter()
        .filter(|(k, _)| k.ends_with('A'))
        .map(|(node, _)| {
            let mut node = *node;
            let mut num_steps = 0;

            loop {
                if node.ends_with('Z') {
                    break;
                }

                node = match map.rules[num_steps] {
                    Step::L => map.network.get(node).unwrap().0,
                    Step::R => map.network.get(node).unwrap().1,
                };

                num_steps += 1;
            }

            num_steps
        })
        .collect::<Vec<_>>()
        .into_iter()
        .fold(1, lcm)
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part1() {
        let input1 = "RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)
";
        let input2 = "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)
";
        assert_eq!(part1(&parse(input1)), 2);
        assert_eq!(part1(&parse(input2)), 6);
    }
    #[test]
    fn test_part2() {
        let input = "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)
";
        assert_eq!(part2(&parse(input)), 6);
    }
}
