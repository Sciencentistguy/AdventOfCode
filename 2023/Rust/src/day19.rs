use std::collections::HashMap;

#[derive(Debug, Clone, Copy)]
enum Operation {
    Gt,
    Lt,
}

#[derive(Debug, Clone, Copy)]
enum Destination<'a> {
    Accept,
    Reject,
    Other(&'a str),
}

#[derive(Debug, Clone, Copy)]
enum Kind {
    X = 0,
    M,
    A,
    S,
}

impl Kind {
    fn index(self) -> usize {
        self as usize
    }
}

#[derive(Debug)]
struct Rule<'a> {
    condition: Option<(Kind, Operation, u64)>,
    destination: Destination<'a>,
}

impl Rule<'_> {
    fn matches(&self, part: &Part) -> bool {
        if self.condition.is_none() {
            return true;
        }
        let (kind, op, value) = self.condition.as_ref().unwrap();
        let to_match = match kind {
            Kind::X => part.x,
            Kind::M => part.m,
            Kind::A => part.a,
            Kind::S => part.s,
        };
        match op {
            Operation::Gt => to_match > *value,
            Operation::Lt => to_match < *value,
        }
    }
}

#[derive(Debug)]
pub struct Workflow<'a>(Vec<Rule<'a>>);

#[derive(Debug, Default)]
pub struct Part {
    x: u64,
    m: u64,
    a: u64,
    s: u64,
}

impl Part {
    fn value(&self) -> u64 {
        self.x + self.m + self.a + self.s
    }
}

impl From<u8> for Kind {
    fn from(value: u8) -> Self {
        match value {
            b'x' => Self::X,
            b'm' => Self::M,
            b'a' => Self::A,
            b's' => Self::S,
            _ => unreachable!("invalid kind {}", value as char),
        }
    }
}

impl From<u8> for Operation {
    fn from(value: u8) -> Self {
        match value {
            b'>' => Self::Gt,
            b'<' => Self::Lt,
            _ => unreachable!("invalid operation {}", value as char),
        }
    }
}

impl<'a> From<&'a str> for Destination<'a> {
    fn from(value: &'a str) -> Self {
        match value {
            "A" => Self::Accept,
            "R" => Self::Reject,
            _ => Self::Other(value),
        }
    }
}

#[derive(Clone)]
struct ValueRange {
    min: u64,
    max: u64,
}

impl ValueRange {
    fn new(min: u64, max: u64) -> Self {
        Self { min, max }
    }

    fn combination_count(&self) -> u64 {
        self.max + 1 - self.min
    }

    fn split(&self, threshold: u64, op: Operation) -> (ValueRange, ValueRange) {
        match op {
            Operation::Gt => (
                ValueRange::new(self.min, threshold),
                ValueRange::new(threshold + 1, self.max),
            ),
            Operation::Lt => (
                ValueRange::new(threshold, self.max),
                ValueRange::new(self.min, threshold - 1),
            ),
        }
    }

    fn is_empty(&self) -> bool {
        self.min > self.max
    }
}

pub fn parse(input: &str) -> (HashMap<&str, Workflow<'_>>, Vec<Part>) {
    let (workflows, parts) = input.split_once("\n\n").unwrap();

    let workflows = workflows
        .lines()
        .map(|line| {
            let (name, line) = line.split_once('{').unwrap();
            let line = line.strip_suffix('}').unwrap();
            let rules = line
                .split(',')
                .map(|r| {
                    let colon = r.find(':');
                    if colon.is_none() {
                        return Rule {
                            condition: None,
                            destination: Destination::from(r),
                        };
                    }
                    let (r, destination) = r.split_at(colon.unwrap());
                    let destination = &destination[1..];
                    let kind = r.as_bytes()[0].into();
                    let op = r.as_bytes()[1].into();
                    let value = r[2..].parse().unwrap();
                    Rule {
                        condition: Some((kind, op, value)),
                        destination: Destination::from(destination),
                    }
                })
                .collect::<Vec<_>>();
            (name, Workflow(rules))
        })
        .collect::<HashMap<_, _>>();

    let parts = parts
        .lines()
        .map(|line| {
            let line = line[1..].strip_suffix('}').unwrap();
            let mut part = Part::default();
            for segment in line.split(',') {
                let (kind, value) = segment.split_once('=').unwrap();
                let value = value.parse().unwrap();
                match kind.as_bytes()[0] {
                    b'x' => part.x = value,
                    b'm' => part.m = value,
                    b'a' => part.a = value,
                    b's' => part.s = value,
                    _ => unreachable!("invalid kind in part: '{kind}' (in '{line}')"),
                }
            }
            part
        })
        .collect();

    (workflows, parts)
}

pub fn part1((workflows, parts): &(HashMap<&str, Workflow>, Vec<Part>)) -> u64 {
    let mut ret = 0;
    for part in parts {
        let mut current_workflow = "in";
        ret += 'outer: loop {
            let wf = &workflows[current_workflow];
            for rule in &wf.0 {
                if rule.matches(part) {
                    match rule.destination {
                        Destination::Accept => break 'outer part.value(),
                        Destination::Reject => break 'outer 0,
                        Destination::Other(x) => {
                            current_workflow = x;
                            continue 'outer;
                        }
                    }
                }
            }
            break 0;
        };
    }

    ret
}

pub fn part2((rules, _): &(HashMap<&str, Workflow>, Vec<Part>)) -> u64 {
    let mut queue = vec![(
        "in",
        [
            ValueRange::new(1, 4000),
            ValueRange::new(1, 4000),
            ValueRange::new(1, 4000),
            ValueRange::new(1, 4000),
        ],
    )];

    let mut ret = 0;
    while let Some((id, bounds)) = queue.pop() {
        let rule = &rules[id];

        let mut bounds = bounds;
        for rule in &rule.0 {
            let mut inner_bounds = bounds.clone();
            let Some(&(kind, op, value)) = rule.condition.as_ref() else {
                continue;
            };
            (bounds[kind.index()], inner_bounds[kind.index()]) =
                bounds[kind.index()].split(value, op);

            if !inner_bounds[kind.index()].is_empty() {
                match rule.destination {
                    Destination::Reject => (),
                    Destination::Accept => {
                        ret += inner_bounds
                            .iter()
                            .map(ValueRange::combination_count)
                            .product::<u64>();
                        continue;
                    }
                    Destination::Other(name) => queue.push((name, inner_bounds)),
                }
            }

            if bounds[kind.index()].is_empty() {
                continue;
            }
        }

        match rule.0.last().unwrap().destination {
            Destination::Reject => continue,
            Destination::Accept => {
                ret += bounds
                    .iter()
                    .map(ValueRange::combination_count)
                    .product::<u64>();
            }
            Destination::Other(name) => queue.push((name, bounds)),
        }
    }
    ret
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &str = "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}

{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}
";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&dbg!(parse(INPUT))), 19114);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 167409079868000);
    }
}
