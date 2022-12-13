use std::cmp::Ordering;

type List = Vec<Element>;

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Element {
    Number(u8),
    List(List),
}

impl Ord for Element {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Element::Number(a), Element::Number(b)) => a.cmp(b),
            (Element::List(a), Element::List(b)) => match a.iter().cmp(b) {
                Ordering::Equal => a.len().cmp(&b.len()),
                other => other,
            },
            (a @ Element::Number(_), Element::List(b)) => {
                let x = vec![a.clone()];
                x.cmp(b)
            }
            (Element::List(a), b @ Element::Number(_)) => {
                let x = vec![b.clone()];
                a.cmp(&x)
            }
        }
    }
}

impl PartialOrd for Element {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

pub fn parse(inpt: &str) -> Vec<List> {
    let mut stack = Vec::new();
    inpt.lines()
        .filter(|l| !l.is_empty())
        .map(|line| {
            const STARTING_CAPACITY: usize = 8;
            stack.clear();
            stack.push(List::with_capacity(STARTING_CAPACITY));
            for c in line.as_bytes() {
                match c {
                    b'[' => {
                        stack.push(List::with_capacity(STARTING_CAPACITY));
                    }
                    b']' => {
                        let last = stack.pop().unwrap();
                        stack.last_mut().unwrap().push(Element::List(last));
                    }
                    b',' => {}
                    c @ b'0'..=b'9' => {
                        stack.last_mut().unwrap().push(Element::Number(c - b'0'));
                    }
                    _ => unreachable!(),
                }
            }
            assert_eq!(stack.len(), 1);
            stack.pop().unwrap()
        })
        .collect()
}

pub fn part1(parsed: &[List]) -> usize {
    parsed
        .iter()
        .array_chunks()
        .enumerate()
        .filter(|(_, [a, b])| a.cmp(b) == Ordering::Less)
        .map(|(x, _)| x + 1)
        .sum()
}

pub fn part2(parsed: &[List]) -> usize {
    let first = vec![Element::List(vec![Element::Number(2)])];
    let second = vec![Element::List(vec![Element::Number(6)])];

    let packets = parsed.iter().filter(|i| *i < &second).collect::<Vec<_>>();

    (packets.iter().filter(|&i| *i < &first).count() + 1) * (packets.len() + 2)
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 13);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 140);
    }
}
