use std::collections::{HashMap, HashSet};

type Ordering = HashMap<u64, HashSet<u64>>;
type Parsed = (Ordering, Vec<Vec<u64>>);
type Solution = u64;

pub fn parse(input: &str) -> Parsed {
    let (ordering_rules, update) = input.split_once("\n\n").unwrap();
    let ordering = ordering_rules.lines().fold(
        HashMap::<_, HashSet<_>>::new(),
        |mut acc, line| {
            let (a, b) = line.split_once('|').unwrap();
            acc.entry(a.parse().unwrap())
                .or_default()
                .insert(b.parse().unwrap());
            acc
        },
    );

    let updates = update
        .lines()
        .map(|line| line.split(',').map(|x| x.parse().unwrap()).collect())
        .collect();
    (ordering, updates)
}

fn is_ordered(update: &Vec<u64>, ordering: &Ordering) -> bool {
    update.array_windows().all(|[a, b]| ordering[a].contains(b))
}

fn reorder(buf: &mut Vec<u64>, update: &[u64], ordering: &Ordering) {
    buf.clear();
    buf.extend_from_slice(update);
    buf.sort_unstable_by(|a, b| {
        use std::cmp::Ordering as Ord;
        if let Some(b_vals) = ordering.get(b) {
            if b_vals.contains(a) {
                Ord::Greater
            } else {
                Ord::Less
            }
        } else {
            Ord::Equal
        }
    })
}

fn find_middle(update: &Vec<u64>) -> u64 {
    update[update.len() / 2]
}

pub fn part1((ordering_rules, updates): &Parsed) -> Solution {
    updates
        .iter()
        .filter_map(|update| {
            if is_ordered(update, ordering_rules) {
                Some(find_middle(update))
            } else {
                None
            }
        })
        .sum()
}

pub fn part2((ordering_rules, updates): &Parsed) -> Solution {
    let mut ret = 0;
    let out_of_order_updates = updates
        .iter()
        .filter(|update| !is_ordered(update, ordering_rules));
    let mut buf = Vec::new();
    for update in out_of_order_updates {
        reorder(&mut buf, update, ordering_rules);
        debug_assert!(is_ordered(&buf, ordering_rules));
        ret += find_middle(&buf);
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

    const INPUT: &str = "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47
";

    const P1_SOLUTION: Solution = 143;
    const P2_SOLUTION: Solution = 123;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
