use std::collections::HashMap;

type Parsed = (Vec<(u64, u64)>, Vec<Vec<u64>>);
type Solution = u64;

pub fn parse(input: &str) -> Parsed {
    let (ordering_rules, update) = input.split_once("\n\n").unwrap();
    let ordering_rules = ordering_rules
        .lines()
        .map(|line| {
            let (a, b) = line.split_once('|').unwrap();
            (a.parse().unwrap(), b.parse().unwrap())
        })
        .collect();

    let update = update
        .lines()
        .map(|line| line.split(',').map(|x| x.parse().unwrap()).collect())
        .collect();
    (ordering_rules, update)
}

fn is_correctly_ordered(update: &Vec<u64>, ordering_rules: &Vec<(u64, u64)>) -> bool {
    for &(from, to) in ordering_rules {
        let pos_a = update.iter().position(|&x| x == from);
        let pos_b = update.iter().position(|&x| x == to);
        if let (Some(pos_a), Some(pos_b)) = (pos_a, pos_b) {
            if pos_a > pos_b {
                return false;
            }
        }
    }
    true
}
fn find_middle(update: &Vec<u64>) -> u64 {
    update[update.len() / 2]
}

pub fn part1((ordering_rules, updates): &Parsed) -> Solution {
    updates
        .iter()
        .filter_map(|update| {
            if is_correctly_ordered(update, ordering_rules) {
                Some(find_middle(update))
            } else {
                None
            }
        })
        .sum()
}

fn put_in_correct_ordering(update: &mut [u64], ordering_rules: &[(u64, u64)]) {
    let mut graph = HashMap::new();
    let mut in_degree = HashMap::new();

    for &(from, to) in ordering_rules {
        graph.entry(from).or_insert_with(Vec::new).push(to);
        *in_degree.entry(to).or_insert(0) += 1;
        in_degree.entry(from).or_insert(0);
    }

    let mut queue: std::collections::VecDeque<u64> = in_degree.iter()
        .filter(|&(_, &deg)| deg == 0)
        .map(|(&node, _)| node)
        .collect();

    let mut sorted = Vec::new();
    while let Some(node) = queue.pop_front() {
        sorted.push(node);
        if let Some(neighbors) = graph.get(&node) {
            for &neighbor in neighbors {
                let deg = in_degree.get_mut(&neighbor).unwrap();
                *deg -= 1;
                if *deg == 0 {
                    queue.push_back(neighbor);
                }
            }
        }
    }

    let mut position = HashMap::new();
    for (i, &page) in sorted.iter().enumerate() {
        position.insert(page, i);
    }

    update.sort_by_key(|&page| position.get(&page).cloned().unwrap_or(usize::MAX));
}

pub fn part2((ordering_rules, updates): &Parsed) -> Solution {
    let mut out_of_order_updates: Vec<_> = updates
        .iter()
        .filter(|update| !is_correctly_ordered(update, ordering_rules))
        .cloned()
        .collect();
    for update in &mut out_of_order_updates {
        dbg!(&update);
        put_in_correct_ordering(update, ordering_rules);
        dbg!(&update);
        assert!(is_correctly_ordered(update, ordering_rules));
    }
    // panic!();
    out_of_order_updates
        .iter()
        .map(|update| find_middle(update))
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
