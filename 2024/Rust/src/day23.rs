use fxhash::FxHashMap as HashMap;
use fxhash::FxHashSet as HashSet;
use itertools::Itertools;
use tap::Tap;

type Parsed<'a> = HashMap<&'a str, HashSet<&'a str>>;
type Solution = u64;

pub fn parse(input: &str) -> Parsed<'_> {
    let lines = input.lines().collect::<Vec<_>>();
    let mut rules: HashMap<&str, HashSet<&str>> = HashMap::default();
    for line in lines {
        let (left, right) = line.split_once('-').unwrap();
        rules.entry(left).or_default().insert(right);
        rules.entry(right).or_default().insert(left);
    }
    rules
}

pub fn part1(parsed: &Parsed) -> Solution {
    let mut ret = 0;
    for ((a, a_set),(b, b_set),(c, c_set)) in parsed.iter().tuple_combinations() {
        // find all sets of three where a -> b, a -> c, b -> c
        if a_set.contains(b) && a_set.contains(c) && b_set.contains(c) {
            // note this should also imply that b -> a, c -> a, c -> b
            debug_assert!(b_set.contains(a) && c_set.contains(a) && c_set.contains(b));
            if a.starts_with('t') || b.starts_with('t') || c.starts_with('t') {
                ret += 1;
            }
        }
    }
    ret
}

pub fn part2(parsed: &Parsed) -> String {
    fn dfs<'a>(
        graph: &HashMap<&'a str, HashSet<&'a str>>,
        node: &'a str,
        current_subgraph: &mut HashSet<&'a str>,
    ) {
        if !current_subgraph.insert(node) {
            return;
        }

        if let Some(neighbors) = graph.get(node) {
            for &neighbor in neighbors {
                if current_subgraph
                    .iter()
                    .all(|x| graph.get(neighbor).unwrap().contains(x))
                {
                    dfs(graph, neighbor, current_subgraph);
                }
            }
        }
    }

    let mut largest_total_subgraph = HashSet::default();
    let mut visited: HashSet<&str> = HashSet::default();

    // Find each connected component
    for &start_node in parsed.keys() {
        if !visited.contains(start_node) {
            let mut current_component = HashSet::default();
            dfs(parsed, start_node, &mut current_component);

            visited.extend(current_component.iter());

            // Keep the larger component
            if current_component.len() > largest_total_subgraph.len() {
                largest_total_subgraph = current_component;
            }
        }
    }

    let v = largest_total_subgraph
        .iter()
        .copied()
        .collect::<Vec<_>>()
        .tap_mut(|x| x.sort_unstable());
    v.join(",")
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "kh-tc
qp-kh
de-cg
ka-co
yn-aq
qp-ub
cg-tb
vc-aq
tb-ka
wh-tc
yn-cg
kh-ub
ta-co
de-co
tc-td
tb-wq
wh-td
ta-ka
td-qp
aq-cg
wq-ub
ub-vc
de-ta
wq-aq
wq-vc
wh-yn
ka-de
kh-ta
co-tc
wh-qp
tb-vc
td-yn
";

    const P1_SOLUTION: Solution = 7;
    const P2_SOLUTION: &str = "co,de,ka,ta";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
