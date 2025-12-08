use std::cmp::Ordering;

use itertools::Itertools;
use petgraph::{Graph, dot::Dot, graph::UnGraph};
use tap::Tap;

type Parsed = Vec<Vec3D>;
type Solution = u64;

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
struct Vec3D {
    x: u64,
    y: u64,
    z: u64,
}

impl Vec3D {
    fn distance(&self, other: &Vec3D) -> f64 {
        let dx = (self.x as i64 - other.x as i64) as f64;
        let dy = (self.y as i64 - other.y as i64) as f64;
        let dz = (self.z as i64 - other.z as i64) as f64;
        (dx * dx + dy * dy + dz * dz).sqrt()
    }
}

impl FromIterator<u64> for Vec3D {
    fn from_iter<T: IntoIterator<Item = u64>>(iter: T) -> Self {
        let mut it = iter.into_iter();
        Vec3D {
            x: it.next().unwrap(),
            y: it.next().unwrap(),
            z: it.next().unwrap(),
        }
    }
}

pub fn parse(input: &str) -> Parsed {
    input
        .trim()
        .lines()
        .map(|line| {
            line.split(',')
                .map(|num| num.parse::<u64>().unwrap())
                .collect()
        })
        .collect()
}

pub fn part1(parsed: &Parsed) -> Solution {
    #[cfg(test)]
    const TRIES: usize = 10;
    #[cfg(not(test))]
    const TRIES: usize = 1000;

    let mut graph = UnGraph::<&Vec3D, ()>::new_undirected();
    let pairs = parsed
        .iter()
        .map(|x| graph.add_node(x))
        .collect::<Vec<_>>()
        .into_iter()
        .tuple_combinations()
        .collect::<Vec<(_, _)>>()
        .tap_mut(|x| {
            x.sort_unstable_by(|p1, p2| {
                let a = graph[p1.0];
                let b = graph[p1.1];
                let c = graph[p2.0];
                let d = graph[p2.1];
                // compare via epsilon to avoid floating point precision issues
                let dist_ab = a.distance(b);
                let dist_cd = c.distance(d);
                if (dist_ab - dist_cd).abs() < f64::EPSILON {
                    std::cmp::Ordering::Equal
                } else if dist_ab < dist_cd {
                    std::cmp::Ordering::Less
                } else {
                    std::cmp::Ordering::Greater
                }
            })
        });
    dbg!(&pairs[..10]);

    for (a, b) in pairs.iter().take(TRIES) {
        // let a = graph.add_node(a);
        // let b = graph.add_node(b);
        // only add the nodes in if they're not already
        // let a = graph
        //     .node_indices()
        //     .find(|&i| graph[i] == *a)
        //     .unwrap_or_else(|| graph.add_node(a));
        // let b = graph
        //     .node_indices()
        //     .find(|&i| graph[i] == *b)
        //     .unwrap_or_else(|| graph.add_node(b));

        graph.add_edge(*a, *b, ());
    }

    //extract the connected components and product their sizes
    // petgraph::algo::connected_components(&graph) returns the number of components, not their sizes
    let mut visited = std::collections::HashSet::new();
    let mut component_sizes = Vec::new();
    for node in graph.node_indices() {
        if visited.contains(&node) {
            continue;
        }
        let mut stack = vec![node];
        let mut size = 0;
        while let Some(n) = stack.pop() {
            if visited.insert(n) {
                size += 1;
                for neighbor in graph.neighbors(n) {
                    stack.push(neighbor);
                }
            }
        }
        component_sizes.push(size);
    }
    component_sizes.sort_unstable_by_key(|&size| std::cmp::Reverse(size));
    component_sizes.iter().take(3).product()

    // todo!()
}

pub fn part2(parsed: &Parsed) -> Solution {
    let mut graph = UnGraph::new_undirected();
    let pairs = parsed
        .iter()
        .map(|x| graph.add_node(x))
        .collect::<Vec<_>>()
        .into_iter()
        .tuple_combinations()
        .collect::<Vec<(_, _)>>()
        .tap_mut(|x| {
            x.sort_unstable_by(|p1, p2| {
                let a = graph[p1.0];
                let b = graph[p1.1];
                let c = graph[p2.0];
                let d = graph[p2.1];
                // compare via epsilon to avoid floating point precision issues
                let dist_ab = a.distance(b);
                let dist_cd = c.distance(d);
                if (dist_ab - dist_cd).abs() < f64::EPSILON {
                    std::cmp::Ordering::Equal
                } else if dist_ab < dist_cd {
                    std::cmp::Ordering::Less
                } else {
                    std::cmp::Ordering::Greater
                }
            })
        });
    dbg!(&pairs[..10]);

    for (a, b) in &pairs {
        if graph.find_edge(*a, *b).is_none() {
            graph.add_edge(*a, *b, ());
        }
        if petgraph::algo::connected_components(&graph) > 1 {
            return dbg!(graph[*a]).x * dbg!(graph[*b]).x;
        }
    }

    unreachable!()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689
";

    const P1_SOLUTION: Solution = 40;
    const P2_SOLUTION: Solution = 25272;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
