use std::{collections::HashSet, convert::Infallible, simd::prelude::*, str::FromStr};

use atoi::FromRadix10;
use itertools::Itertools;
use petgraph::{
    prelude::*,
    unionfind::UnionFind,
    visit::{IntoEdgeReferences, NodeCompactIndexable},
};
use rayon::prelude::*;

pub struct Parsed {
    graph: UnGraph<Vec3D, ()>,
    pairs: Vec<((Vec3D, NodeIndex), (Vec3D, NodeIndex))>,
}
type Solution = u64;

#[repr(C)]
#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct Vec3D {
    x: u64,
    y: u64,
    z: u64,
}

impl Vec3D {
    fn as_array(&self) -> &[u64; 3] {
        // Safety: Self is repr(C), so the 3 fields are guaranteed to be contiguous and in order.
        unsafe { std::mem::transmute(self) }
    }

    // A simd-powered 3D pythagoras
    fn distance(&self, other: &Vec3D) -> f64 {
        let v_self = Simd::from_array(*self.as_array()).cast::<f64>();
        let v_other = Simd::from_array(*other.as_array()).cast::<f64>();

        let v_d = v_self - v_other;

        let v_sq = v_d * v_d;

        v_sq.reduce_sum().sqrt()
    }
}

impl FromStr for Vec3D {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.as_bytes();

        let (x, i) = u64::from_radix_10(s);
        debug_assert!(s[i] == b',');
        let s = &s[i + 1..];

        let (y, i) = u64::from_radix_10(s);
        debug_assert!(s[i] == b',');
        let s = &s[i + 1..];

        let (z, _) = u64::from_radix_10(s);

        Ok(Self { x, y, z })
    }
}

pub fn parse(input: &str) -> Parsed {
    let points = input
        .trim()
        .lines()
        .map(|line| Vec3D::from_str(line).unwrap())
        .collect::<Vec<_>>();

    let mut graph = UnGraph::<Vec3D, ()>::new_undirected();

    let mut pairs = points
        .iter()
        .map(|x| (*x, graph.add_node(*x)))
        .collect::<Vec<_>>()
        .into_iter()
        .tuple_combinations()
        .collect::<Vec<(_, _)>>();

    pairs.par_sort_unstable_by(|((a, _), (b, _)), ((c, _), (d, _))| {
        a.distance(b).total_cmp(&c.distance(d))
    });

    Parsed { graph, pairs }
}

pub fn part1(Parsed { graph, pairs }: &Parsed) -> Solution {
    #[cfg(test)]
    const TRIES: usize = 10;
    #[cfg(not(test))]
    const TRIES: usize = 1000;

    let mut graph = graph.clone();

    for ((_, a), (_, b)) in pairs.iter().take(TRIES) {
        graph.add_edge(*a, *b, ());
    }

    let mut visited = HashSet::new();
    let mut component_sizes = Vec::new();
    let mut stack = Vec::new();
    for node in graph.node_indices() {
        if visited.contains(&node) {
            continue;
        }
        stack.clear();
        stack.push(node);
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
}

pub fn part2(Parsed { graph, pairs }: &Parsed) -> Solution {
    let mut graph = graph.clone();

    for ((a_vec, a), (b_vec, b)) in pairs {
        if graph.find_edge(*a, *b).is_none() {
            graph.add_edge(*a, *b, ());
        }
        if connected_components(&graph) == 1 {
            return a_vec.x * b_vec.x;
        }
    }

    unreachable!()
}

// Copied from
// https://github.com/petgraph/petgraph/blob/162903562ce5b00cdba390a0d9c1bb80f1c75bf5/src/algo/mod.rs#L136-L152
// so I can use unwrap_unchecked and save 5% in part2
fn connected_components<G>(g: G) -> usize
where
    G: NodeCompactIndexable + IntoEdgeReferences,
{
    let mut node_sets = UnionFind::new(g.node_bound());
    for edge in g.edge_references() {
        let (a, b) = (edge.source(), edge.target());

        // union the two nodes of the edge
        unsafe {
            node_sets
                .try_union(g.to_index(a), g.to_index(b))
                .unwrap_unchecked()
        };
    }

    let mut labels = node_sets.into_labeling();
    labels.sort_unstable();
    labels.dedup();
    labels.len()
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
