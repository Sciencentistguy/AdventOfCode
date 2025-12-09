use std::simd::prelude::*;

use atoi::FromRadix10;
use common::{ByteSplit, Vec2D};
use itertools::Itertools;

type Parsed = Vec<Vec2D<u64>>;
type Solution = u64;

struct Edges {
    min_x: Vec<u64>,
    max_x: Vec<u64>,
    min_y: Vec<u64>,
    max_y: Vec<u64>,
}

pub fn parse(input: &str) -> Parsed {
    input
        .trim()
        .as_bytes()
        .byte_lines()
        .map(|line| {
            let (a, b) = line.byte_split_once(b',').unwrap();
            let (x, _) = u64::from_radix_10(a);
            let (y, _) = u64::from_radix_10(b);
            Vec2D { x, y }
        })
        .collect()
}

#[allow(dead_code)] // for debugging
pub fn print_polygon(polygon: &[Vec2D<u64>]) {
    pub fn is_on_boundary(polygon: &[Vec2D<u64>], point: &Vec2D<u64>) -> bool {
        for i in 0..polygon.len() {
            let p1 = polygon[i];
            let p2 = polygon[(i + 1) % polygon.len()];

            if p1.x == p2.x && p1.x == point.x {
                if point.y >= p1.y.min(p2.y) && point.y <= p1.y.max(p2.y) {
                    return true;
                }
            } else if p1.y == p2.y
                && p1.y == point.y
                && point.x >= p1.x.min(p2.x)
                && point.x <= p1.x.max(p2.x)
            {
                return true;
            }
        }
        false
    }

    let min_x = polygon.iter().map(|p| p.x).min().unwrap_or(0);
    let max_x = polygon.iter().map(|p| p.x).max().unwrap_or(0);
    let min_y = polygon.iter().map(|p| p.y).min().unwrap_or(0);
    let max_y = polygon.iter().map(|p| p.y).max().unwrap_or(0);

    for y in min_y.saturating_sub(2)..=max_y + 2 {
        for x in min_x.saturating_sub(2)..=max_x + 2 {
            let p = Vec2D::new(x, y);
            if polygon.contains(&p) {
                print!("#");
            } else if is_on_boundary(polygon, &p) {
                print!("X");
            } else {
                print!(".");
            }
        }
        println!();
    }
}

fn find_edges(polygon: &[Vec2D<u64>]) -> Edges {
    let mut min_x = Vec::with_capacity(polygon.len());
    let mut max_x = Vec::with_capacity(polygon.len());
    let mut min_y = Vec::with_capacity(polygon.len());
    let mut max_y = Vec::with_capacity(polygon.len());
    for i in 0..polygon.len() - 1 {
        let p1 = polygon[i];
        let p2 = polygon[i + 1];

        min_x.push(p1.x.min(p2.x));
        max_x.push(p1.x.max(p2.x));
        min_y.push(p1.y.min(p2.y));
        max_y.push(p1.y.max(p2.y));
    }

    // handle the edge between the last and the first points
    let p_last = polygon[polygon.len() - 1];
    let p_first = polygon[0];
    min_x.push(p_last.x.min(p_first.x));
    max_x.push(p_last.x.max(p_first.x));
    min_y.push(p_last.y.min(p_first.y));
    max_y.push(p_last.y.max(p_first.y));

    Edges {
        min_x,
        max_x,
        min_y,
        max_y,
    }
}

pub fn part1(parsed: &Parsed) -> Solution {
    parsed
        .iter()
        .tuple_combinations()
        .map(|(x1, x2)| {
            let width = x1.x.abs_diff(x2.x) + 1;
            let height = x1.y.abs_diff(x2.y) + 1;
            width * height
        })
        .max()
        .unwrap()
}

#[inline(always)]
fn is_contained_within(
    Edges {
        min_x: edge_min_x,
        max_x: edge_max_x,
        min_y: edge_min_y,
        max_y: edge_max_y,
    }: &Edges,

    min: &Vec2D<u64>,
    max: &Vec2D<u64>,
) -> bool {
    const LANES: usize = 4; // 4 seems to work the best on my machine ¯\_(ツ)_/¯
    type Sv = Simd<u64, LANES>;

    let mut intersects = false;
    let lane_min_x = Sv::splat(min.x);
    let lane_max_x = Sv::splat(max.x);
    let lane_min_y = Sv::splat(min.y);
    let lane_max_y = Sv::splat(max.y);

    let n = edge_min_x.len();
    let mut i = 0;
    while i + LANES <= n {
        let edge_max_x = Sv::from_slice(&edge_max_x[i..i + LANES]);
        let edge_min_x = Sv::from_slice(&edge_min_x[i..i + LANES]);
        let edge_max_y = Sv::from_slice(&edge_max_y[i..i + LANES]);
        let edge_min_y = Sv::from_slice(&edge_min_y[i..i + LANES]);

        let m1 = lane_min_x.simd_lt(edge_max_x);
        let m2 = lane_max_x.simd_gt(edge_min_x);
        let m3 = lane_min_y.simd_lt(edge_max_y);
        let m4 = lane_max_y.simd_gt(edge_min_y);

        if (m1 & m2 & m3 & m4).any() {
            intersects = true;
            break;
        }

        i += LANES;
    }

    // scalar tail
    if !intersects {
        while i < n {
            if min.x < edge_max_x[i]
                && max.x > edge_min_x[i]
                && min.y < edge_max_y[i]
                && max.y > edge_min_y[i]
            {
                intersects = true;
                break;
            }
            i += 1;
        }
    }
    intersects
}

pub fn part2(parsed: &Parsed) -> Solution {
    let edges = find_edges(parsed);

    let mut ret = 0;
    for (p1, p2) in parsed.iter().tuple_combinations() {
        let height = p1.y.abs_diff(p2.y) + 1;
        let width = p1.x.abs_diff(p2.x) + 1;
        let area = width * height;

        if area < ret {
            continue;
        }

        let min = Vec2D {
            x: p1.x.min(p2.x),
            y: p1.y.min(p2.y),
        };

        let max = Vec2D {
            x: p1.x.max(p2.x),
            y: p1.y.max(p2.y),
        };

        if !is_contained_within(&edges, &min, &max) {
            ret = area;
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

    const INPUT: &str = "7,1
11,1
11,7
9,7
9,5
2,5
2,3
7,3";

    const P1_SOLUTION: Solution = 50;
    const P2_SOLUTION: Solution = 24;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
