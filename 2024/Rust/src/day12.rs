use std::collections::HashSet;

use common::Vec2D;
use ndarray::Array1;
use smallvec::SmallVec;
use rayon::prelude::*;

type Parsed = Vec<(char, HashSet<Vec2D<usize>>)>;
type Solution = usize;

pub fn parse(input: &str) -> Parsed {
    let lines = input.lines().collect::<Vec<_>>();
    let shape = (lines.len(), lines[0].len());
    let arr: Array1<u8> = input.lines().flat_map(|x| x.bytes()).collect();
    let arr = arr.to_shape(shape).unwrap();

    let mut plots = Vec::new();
    let mut seen: HashSet<Vec2D<usize>> = HashSet::new();

    const DIRECTIONS: [Vec2D<isize>; 4] = [
        Vec2D::new(0, 1),
        Vec2D::new(1, 0),
        Vec2D::new(0, -1),
        Vec2D::new(-1, 0),
    ];
    for (starting_point, &ch) in arr.indexed_iter() {
        // dfs to find all points in plot that match ch
        let starting_point: Vec2D<usize> = starting_point.into();
        if seen.contains(&starting_point) {
            continue;
        }
        let mut stack = vec![starting_point];
        let mut current_plot = HashSet::new();
        let mut tried = HashSet::new();
        while let Some(point) = stack.pop() {
            if tried.contains(&point) {
                continue;
            }
            tried.insert(point);

            if arr[point] == ch {
                current_plot.insert(point);
                for dir in DIRECTIONS {
                    let Some(new_point) = point.checked_add_signed(dir) else {
                        continue;
                    };
                    if new_point.x < shape.0 && new_point.y < shape.1 {
                        stack.push(new_point);
                    }
                }
            }
        }
        seen.extend(&current_plot);
        plots.push((ch as char, current_plot));
    }
    plots
}

pub fn part1(parsed: &Parsed) -> Solution {
    parsed.par_iter().map(|(_, region)| {
        let area = region.len();
        let perimeter: usize = region
            .iter()
            .map(|&point| {
                point
                    .signed_neighbours()
                    .filter(|neighbour| {
                        neighbour
                            .unsigned()
                            .map(|neighbour| !region.contains(&neighbour))
                            .unwrap_or(true)
                    })
                    .count()
            })
            .sum();
        area * perimeter
    }).sum()
}

fn sides_from_edges(edges: &HashSet<(Vec2D<usize>, Vec2D<isize>)>) -> usize {
    let contains = |x: Option<_>, y| x.map(|x| edges.contains(&(x, y))).unwrap_or(false);
    edges
        .par_iter()
        .filter(|(point, dir)| match *dir {
            Vec2D::<usize>::UP if !contains(point.right(), Vec2D::<usize>::UP) => true,
            Vec2D::<usize>::DOWN if !contains(point.right(), Vec2D::<usize>::DOWN) => true,
            Vec2D::<usize>::LEFT if !contains(point.down(), Vec2D::<usize>::LEFT) => true,
            Vec2D::<usize>::RIGHT if !contains(point.down(), Vec2D::<usize>::RIGHT) => true,
            _ => false,
        })
        .count()
}

pub fn part2(parsed: &Parsed) -> Solution {
    parsed
        .par_iter()
        .map(|(_plant, region)| {
            let area = region.len();
            let edges = region
                .iter()
                .flat_map(|&cell| {
                    let mut edges = SmallVec::<_, 4>::new();
                    let contains = |x: Option<_>| x.map(|x| region.contains(&x)).unwrap_or(false);
                    if !contains(cell.up()) {
                        edges.push((cell, Vec2D::<usize>::UP));
                    }
                    if !contains(cell.down()) {
                        edges.push((cell, Vec2D::<usize>::DOWN));
                    }
                    if !contains(cell.left()) {
                        edges.push((cell, Vec2D::<usize>::LEFT));
                    }
                    if !contains(cell.right()) {
                        edges.push((cell, Vec2D::<usize>::RIGHT));
                    }
                    edges
                })
                .collect();
            let sides = sides_from_edges(&edges);
            area * sides
        })
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

    const INPUT: &str = "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
";

    const P1_SOLUTION: Solution = 1930;
    const P2_SOLUTION: Solution = 1206;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
