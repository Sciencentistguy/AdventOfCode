use std::{
    collections::{HashSet, VecDeque},
    mem::MaybeUninit,
};

use common::Vec2D;
use ndarray::{Array2, Axis};

type Parsed = Array2<usize>;
type Solution = usize;

pub fn parse(input: &str) -> Parsed {
    let lines = input.lines().collect::<Vec<_>>();

    let mut arr: Array2<MaybeUninit<usize>> = Array2::uninit((lines[0].len(), lines.len()));

    #[cfg(debug_assertions)]
    let mut indices = HashSet::new();

    for (i, line) in lines.iter().enumerate() {
        for (j, ch) in line.bytes().enumerate() {
            #[cfg(debug_assertions)]
            indices.insert((j, i));

            unsafe {
                arr[(j, i)]
                    .as_mut_ptr()
                    .write((ch as char).to_digit(10).unwrap() as usize)
            };
        }
    }

    #[cfg(debug_assertions)]
    {
        // check that all indices have been written
        for x in 0..arr.len_of(Axis(0)) {
            for y in 0..arr.len_of(Axis(1)) {
                debug_assert!(indices.contains(&(x, y)));
            }
        }
    }
    unsafe { arr.assume_init() }
}

const DIRECTIONS: [Vec2D<isize>; 4] = [
    Vec2D { x: 0, y: 1 },
    Vec2D { x: 1, y: 0 },
    Vec2D { x: 0, y: -1 },
    Vec2D { x: -1, y: 0 },
];

fn bfs(parsed: &Parsed, start: Vec2D<usize>) -> usize {
    let mut score = 0;
    let mut visited = Array2::from_elem(parsed.dim(), false);
    let mut queue = VecDeque::new();
    queue.push_back((start, 0));

    while let Some((c, ch)) = queue.pop_front() {
        if visited[c] {
            continue;
        }
        visited[c] = true;

        if ch == 9 {
            score += 1;
            continue;
        }

        for dir in DIRECTIONS {
            let n = c.checked_add_signed(dir);

            if let Some(n) = n
                && n.y < parsed.nrows()
                && n.x < parsed.ncols()
                && !visited[n]
                && parsed[n] == ch + 1
            {
                queue.push_back((n, ch + 1));
            }
        }
    }

    score
}

fn dfs(parsed: &Parsed, visited: &mut Array2<bool>, pos: Vec2D<usize>, ch: usize) -> usize {
    if ch == 9 {
        return 1;
    }

    let mut count = 0;
    for dir in DIRECTIONS {
        let n = pos.checked_add_signed(dir);

        if let Some(n) = n
            && n.y < parsed.nrows()
            && n.x < parsed.ncols()
            && !visited[n]
            && parsed[n] == ch + 1
        {
            visited[n] = true;
            count += dfs(parsed, visited, n, ch + 1);
            visited[n] = false;
        }
    }

    count
}

pub fn part1(parsed: &Parsed) -> Solution {
    let mut total_score = 0;
    for (pos, &height) in parsed.indexed_iter() {
        if height == 0 {
            total_score += bfs(parsed, pos.into());
        }
    }
    total_score
}

pub fn part2(parsed: &Parsed) -> Solution {
    let mut total_rating = 0;

    for (pos, &height) in parsed.indexed_iter() {
        if height == 0 {
            let mut visited = Array2::from_elem(parsed.dim(), false);
            visited[pos] = true;
            total_rating += dfs(parsed, &mut visited, pos.into(), 0);
        }
    }

    total_rating
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732";

    const P1_SOLUTION: Solution = 36;
    const P2_SOLUTION: Solution = 81;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
