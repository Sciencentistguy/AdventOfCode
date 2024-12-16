use std::collections::VecDeque;

use fxhash::FxHashMap as HashMap;
use fxhash::FxHashSet as HashSet;

use common::Vec2D;
use itertools::Itertools;
use ndarray::Array2;

type Parsed = (Maze, Vec2D<usize>, Vec2D<usize>);
type Solution = usize;

type Maze = Array2<bool>;

pub fn parse(input: &str) -> Parsed {
    let lines = input.lines().collect::<Vec<_>>();
    let width = lines[0].len();
    let height = lines.len();
    let mut start = None;
    let mut end = None;
    let mut grid = Array2::from_elem((height, width), false);
    for (y, line) in lines.iter().enumerate() {
        for (x, ch) in line.chars().enumerate() {
            match ch {
                'S' => start = Some(Vec2D::new(x, y)),
                'E' => end = Some(Vec2D::new(x, y)),
                '.' => {}
                '#' => grid[[y, x]] = true,
                x => unreachable!("Invalid character in maze: {x}"),
            }
        }
    }
    (grid, start.unwrap(), end.unwrap())
}

type PredMap = HashMap<(Vec2D<usize>, usize), Vec<(Vec2D<usize>, usize)>>;

static DIRECTIONS: [fn(Vec2D<usize>) -> Vec2D<usize>; 4] = [
    |Vec2D { x, y }| Vec2D::new(x + 1, y),
    |Vec2D { x, y }| Vec2D::new(x, y - 1),
    |Vec2D { x, y }| Vec2D::new(x - 1, y),
    |Vec2D { x, y }| Vec2D::new(x, y + 1),
];

fn move_or_rotate(curr_pos: Vec2D<usize>, curr_dir: usize, rot: usize) -> (Vec2D<usize>, usize) {
    let next_dir = (curr_dir + rot) % 4;
    let next_pos = if rot == 0 {
        DIRECTIONS[next_dir](curr_pos)
    } else {
        curr_pos
    };
    (next_pos, next_dir)
}

fn find_path(
    map: &Maze,
    start: Vec2D<usize>,
    goal: Vec2D<usize>,
) -> Option<(Vec2D<usize>, usize, isize, PredMap)> {
    let start_dir = 0;
    let start_score = 0;
    let mut stack = VecDeque::from([(start, start_dir, start_score)]);
    let mut path_node = HashMap::default();
    path_node.insert((start, start_dir), start_score);
    let mut predecessors = PredMap::default();

    while let Some((curr_pos, curr_dir, score)) = stack.pop_front() {
        for (rot, cost) in [(3, -1000), (0, -1), (1, -1000)] {
            let next_node @ (next_pos, next_dir) = move_or_rotate(curr_pos, curr_dir, rot);
            let new_score = score + cost;
            let old_score = path_node.get(&(next_pos, next_dir)).copied();

            if !map[next_pos] && Some(new_score) >= old_score {
                if Some(new_score) > old_score && curr_pos != goal {
                    stack.push_back((next_pos, next_dir, new_score));
                }

                path_node.remove(&next_node);
                path_node.insert(next_node, new_score);

                // Update next node predecessors
                let mut new_preds = vec![(curr_pos, curr_dir)];
                if old_score == Some(new_score)
                    && let Some(predecessors) = predecessors.remove(&next_node)
                {
                    new_preds.extend_from_slice(&predecessors);
                }
                predecessors.insert(next_node, new_preds);
            }
        }
    }
    let (direction_arrival, score) = (0..4)
        .filter_map(|dir| Some((dir, path_node.get(&(goal, dir))?)))
        .max_by_key(|(_dir, score)| **score)?;
    Some((goal, direction_arrival, -*score, predecessors))
}

pub fn part1((maze, start, end): &Parsed) -> Solution {
    find_path(maze, *start, *end).unwrap().2 as usize
}

pub fn part2((maze, start, end): &Parsed) -> Solution {
    let (goal, arrival_dir, _, preds) = find_path(maze, *start, *end).unwrap();

    let mut visited = HashSet::default();
    let mut stack_back = VecDeque::from([(goal, arrival_dir)]);
    while let Some((pos, curr_dir)) = stack_back.pop_front() {
        if visited.insert((pos, curr_dir))
            && let Some(preds) = preds.get(&(pos, curr_dir))
        {
            stack_back.extend(preds)
        }
    }

    visited.into_iter().counts_by(|(coord, _dir)| coord).len()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############
";

    const P1_SOLUTION: Solution = 7036;
    const P2_SOLUTION: Solution = 45;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
