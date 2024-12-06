use fxhash::FxBuildHasher;
use fxhash::FxHashMap as HashMap;
use fxhash::FxHashSet as HashSet;

use common::Vec2D;
use rayon::prelude::*;
use smallvec::SmallVec;

type Parsed = (Guard, HashMap<Vec2D<usize>, bool>);
type Solution = usize;

#[derive(Debug, Default, Clone, Copy)]
pub struct Guard {
    pos: Vec2D<usize>,
    facing: Vec2D<isize>,
}

impl Guard {
    const UP: Vec2D<isize> = Vec2D::new(0, -1);

    fn move_forward(&mut self, map: &HashMap<Vec2D<usize>, bool>) -> bool {
        self.move_forward_with_extra_barrier(map, None)
    }
    fn move_forward_with_extra_barrier(
        &mut self,
        map: &HashMap<Vec2D<usize>, bool>,
        barrier: Option<Vec2D<usize>>,
    ) -> bool {
        let new_pos = Vec2D::new(
            (self.pos.x as isize + self.facing.x) as usize,
            (self.pos.y as isize + self.facing.y) as usize,
        );

        if *map.get(&new_pos).unwrap_or(&false) || barrier == Some(new_pos) {
            false
        } else {
            self.pos = new_pos;
            true
        }
    }

    fn turn_right(&mut self) {
        self.facing.rotate_right();
    }
}

pub fn parse(input: &str) -> Parsed {
    let mut ret = HashMap::with_capacity_and_hasher(input.len(), Default::default());
    let mut guard = None;
    for (y, line) in input.lines().enumerate() {
        for (x, c) in line.bytes().enumerate() {
            match c {
                b'.' => {
                    ret.insert(Vec2D::new(x, y), false);
                }
                b'^' => {
                    guard = Some(Guard {
                        pos: Vec2D::new(x, y),
                        facing: Guard::UP,
                    });
                    ret.insert(Vec2D::new(x, y), false);
                }
                b'#' => {
                    ret.insert(Vec2D::new(x, y), true);
                }

                _ => unreachable!("Invalid character: {}", c as char),
            }
        }
    }
    (guard.expect("Failed to find starting point"), ret)
}

fn walk_map(mut current: Guard, map: &HashMap<Vec2D<usize>, bool>) -> HashSet<Vec2D<usize>> {
    let mut visited = HashSet::default();
    visited.insert(current.pos);
    loop {
        if !current.move_forward(map) {
            current.turn_right();
        }

        if !map.contains_key(&current.pos) {
            return visited;
        }

        visited.insert(current.pos);
    }
}

fn find_loop(
    mut guard: Guard,
    map: &HashMap<Vec2D<usize>, bool>,
    barrier: Vec2D<usize>,
    visited: &mut HashMap<Vec2D<usize>, SmallVec<Vec2D<isize>, 4>>,
) -> bool {
    visited.clear();
    visited.insert(guard.pos, Default::default());
    visited.get_mut(&guard.pos).unwrap().push(guard.facing);

    loop {
        if !guard.move_forward_with_extra_barrier(map, Some(barrier)) {
            guard.turn_right();
        }

        if visited
            .entry(guard.pos)
            .or_default()
            .contains(&guard.facing)
        {
            return true;
        }
        visited.get_mut(&guard.pos).unwrap().push(guard.facing);

        if !map.contains_key(&guard.pos) {
            return false;
        }
    }
}

pub fn part1((guard, map): &Parsed) -> (Solution, HashSet<Vec2D<usize>>) {
    let mut x = walk_map(*guard, map);
    let ret = x.len();
    x.remove(&guard.pos); // for part 2
    (ret, x)
}

#[allow(clippy::deref_addrof)] // This is required for static mut access
pub fn part2((guard, map): &Parsed, visited: &HashSet<Vec2D<usize>>) -> Solution {
    visited
        .par_iter()
        .filter(|&&pos| {
            #[thread_local]
            static mut BUF: HashMap<Vec2D<usize>, SmallVec<Vec2D<isize>, 4>> = HashMap::with_hasher(FxBuildHasher::new());

            // Safety: each invocation of this closure must be on a different thread
            pos != guard.pos && find_loop(*guard, map, pos, unsafe {&mut *&raw mut BUF})
        })
        .count()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    let (part1, visited) = part1(&parsed);
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2(&parsed, &visited));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...
";

    const P1_SOLUTION: Solution = 41;
    const P2_SOLUTION: Solution = 6;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)).0, P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        let visited = part1(&parse(INPUT)).1;
        assert_eq!(part2(&parse(INPUT), &visited), P2_SOLUTION);
    }
}
