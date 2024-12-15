use fxhash::FxHashMap as HashMap;
use fxhash::FxHashSet as HashSet;

use common::Vec2D;
use ndarray::Array2;

type Parsed = (
    HashSet<Vec2D<usize>>,
    HashSet<Vec2D<usize>>,
    HashSet<Vec2D<usize>>,
    HashMap<Vec2D<usize>, usize>,
    Vec<Vec2D<isize>>,
    Vec2D<usize>,
);
type Solution = usize;

enum Space {
    Space,
    Player,
    Wall,
    Box,
}

struct Grid {
    grid: Array2<Space>,
    player: Vec2D<usize>,
}

impl Grid {
    // Moves the player in the given direction. If there is a box in the way, the player pushes the
    // box. If there is a box in the way of that, it gets pushed as well, recursively. If it hits a
    // wall, it stops, and no movement occurs.
    fn move_player(&mut self, direction: Vec2D<isize>) {
        let new_player = if let Some(x) = self.player.checked_add_signed(direction)
            && x.x < self.grid.ncols()
            && x.y < self.grid.nrows()
        {
            x
        } else {
            panic!("Player moved out of bounds");
        };

        todo!()
    }
}

pub fn parse(input: &str) -> Parsed {
    let (grid, instructions) = input.split_once("\n\n").unwrap();

    let lines = grid.lines().collect::<Vec<_>>();
    let mut walls = HashSet::default();
    let mut walls2 = HashSet::default();
    let mut boxes = HashSet::default();
    let mut boxes2 = HashMap::default();
    let mut starting = None;

    for (y, line) in lines.iter().enumerate() {
        for (x, ch) in line.bytes().enumerate() {
            match ch {
                b'.' => {}
                b'@' => {
                    starting = Some(Vec2D::from((x, y)));
                }
                b'#' => {
                    walls.insert(Vec2D::from((x, y)));
                    walls2.insert(Vec2D::from((x, y)));
                    walls2.insert(Vec2D::from((x + 1, y)));
                }
                b'O' => {
                    boxes.insert(Vec2D::from((x, y)));
                    boxes2.insert(Vec2D::from((x, y)), boxes2.len() / 2);
                    boxes2.insert(Vec2D::from((x + 1, y)), boxes2.len() / 2);
                }
                _ => unreachable!(),
            }
        }
    }

    let instructions = instructions
        .bytes()
        .filter_map(|x| match x {
            b'<' => Some(Vec2D { x: -1, y: 0 }),
            b'>' => Some(Vec2D { x: 1, y: 0 }),
            b'^' => Some(Vec2D { x: 0, y: -1 }),
            b'v' => Some(Vec2D { x: 0, y: 1 }),
            _ => None, // discard invalid characters including newlines
        })
        .collect();

    (
        walls,
        walls2,
        boxes,
        boxes2,
        instructions,
        starting.unwrap(),
    )
}

pub fn part1((walls, _, boxes, _, instructions, starting): &Parsed) -> Solution {
    let mut pos = *starting;
    let mut boxes = boxes.to_owned();
    for instruction in instructions {
        pos = dfs(pos, *instruction, walls, &mut boxes).unwrap_or(pos);
    }
    boxes.iter().map(|b| b.x + 100 * b.y).sum()
}

fn dfs(
    pos: Vec2D<usize>,
    direction: Vec2D<isize>,
    walls: &HashSet<Vec2D<usize>>,
    boxes: &mut HashSet<Vec2D<usize>>,
) -> Option<Vec2D<usize>> {
    let next = pos.checked_add_signed(direction)?;
    if walls.contains(&next) {
        return None;
    }
    if boxes.contains(&next) {
        if let Some(empty) = dfs(next, direction, walls, boxes) {
            boxes.remove(&next);
            boxes.insert(empty);
            return Some(next);
        } else {
            return None;
        }
    }
    Some(next)
}

pub fn part2((_, walls, _, boxes, instructions, starting): &Parsed) -> Solution {
    let mut pos = *starting;
    let mut boxes = boxes.to_owned();
    for instruction in instructions {
        if instruction.y == 0 {
            pos = left_right(pos, *instruction, walls, &mut boxes).unwrap_or(pos);
        } else if instruction.x == 0 {
            pos = up_down(pos, *instruction, walls, &mut boxes, false).unwrap_or(pos);
        }
    }

    let n = boxes.len() / 2;
    let mut xmap: HashMap<_, Vec<_>> = HashMap::default();
    let mut ydist = vec![0; n+1];
    for b in boxes {
        xmap.entry(b.1).or_default().push(b.0.x);
        ydist[b.1] = b.0.y;
    }
    let mut res = 0;
    for i in 0..n {
        res += xmap[&i].iter().min().unwrap() + 100 * ydist[i];
    }
    res
}

fn left_right(
    curr: Vec2D<usize>,
    dir: Vec2D<isize>,
    walls: &HashSet<Vec2D<usize>>,
    boxes: &mut HashMap<Vec2D<usize>, usize>,
) -> Option<Vec2D<usize>> {
    debug_assert!(dir.y == 0 && dir.x == -1 || dir.x == 1);
    let next = curr.checked_add_signed(dir)?;
    if walls.contains(&next) {
        None
    } else if let Some(&id) = boxes.get(&next) {
        if let Some(empty) = left_right(next, dir, walls, boxes) {
            boxes.insert(empty, id);
            boxes.remove(&next);
            Some(next)
        } else {
            None
        }
    } else {
        Some(next)
    }
}

const RIGHT: Vec2D<isize> = Vec2D { x: 1, y: 0 };
const LEFT: Vec2D<isize> = Vec2D { x: -1, y: 0 };

fn up_down(
    curr: Vec2D<usize>,
    dir: Vec2D<isize>,
    walls: &HashSet<Vec2D<usize>>,
    boxes: &mut HashMap<Vec2D<usize>, usize>,
    dry_run: bool,
) -> Option<Vec2D<usize>> {
    debug_assert!(dir.x == 0 && dir.y == -1 || dir.y == 1);
    let next = curr.checked_add_signed(dir)?;
    if walls.contains(&next) {
        None
    } else if let Some(&id) = boxes.get(&next) {
        step(
            boxes,
            next.checked_add_signed(RIGHT).unwrap(),
            id,
            next,
            dir,
            walls,
            dry_run,
        )
        .or_else(|| {
            step(
                boxes,
                next.checked_add_signed(LEFT).unwrap(),
                id,
                next,
                dir,
                walls,
                dry_run,
            )
        })
    } else {
        Some(next)
    }
}

fn step(
    boxes: &mut HashMap<Vec2D<usize>, usize>,
    neighbor: Vec2D<usize>,
    id: usize,
    next: Vec2D<usize>,
    dir: Vec2D<isize>,
    walls: &HashSet<Vec2D<usize>>,
    dry_run: bool,
) -> Option<Vec2D<usize>> {
    if boxes.get(&neighbor).is_some_and(|&v| v == id) {
        if let (Some(v1), Some(v2)) = (
            up_down(next, dir, walls, boxes, dry_run),
            up_down(neighbor, dir, walls, boxes, dry_run),
        ) {
            if !dry_run {
                boxes.remove(&next);
                boxes.remove(&neighbor);
                boxes.insert(v1, id);
                boxes.insert(v2, id);
            }
            return Some(next);
        }
    }
    None
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = todo!();

    const P1_SOLUTION: Solution = todo!();
    const P2_SOLUTION: Solution = todo!();

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
