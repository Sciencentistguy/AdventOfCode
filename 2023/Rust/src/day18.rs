use std::{
    cmp::{max, min},
    collections::VecDeque,
};

use common::ArraySplit;

#[derive(Debug, Clone, Copy)]
enum Direction {
    Up(usize),
    Down(usize),
    Right(usize),
    Left(usize),
}

#[derive(PartialEq, Eq, Clone, Copy)]
enum FieldTile {
    Trench = 1,
    Terrain = 0,
}

#[derive(Debug, Clone)]
pub struct Instruction {
    p1_value: Direction,
    p2_value: Direction,
}

type FieldBB = ((isize, isize), (isize, isize));

fn get_field_size(
    instrs: &[Instruction],
    mut vertices: Option<&mut Vec<(isize, isize)>>,
) -> FieldBB {
    use Direction::*;
    let mut min_x = isize::MAX;
    let mut max_x = isize::MIN;
    let mut min_y = isize::MAX;
    let mut max_y = isize::MIN;
    let mut x: isize = 0;
    let mut y: isize = 0;
    if let Some(vertices) = vertices.as_mut() {
        vertices.push((x, y));
    }
    for instr in instrs {
        match if vertices.is_some() {
            // part 2
            instr.p2_value
        } else {
            instr.p1_value
        } {
            Left(n) => x = x.checked_sub_unsigned(n).unwrap(),
            Right(n) => x = x.checked_add_unsigned(n).unwrap(),
            Up(n) => y = y.checked_sub_unsigned(n).unwrap(),
            Down(n) => y = y.checked_add_unsigned(n).unwrap(),
        };
        min_x = min(min_x, x);
        max_x = max(max_x, x);
        min_y = min(min_y, y);
        max_y = max(max_y, y);

        if let Some(vertices) = vertices.as_mut() {
            vertices.push((x, y));
        }
    }
    ((min_x, max_x), (min_y, max_y))
}

fn fill_map(instrs: &[Instruction], map: &mut [Vec<FieldTile>], dims: FieldBB) {
    use Direction::*;

    let mut x = 0isize;
    let mut y = 0isize;
    for instr in instrs {
        match instr.p1_value {
            Left(n) | Right(n) => {
                let step;
                let to_x = if let Left(_) = instr.p1_value {
                    let tmp = x.checked_sub_unsigned(n).unwrap();
                    step = -1isize;
                    tmp
                } else {
                    let tmp = x.checked_add_unsigned(n).unwrap();
                    step = 1isize;
                    tmp
                };
                while x != to_x {
                    let (tmp_x, tmp_y) = normalize_coord((x, y), dims);
                    map[tmp_y][tmp_x] = FieldTile::Trench;
                    x = x.checked_add(step).unwrap();
                }
            }
            Up(n) | Down(n) => {
                let step;
                let to_y = if let Up(_) = instr.p1_value {
                    let tmp = y.checked_sub_unsigned(n).unwrap();
                    step = -1isize;
                    tmp
                } else {
                    let tmp = y.checked_add_unsigned(n).unwrap();
                    step = 1isize;
                    tmp
                };
                while y != to_y {
                    let (tmp_x, tmp_y) = normalize_coord((x, y), dims);
                    map[tmp_y][tmp_x] = FieldTile::Trench;
                    y = y.checked_add(step).unwrap();
                }
            }
        };
    }
}

#[rustfmt::skip]
fn normalize_coord(coord: (isize, isize), dims: FieldBB) -> (usize, usize) {
    (
        (coord.0 + dims.0.0.abs()) as usize,
        (coord.1 + dims.1.0.abs()) as usize,
    )
}

fn find_start_point(map: &[Vec<FieldTile>]) -> Option<(usize, usize)> {
    for (y, line) in map.iter().enumerate() {
        let mut x = line.iter().position(|&c| c == FieldTile::Trench).unwrap();
        let last = line.iter().rposition(|&c| c == FieldTile::Trench).unwrap();
        while x < last {
            if line[x] == FieldTile::Terrain {
                return Some((x, y));
            }
            x += 1;
        }
    }
    None
}

const OFFSETS: [(isize, isize); 8] = [
    (-1, -1),
    (0, -1),
    (1, -1),
    (-1, 0),
    (1, 0),
    (-1, 1),
    (0, 1),
    (1, 1),
];

fn flood_fill(map: &mut [Vec<FieldTile>], start: (usize, usize)) {
    let mut queue = VecDeque::new();
    queue.push_back(start);
    while let Some((x, y)) = queue.pop_front() {
        if map[y][x] == FieldTile::Trench {
            continue;
        }

        // Process
        map[y][x] = FieldTile::Trench;

        // Find neighbors
        for offset in OFFSETS {
            let (Some(x), Some(y)) = (
                x.checked_add_signed(offset.0),
                y.checked_add_signed(offset.1),
            ) else {
                continue;
            };
            if x > map.first().unwrap().len() - 1
                || y > map.len() - 1
                || map[y][x] == FieldTile::Trench
            {
                continue;
            }
            queue.push_back((x, y));
        }
    }
}

fn shoelace(vertices: &[(isize, isize)]) -> usize {
    let mut area = vertices
        .array_windows()
        .map(|[(x1, y1), (x2, y2)]| (x1 * y2 - y1 * x2) + (x1 - x2).abs() + (y1 - y2).abs())
        .sum::<isize>();

    let first = vertices.first().unwrap();
    let last = vertices.last().unwrap();
    area += (last.0 * first.1 - last.1 * first.0).abs();

    ((area / 2) + 1/*start pixel*/) as usize
}

pub fn parse(input: &str) -> Vec<Instruction> {
    input
        .lines()
        .map(|line| {
            let [dir, amount, colour] = line.array_split(' ');
            let colour = &colour[2..8];
            let p1_dir = {
                let amount = amount.parse().unwrap();
                match dir {
                    "R" => Right(amount),
                    "L" => Left(amount),
                    "U" => Up(amount),
                    "D" => Down(amount),
                    _ => panic!("Invalid input"),
                }
            };
            let p2_dir = {
                let amount = usize::from_str_radix(&colour[..5], 16).unwrap();
                match &colour[5..6] {
                    "0" => Right(amount),
                    "1" => Down(amount),
                    "2" => Left(amount),
                    "3" => Up(amount),
                    _ => panic!("Invalid input"),
                }
            };
            use Direction::*;
            Instruction {
                p1_value: p1_dir,
                p2_value: p2_dir,
            }
        })
        .collect()
}

pub fn part1(instrs: &[Instruction]) -> u32 {
    let bb @ ((min_x, max_x), (min_y, max_y)) = get_field_size(instrs, None);

    let x_size = (max_x + min_x.abs() + 1) as usize;
    let y_size = (max_y + min_y.abs() + 1) as usize;

    let mut map = vec![vec![FieldTile::Terrain; x_size]; y_size];

    fill_map(instrs, &mut map, bb);

    let start = find_start_point(&map).unwrap();

    flood_fill(&mut map, start);

    map.iter().flatten().map(|x| *x as u32).sum()
}

pub fn part2(instructions: &[Instruction]) -> usize {
    let mut vertices: Vec<(isize, isize)> = Vec::new();

    let _ = get_field_size(instructions, Some(&mut vertices));

    shoelace(&vertices)
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &str = "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)
";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&dbg!(parse(INPUT))), 62);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 952408144115);
    }
}
