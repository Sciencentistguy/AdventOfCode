use itertools::Itertools;
use rustc_hash::FxHashSet as HashSet;
use std::collections::VecDeque;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Point3D {
    x: i32,
    y: i32,
    z: i32,
}

impl Point3D {
    const fn new(x: i32, y: i32, z: i32) -> Self {
        Self { x, y, z }
    }
    const fn neighbours(&self) -> [Self; 6] {
        [
            Self::new(self.x - 1, self.y, self.z),
            Self::new(self.x + 1, self.y, self.z),
            Self::new(self.x, self.y + 1, self.z),
            Self::new(self.x, self.y - 1, self.z),
            Self::new(self.x, self.y, self.z + 1),
            Self::new(self.x, self.y, self.z - 1),
        ]
    }
}

fn flood_fill(
    s: Point3D,
    map: &HashSet<Point3D>,
    visited: &mut HashSet<Point3D>,
    end: Point3D,
) -> usize {
    let mut queue = VecDeque::new();
    let mut sum = 0;

    queue.push_back(s.clone());

    while !queue.is_empty() {
        let next = queue.pop_front().unwrap();
        if !(visited.contains(&next)
            || next.x > end.x
            || next.x < s.x
            || next.y > end.y
            || next.y < s.y
            || next.z > end.z
            || next.z < s.z)
        {
            visited.insert(next.clone());
            for n in next.neighbours() {
                if map.contains(&n) {
                    sum += 1;
                } else {
                    queue.push_back(n);
                }
            }
        }
    }
    sum
}

pub fn parse(inpt: &str) -> HashSet<Point3D> {
    assert!(inpt.is_ascii());

    inpt.lines()
        .map(|line| {
            let (first_comma, second_comma) = memchr::memchr_iter(b',', line.as_bytes())
                .collect_tuple()
                .unwrap();
            let x = line[..first_comma].parse().unwrap();
            let y = line[first_comma + 1..second_comma].parse().unwrap();
            let z = line[second_comma + 1..].parse().unwrap();

            Point3D { x, y, z }
        })
        .collect()
}

pub fn part1(cubes: &HashSet<Point3D>) -> usize {
    cubes
        .iter()
        .flat_map(|cube| cube.neighbours())
        .filter(|nb| !cubes.contains(nb))
        .count()
}

pub fn part2(cubes: &HashSet<Point3D>) -> usize {
    let (mut max_x, mut max_y, mut max_z) = (0, 0, 0);
    for cube in cubes.iter() {
        max_x = max_x.max(cube.x);
        max_y = max_y.max(cube.y);
        max_z = max_z.max(cube.z);
    }
    let end = Point3D::new(max_x + 1, max_y + 1, max_z + 1);

    flood_fill(
        Point3D::new(-1, -1, -1),
        cubes,
        &mut HashSet::default(),
        end,
    )
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 64);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 58);
    }
}
