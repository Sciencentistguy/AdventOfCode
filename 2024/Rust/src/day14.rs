use common::ArraySplit;
use fxhash::FxHashSet as HashSet;
use nalgebra::Vector2;

#[derive(Debug, Clone)]
pub struct Robot {
    position: Vector2<i64>,
    velocity: Vector2<i64>,
}

type Parsed = (Vec<Robot>, i64, i64);
type Solution = usize;

pub fn parse(input: &str) -> Parsed {
    let robots: Vec<_> = input
        .lines()
        .map(|line| {
            let (position, velocity) = line.split_once(' ').unwrap();
            let [px, py] = position[2..].array_split(',').map(|x| x.parse().unwrap());
            let [vx, vy] = velocity[2..].array_split(',').map(|x| x.parse().unwrap());
            Robot {
                position: Vector2::new(px, py),
                velocity: Vector2::new(vx, vy),
            }
        })
        .collect();
    let max_x = robots.iter().map(|r| r.position.x).max().unwrap();
    let max_y = robots.iter().map(|r| r.position.y).max().unwrap();
    (robots, max_x, max_y)
}

pub fn part1((robots, max_x, max_y): &Parsed) -> Solution {
    const NUM_ITERATIONS: i64 = 100;

    let mut top_left = 0;
    let mut top_right = 0;
    let mut bottom_left = 0;
    let mut bottom_right = 0;

    let mid_x = max_x / 2;
    let mid_y = max_y / 2;

    for robot in robots {
        let x = (robot.position.x + robot.velocity.x * NUM_ITERATIONS).rem_euclid(max_x+1);
        let y = (robot.position.y + robot.velocity.y * NUM_ITERATIONS).rem_euclid(max_y+1);

        if x < mid_x && y < mid_y {
            top_left += 1;
        } else if x > mid_x && y < mid_y {
            top_right += 1;
        } else if x < mid_x && y > mid_y {
            bottom_left += 1;
        } else if x > mid_x && y > mid_y {
            bottom_right += 1;
        }
    }

    top_left * top_right * bottom_left * bottom_right
}

fn is_block(robots: &HashSet<Vector2<i64>>, x: i64, y: i64) -> bool {
    (0..3).all(|i| (0..3).all(|j| robots.contains(&Vector2::new(x + i, y + j))))
}

pub fn part2((robots, max_x, max_y): &Parsed) -> Solution {
    let mut robots = robots.to_owned();
    let mut positions = HashSet::with_capacity_and_hasher(robots.len(), Default::default());
    for i in 0.. {
        for robot in &mut robots {
            robot.position += robot.velocity;

            robot.position.x = robot.position.x.rem_euclid(max_x + 1);
            robot.position.y = robot.position.y.rem_euclid(max_y + 1);
        }

        // if we have 9 robots in a 3x3 grid, can we assume we have found the easter egg?
        // ¯\_(ツ)_/¯  works on my m̶a̶c̶h̶i̶n̶e̶ input
        positions.clear();
        positions.extend(robots.iter().map(|r| r.position));
        if positions
            .iter()
            .any(|&pos| is_block(&positions, pos.x - 1, pos.y - 1))
        {
            return i + 1;
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

    const INPUT: &str = "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3
";

    const P1_SOLUTION: Solution = 12;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
}
