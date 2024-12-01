use std::{
    collections::HashMap,
    time::{Duration, Instant},
};

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Point3D {
    x: i64,
    y: i64,
    z: i64,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Point4D {
    x: i64,
    y: i64,
    z: i64,
    w: i64,
}

impl Point3D {
    fn zero() -> Self {
        Self { x: 0, y: 0, z: 0 }
    }
}

impl Point4D {
    fn zero() -> Self {
        Self {
            x: 0,
            y: 0,
            z: 0,
            w: 0,
        }
    }
}

type ConwaySpace3D = HashMap<Point3D, bool>;
type ConwaySpace4D = HashMap<Point4D, bool>;

fn parse_input(input: &str) -> ((ConwaySpace3D, ConwaySpace4D), Duration) {
    let start = Instant::now();

    let mut space_3d: ConwaySpace3D = HashMap::new();

    let mut max_xy3 = Point3D::zero();
    for line in input.lines() {
        max_xy3.x = 0;
        for c in line.bytes() {
            space_3d.insert(
                max_xy3.clone(),
                match c {
                    b'#' => true,
                    b'.' => false,
                    _ => unreachable!("invalid input"),
                },
            );
            max_xy3.x += 1;
        }
        max_xy3.y += 1;
    }

    expand_space_3d(&mut space_3d, max_xy3);

    let mut space_4d: ConwaySpace4D = HashMap::new();

    let mut max_xy4 = Point4D::zero();
    for line in input.lines() {
        max_xy4.x = 0;
        for c in line.bytes() {
            space_4d.insert(
                max_xy4.clone(),
                match c {
                    b'#' => true,
                    b'.' => false,
                    _ => unreachable!("invalid input"),
                },
            );
            max_xy4.x += 1;
        }
        max_xy4.y += 1;
    }
    expand_space_4d(&mut space_4d, max_xy4);

    let end = Instant::now();
    ((space_3d, space_4d), end - start)
}

fn expand_space_3d(space: &mut ConwaySpace3D, max_xy: Point3D) {
    for z in -1..=1 {
        for y in -1..=max_xy.y {
            for x in -1..=max_xy.x {
                let pos = Point3D { x, y, z };
                space.entry(pos).or_insert(false);
            }
        }
    }
}

fn expand_space_4d(space: &mut ConwaySpace4D, max_xy: Point4D) {
    for z in -1..=1 {
        for y in -1..=max_xy.y {
            for x in -1..=max_xy.x {
                for w in -1..=1 {
                    let pos = Point4D { x, y, z, w };
                    space.entry(pos).or_insert(false);
                }
            }
        }
    }
}

fn get_active_adjacents_3d(space: &mut ConwaySpace3D, centre: &Point3D) -> usize {
    let mut count = 0;

    for z in centre.z - 1..=centre.z + 1 {
        for y in centre.y - 1..=centre.y + 1 {
            for x in centre.x - 1..=centre.x + 1 {
                let pos = Point3D { x, y, z };
                if centre != &pos {
                    let v = space.entry(pos).or_insert(false);
                    if *v {
                        count += 1;
                    }
                }
            }
        }
    }
    count
}

fn get_active_adjacents_4d(space: &mut ConwaySpace4D, centre: &Point4D) -> usize {
    let mut count = 0;

    for z in centre.z - 1..=centre.z + 1 {
        for y in centre.y - 1..=centre.y + 1 {
            for x in centre.x - 1..=centre.x + 1 {
                for w in centre.w - 1..=centre.w + 1 {
                    let pos = Point4D { x, y, z, w };
                    if centre != &pos {
                        let v = space.entry(pos).or_insert(false);
                        if *v {
                            count += 1;
                        }
                    }
                }
            }
        }
    }
    count
}

fn do_conway_3d(space: &mut ConwaySpace3D) {
    let mut working_copy = space.clone();
    for (location, state) in space.iter_mut() {
        let adjacents = get_active_adjacents_3d(&mut working_copy, location);
        if *state && adjacents != 2 && adjacents != 3 {
            *state = false;
        } else if adjacents == 3 {
            *state = true;
        }
    }

    // TODO how about don't copy all this data all the time
    working_copy.extend(space.iter().map(|(k, v)| (k.clone(), *v)));
    std::mem::swap(space, &mut working_copy);
}

fn do_conway_4d(space: &mut ConwaySpace4D) {
    let mut working_copy = space.clone();
    for (location, state) in space.iter_mut() {
        let adjacents = get_active_adjacents_4d(&mut working_copy, location);
        if *state && adjacents != 2 && adjacents != 3 {
            *state = false;
        } else if adjacents == 3 {
            *state = true;
        }
    }

    // TODO how about don't copy all this data all the time
    working_copy.extend(space.iter().map(|(k, v)| (k.clone(), *v)));
    std::mem::swap(space, &mut working_copy);
}

fn solve_part1(mut space: ConwaySpace3D) -> (usize, Duration) {
    let start = Instant::now();

    for _ in 0..6 {
        do_conway_3d(&mut space);
    }

    let res = space.values().filter(|x| **x).count();

    let end = Instant::now();
    (res, end - start)
}

fn solve_part2(mut space: ConwaySpace4D) -> (usize, Duration) {
    let start = Instant::now();
    for _ in 0..6 {
        do_conway_4d(&mut space);
    }

    let res = space.values().filter(|x| **x).count();
    let end = Instant::now();
    (res, end - start)
}

pub fn run(input: String) {
    let ((space_3d, space_4d), time) = parse_input(&input);
    println!("Day 17, parsing input took {}ns", time.as_nanos());
    let (p1, time) = solve_part1(space_3d);
    println!("Day 17, part 1: {}. Took {}ns", p1, time.as_nanos());
    let (p2, time) = solve_part2(space_4d);
    println!("Day 17, part 2: {}. Took {}ns", p2, time.as_nanos());
}
