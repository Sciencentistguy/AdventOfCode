use itertools::Itertools;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Point2D {
    x: i32,
    y: i32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Report {
    sensor: Point2D,
    closest_beacon: Point2D,
    distance: i32,
}

impl Report {
    // Calculate the line segment (if any) where this report's area intersects the line y
    fn intersect(&self, y: i32) -> Option<Segment> {
        if (self.sensor.y - y).abs() > self.distance {
            return None;
        }

        let displacement = self.distance - (self.sensor.y - y).abs();

        Some(Segment {
            start: Point2D {
                x: self.sensor.x - displacement,
                y,
            },
            end: Point2D {
                x: self.sensor.x + displacement,
                y,
            },
        })
    }

    /// Trace the circumference of the area of this sensor
    pub fn circumference(&self) -> Vec<Point2D> {
        let mut result = Vec::with_capacity(7000000usize.next_power_of_two());
        for y in (self.sensor.y - self.distance - 1)..=(self.sensor.y + self.distance + 1) {
            let displacement = self.distance - (self.sensor.y - y).abs() + 1;
            // left side
            result.push(Point2D {
                x: self.sensor.x - displacement,
                y,
            });
            // if its to the right, add that as well
            if displacement > 0 {
                result.push(Point2D {
                    x: self.sensor.x + displacement,
                    y,
                })
            }
        }
        result
    }

    /// Is `point` within the area of this sensor
    pub fn covers(&self, point: &Point2D) -> bool {
        (point.x - self.sensor.x).abs() + (point.y - self.sensor.y).abs() <= self.distance
    }
}

// A horizontal line segment
#[derive(Clone, Copy)]
pub struct Segment {
    start: Point2D,
    end: Point2D,
}

impl Segment {
    #[allow(clippy::len_without_is_empty)] // hush
    pub fn len(&self) -> i32 {
        self.end.x - self.start.x + 1
    }

    pub fn subtract(self, other: &Segment) -> (Option<Segment>, Option<Segment>) {
        (
            Segment {
                start: Point2D {
                    x: self.start.x,
                    y: self.start.y,
                },
                end: Point2D {
                    x: other.start.x - 1,
                    y: self.start.y,
                },
            }
            .validate(),
            Segment {
                start: Point2D {
                    x: other.end.x + 1,
                    y: self.start.y,
                },
                end: Point2D {
                    x: self.end.x,
                    y: self.start.y,
                },
            }
            .validate(),
        )
    }

    // Line segments are only valid if they have a positive length
    pub fn validate(self) -> Option<Segment> {
        if self.start.x <= self.end.x && self.start.y <= self.end.y {
            Some(self)
        } else {
            None
        }
    }

    pub fn overlaps(self, other: Segment) -> bool {
        !(self.start.x > other.end.x || self.end.x < other.start.x)
    }
}

// A trait so the in-line call looks nicer
pub trait InsertNonOverlapping {
    fn insert_non_overlapping(self, egment: Segment) -> Self;
}

impl InsertNonOverlapping for Vec<Segment> {
    fn insert_non_overlapping(mut self, segment: Segment) -> Vec<Segment> {
        let mut stack: Vec<Segment> = vec![segment];

        while !stack.is_empty() {
            let current = stack.pop().unwrap();
            let other = self.iter().find(|other| other.overlaps(current));
            match other {
                None => {
                    self.push(current);
                }
                Some(other) => {
                    let (a, b) = current.subtract(other);

                    if let Some(a) = a {
                        stack.push(a)
                    }
                    if let Some(b) = b {
                        stack.push(b)
                    }
                }
            }
        }
        self
    }
}

const fn manhattan_distance(
    Point2D { x: x1, y: y1 }: Point2D,
    Point2D { x: x2, y: y2 }: Point2D,
) -> i32 {
    (x1 - x2).abs() + (y1 - y2).abs()
}

pub fn parse(inpt: &str) -> Vec<Report> {
    assert!(inpt.is_ascii());
    inpt.lines()
        .map(|line| {
            let first_num_start = 12;
            let first_num_end =
                first_num_start + memchr::memchr(b',', line[first_num_start..].as_bytes()).unwrap();
            let first_num = line[first_num_start..first_num_end].parse().unwrap();

            let second_num_start = memchr::memchr(b'y', line.as_bytes()).unwrap() + 2;
            let second_num_end = second_num_start
                + memchr::memchr(b':', line[second_num_start..].as_bytes()).unwrap();
            let second_num = line[second_num_start..second_num_end].parse().unwrap();

            let third_num_start = memchr::memrchr(b'x', line.as_bytes()).unwrap() + 2;
            let third_num_end =
                third_num_start + memchr::memchr(b',', line[third_num_start..].as_bytes()).unwrap();
            let third_num = line[third_num_start..third_num_end].parse().unwrap();

            let fourth_num_start = memchr::memrchr(b'=', line.as_bytes()).unwrap() + 1;
            let fourth_num = line[fourth_num_start..].parse().unwrap();

            let sensor = Point2D {
                x: first_num,
                y: second_num,
            };
            let closest_beacon = Point2D {
                x: third_num,
                y: fourth_num,
            };

            Report {
                sensor,
                closest_beacon,
                distance: manhattan_distance(sensor, closest_beacon),
            }
        })
        .collect()
}

pub fn part1(input: &[Report], row: i32) -> i32 {
    let segments = input
        .iter()
        .flat_map(|sensor| sensor.intersect(row))
        .fold(Vec::<Segment>::new(), |segments, segment| {
            segments.insert_non_overlapping(segment)
        });

    segments.iter().map(|segment| segment.len()).sum::<i32>()
        - input
            .iter()
            .map(|sensor| sensor.closest_beacon)
            .filter(|beacon| {
                segments.iter().any(|segment| {
                    segment.start.y == beacon.y
                        && segment.start.x <= beacon.x
                        && segment.end.x >= beacon.x
                })
            })
            .unique()
            .count() as i32
}

pub fn part2(input: &[Report], max: i32) -> i64 {
    let result = input
        .iter()
        .flat_map(|sensor| sensor.circumference())
        .filter(|point| point.x >= 0 && point.x <= max && point.y >= 0 && point.y <= max)
        .find(|point| !input.iter().any(|sensor| sensor.covers(point)))
        .unwrap();

    result.x as i64 * 4_000_000 + (result.y as i64)
}

pub fn run(input: &str) {
    const P1_ROW: i32 = 2_000_000;
    const P2_MAX: i32 = 4_000_000;

    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed, P1_ROW));
    println!("Part 2: {}", part2(&parsed, P2_MAX));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT), 10), 26);
    }

    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT), 20), 56000011);
    }
}
