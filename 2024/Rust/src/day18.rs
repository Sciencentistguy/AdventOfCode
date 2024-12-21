#![allow(clippy::deref_addrof)]

use std::collections::VecDeque;
use std::hash::BuildHasherDefault;

use common::Vec2D;
use compact_str::CompactString;
use compact_str::format_compact;
use fxhash::FxHashMap as HashMap;
use fxhash::FxHashSet as HashSet;

type Parsed = Vec<Vec2D<usize>>;

pub fn parse(input: &str) -> Parsed {
    input
        .lines()
        .map(|line| {
            line.split_once(',')
                .map(|(x, y)| Vec2D {
                    x: x.parse().unwrap(),
                    y: y.parse().unwrap(),
                })
                .unwrap()
        })
        .collect()
}

#[cfg(not(test))]
const GRID_SIZE: usize = 70;
#[cfg(test)]
const GRID_SIZE: usize = 6;

fn djikstra(invalid: &[Vec2D<usize>]) -> Option<u64> {
    const START: Vec2D<usize> = Vec2D { x: 0, y: 0 };
    const DEST: Vec2D<usize> = Vec2D {
        x: GRID_SIZE,
        y: GRID_SIZE,
    };

    #[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
    struct State {
        cost: u64,
        node: Vec2D<usize>,
    }

    impl Ord for State {
        fn cmp(&self, other: &Self) -> std::cmp::Ordering {
            other.cost.cmp(&self.cost)
        }
    }
    impl PartialOrd for State {
        fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
            Some(self.cmp(other))
        }
    }

    const DIRECTIONS: [Vec2D<isize>; 4] = [
        Vec2D { x: 0, y: 1 },
        Vec2D { x: 0, y: -1 },
        Vec2D { x: 1, y: 0 },
        Vec2D { x: -1, y: 0 },
    ];

    // Safety: Only one &mut is created per run of this function
    let invalid = {
        #[thread_local]
        static mut INVALID: HashSet<Vec2D<usize>> = HashSet::with_hasher(BuildHasherDefault::new());
        let invalid2 = unsafe { &mut *&raw mut INVALID };
        invalid2.clear();
        invalid2.extend(invalid.iter().copied());
        invalid2
    };
    let visited = {
        #[thread_local]
        static mut VISITED: HashSet<Vec2D<usize>> = HashSet::with_hasher(BuildHasherDefault::new());
        let visited = unsafe { &mut *&raw mut VISITED };
        visited.clear();
        visited
    };
    let distances = {
        #[thread_local]
        static mut DISTANCES: HashMap<Vec2D<usize>, u64> =
            HashMap::with_hasher(BuildHasherDefault::new());
        let distances = unsafe { &mut *&raw mut DISTANCES };
        distances.clear();
        distances
    };
    let queue = {
        #[thread_local]
        static mut QUEUE: VecDeque<State> = VecDeque::new();
        let queue = unsafe { &mut *&raw mut QUEUE };
        queue.clear();
        queue
    };

    distances.insert(START, 0);
    queue.push_back(State {
        cost: 0,
        node: START,
    });

    while let Some(State { cost, node }) = queue.pop_front() {
        if visited.contains(&node) {
            continue;
        }

        visited.insert(node);

        for direction in DIRECTIONS {
            if let Some(new) = node.checked_add_signed(direction)
                && new.y <= GRID_SIZE
                && new.x <= GRID_SIZE
                && !invalid.contains(&new)
            {
                let new_distance = cost + 1;

                if distances
                    .get(&new)
                    .map(|&x| new_distance < x)
                    .unwrap_or(true)
                {
                    distances.insert(new, new_distance);
                    queue.push_back(State {
                        cost: new_distance,
                        node: new,
                    });
                }
            }
        }
    }

    distances.get(&DEST).copied()
}

pub fn part1(parsed: &Parsed) -> u64 {
    #[cfg(not(test))]
    const NUM_BYTES_TO_SIMULATE: usize = 1024;
    #[cfg(test)]
    const NUM_BYTES_TO_SIMULATE: usize = 12;

    djikstra(&parsed[..NUM_BYTES_TO_SIMULATE]).unwrap()
}

pub fn part2(parsed: &Parsed) -> CompactString {
    // binary search the number of bytes to simulate
    let mut low = 0;
    let mut high = parsed.len();
    while low < high {
        let mid = (low + high) / 2;
        if djikstra(&parsed[..mid]).is_none() {
            high = mid;
        } else {
            low = mid + 1;
        }
    }

    format_compact!("{},{}", parsed[low - 1].x, parsed[low - 1].y)
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0
";

    const P1_SOLUTION: u64 = 22;
    const P2_SOLUTION: &str = "6,1";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
