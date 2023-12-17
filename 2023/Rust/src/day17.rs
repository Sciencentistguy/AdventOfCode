use pathfinding::directed::dijkstra::dijkstra;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
enum Direction {
    North,
    South,
    East,
    West,
}

impl Direction {
    // Return the opposite direction.
    fn opposite(&self) -> Self {
        match self {
            Self::North => Self::South,
            Self::South => Self::North,
            Self::East => Self::West,
            Self::West => Self::East,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Point {
    x: usize,
    y: usize,
}

impl Point {
    fn new(x: usize, y: usize) -> Self {
        Self { x, y }
    }

    // Return the valid next points from this point. It won't include
    // any that are out of bounds.
    fn valid_next(&self, grid: &[Vec<usize>]) -> Vec<(Direction, Point)> {
        let mut next = Vec::new();
        if self.x > 0 {
            next.push((Direction::West, Self::new(self.x - 1, self.y)));
        }
        if self.y > 0 {
            next.push((Direction::North, Self::new(self.x, self.y - 1)));
        }
        if self.x < grid[0].len() - 1 {
            next.push((Direction::East, Self::new(self.x + 1, self.y)));
        }
        if self.y < grid.len() - 1 {
            next.push((Direction::South, Self::new(self.x, self.y + 1)));
        }
        next
    }
}

// We'll use this to track our current position and direction and how
// many times we've gone that direction.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
struct Node {
    position: Point,
    direction: Direction,
    direction_count: usize,
}

impl Node {
    fn new(position: Point, direction: Direction, direction_count: usize) -> Self {
        Self {
            position,
            direction,
            direction_count,
        }
    }
}

fn neighbors_p1(node: &Node, grid: &[Vec<usize>]) -> Vec<Node> {
    let mut neighbors = Vec::new();
    // Get the possible next valid points.
    for (d, p) in node.position.valid_next(grid) {
        if d == node.direction.opposite() {
            // We can't go in the opposite direction.
            continue;
        } else if d != node.direction {
            // If we are going in a new direction, then we can use it,
            // just reset the count.
            neighbors.push(Node::new(p, d, 1));
        } else if node.direction_count < 3 {
            // If we have been going in this direction for less than
            // 3, we can keep going.
            neighbors.push(Node::new(p, d, node.direction_count + 1));
        }

        // All other cases are invalid for p1.
    }
    neighbors
}

fn neighbors_p2(node: &Node, grid: &[Vec<usize>]) -> Vec<Node> {
    let mut neighbors = Vec::new();
    for (d, p) in node.position.valid_next(grid) {
        if d == node.direction.opposite() {
            // We can't go in the opposite direction.
            continue;
        } else if d != node.direction && node.direction_count >= 4 {
            // We can only change direction if we've already gone in
            // this direction 4 times or more.
            neighbors.push(Node::new(p, d, 1));
        } else if d == node.direction && node.direction_count < 10 {
            // We can only go in the same direction if we haven't gone
            // more than 10 times in this durection..
            neighbors.push(Node::new(p, d, node.direction_count + 1));
        }

        // All other cases are invalid for p2.
    }
    neighbors
}

pub fn parse(input: &str) -> Vec<Vec<usize>> {
    input
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| {
            line.chars()
                .map(|c| c.to_digit(10).unwrap() as usize)
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>()
}

pub fn part1(grid: &[Vec<usize>]) -> usize {
    // Part 1
    let start = Point::new(0, 0);
    let goal = Point::new(grid[0].len() - 1, grid.len() - 1);

    let n = Node::new(start.clone(), Direction::South, 0);
    dijkstra(
        &n,
        |n| {
            neighbors_p1(n, grid)
                .into_iter()
                .map(|n| (n.clone(), grid[n.position.y][n.position.x]))
        },
        |n| n.position == goal,
    )
    .unwrap()
    .1
}

pub fn part2(grid: &[Vec<usize>]) -> usize {
    let start = Point::new(0, 0);
    let goal = Point::new(grid[0].len() - 1, grid.len() - 1);

    let n_e = Node::new(start.clone(), Direction::East, 0);
    let n_s = Node::new(start.clone(), Direction::South, 0);
    let djik = |starting_node| {
        dijkstra(
            &starting_node,
            |n| {
                neighbors_p2(n, grid)
                    .into_iter()
                    .map(|n| (n.clone(), grid[n.position.y][n.position.x]))
            },
            |n| n.position == goal && n.direction_count >= 4,
        )
        .unwrap()
        .1
    };
    let s = djik(n_s);
    let e = djik(n_e);
    e.min(s)
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &str = "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&dbg!(parse(INPUT))), 102);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 94);
        let input_2 = "111111111111
999999999991
999999999991
999999999991
999999999991";
        assert_eq!(part2(&parse(input_2)), 71);
    }
}
