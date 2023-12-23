use bitvec::prelude::*;

pub struct Map<'a> {
    data: &'a [u8],
    w: usize,
    h: usize,
}

impl Map<'_> {
    // Find the '.' on the top row
    pub fn start(&self) -> (usize, usize) {
        (
            self.data
                .iter()
                .take(self.w)
                .position(|&b| b == b'.')
                .unwrap(),
            0,
        )
    }

    // Find the '.' on the bottom row
    pub fn target(&self) -> (usize, usize) {
        (
            self.data
                .iter()
                .skip((self.h - 1) * (self.w + 1))
                .position(|&b| b == b'.')
                .unwrap(),
            self.h - 1,
        )
    }

    // returns true for a given point if that point has more than 2 possible locations to move to
    fn is_branch_point(&self, (col, row): (usize, usize)) -> bool {
        self.data[col + row * (self.w + 1)] != b'#'
            && [(1, 0), (0, -1), (-1, 0), (0, 1)]
                .into_iter()
                .map(|(dc, dr)| (col.wrapping_add_signed(dc), row.wrapping_add_signed(dr)))
                .filter(|&(col, row)| {
                    col < self.w && row < self.h && self.data[col + row * (self.w + 1)] != b'#'
                })
                .count()
                > 2
    }

    // Returns a Vec of all branch points
    pub fn branch_points(&self) -> Vec<(usize, usize)> {
        (0..self.w * self.h)
            .map(|pos| (pos % self.w, pos / self.w))
            .filter(|&coord| self.is_branch_point(coord))
            .collect::<Vec<_>>()
    }

    const DIRECTIONS_WITH_SLOPES: [(isize, isize, u8); 4] =
        [(1, 0, b'>'), (0, -1, b'^'), (-1, 0, b'<'), (0, 1, b'v')];

    /// Retunrs an iterator over all of the valid points to move to, taking into account slopes
    pub fn adj_iter(
        &self,
        (col, row): (usize, usize),
        ignore_slopes: bool,
    ) -> impl Iterator<Item = (usize, usize)> + '_ {
        Self::DIRECTIONS_WITH_SLOPES
            .iter()
            .map(move |&(dc, dr, ok)| {
                (col.wrapping_add_signed(dc), row.wrapping_add_signed(dr), ok)
            })
            .filter(move |&(col, row, ok)| {
                // We're inside the bounds
                col < self.w
                    && row < self.h
                    && (
                        // and either
                        // data[x,y] is either a '.' or the right kind of slope
                        [b'.', ok].contains(&self.data[col + row * (self.w + 1)])
                        // or we're ignoring slopes, and its not a '#'
                        || ignore_slopes && self.data[col + row * (self.w + 1)] != b'#'
                    )
            })
            .map(|(col, row, _)| (col, row))
    }
}

pub fn parse(input: &str) -> Map {
    let data = input.as_bytes();
    let w = data.iter().position(|&b| b == b'\n').unwrap_or(data.len());
    let h = (data.len() + 1) / (w + 1);
    Map { data, w, h }
}

pub fn solve(grid: &Map, ignore_slopes: bool) -> usize {
    // nodes are branch points and start / target
    let mut nodes = grid.branch_points();
    nodes.push(grid.start());
    nodes.push(grid.target());

    // calculate length of (unique) paths between all nodes
    let mut adjacents = vec![Vec::new(); nodes.len()];
    for (k0, &start) in nodes.iter().enumerate() {
        let mut queue = vec![(0, start, None)];
        while let Some((steps, cur, prev)) = queue.pop() {
            if cur != start && let Some(k1) = nodes.iter().position(|&coord| coord == cur)
            {
                adjacents[k0].push((k1, steps));
                continue;
            }

            queue.extend(
                grid.adj_iter(cur, ignore_slopes)
                    .filter(|&adj| Some(adj) != prev) // don't go backwards
                    .map(|adj| (steps + 1, adj, Some(cur))),
            );
        }
    }

    // seen information is stored in bits of u64
    assert!(nodes.len() <= 64);


    // find maximum
    let start = nodes.len() - 2;
    let target = nodes.len() - 1;
    let x = bitarr![u64, Lsb0; 0; 1];
    x.set(start, true);
    let mut queue = Vec::from([(0, start, x)]);
    let mut max = 0;
    while let Some((cost, idx, seen)) = queue.pop() {
        if idx == target && cost > max {
            max = cost;
            continue;
        }

        for &(adj, weight) in &adjacents[idx] {
            if seen & (1 << adj) == 0 {
                queue.push((cost + weight, adj, seen | 1 << adj));
            }
        }
    }
    max
}

pub fn part1(map: &Map) -> usize {
    solve(map, false)
}

pub fn part2(map: &Map) -> usize {
    solve(map, true)
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &str = "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#
";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 405);
    }
    #[test]
    fn test_part2() {
        // assert_eq!(part2(&parse(INPUT)), 2);
    }
}
