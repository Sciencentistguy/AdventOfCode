#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Tile {
    Round,
    Square,
    Empty,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Telescope(Vec<Vec<Tile>>);

impl Telescope {
    fn slide_north(&mut self) {
        for col in 0..self.0[0].len() {
            let mut empty_or_round_row = 0;
            for row in 0..self.0.len() {
                let curr = self.0[row][col];
                match curr {
                    Tile::Square => empty_or_round_row = row + 1,
                    Tile::Round => {
                        // swap the current tile with the empty_or_round one
                        let replace_with =
                            std::mem::replace(&mut self.0[empty_or_round_row][col], curr);
                        self.0[row][col] = replace_with;
                        empty_or_round_row += 1;
                    }
                    Tile::Empty => (),
                }
            }
        }
    }

    fn weight(&self) -> usize {
        self.0
            .iter()
            .rev()
            .enumerate()
            .map(|(i, row)| {
                let round_rocks = row.iter().filter(|&tile| *tile == Tile::Round).count();
                round_rocks * (i + 1)
            })
            .sum()
    }

    #[allow(clippy::needless_range_loop)]
    fn clockwise(&self) -> Self {
        let size = self.0.len();
        let mut rotated = vec![vec![Tile::Empty; size]; size];
        for row in 0..size {
            for col in 0..size {
                rotated[col][size - 1 - row] = self.0[row][col];
            }
        }
        Self(rotated)
    }

    fn cycle(mut self) -> Self {
        for _ in 0..4 {
            self.slide_north();
            self = self.clockwise();
        }
        self
    }
}

pub fn parse(input: &str) -> Telescope {
    Telescope(
        input
            .lines()
            .map(|line| {
                line.chars()
                    .map(|c| match c {
                        '.' => Tile::Empty,
                        '#' => Tile::Square,
                        'O' => Tile::Round,
                        _ => unreachable!(),
                    })
                    .collect()
            })
            .collect(),
    )
}

pub fn part1(telescope: &Telescope) -> usize {
    let mut telescope = telescope.to_owned();
    telescope.slide_north();
    telescope.weight()
}

pub fn part2(telescope: &Telescope) -> usize {
    let mut telescope = telescope.to_owned();
    let mut seen = vec![telescope.clone()];

    loop {
        telescope = telescope.cycle();
        // if the same layout has been seen before, we are done.
        if let Some(idx) = seen.iter().position(|x| x == &telescope) {
            let cycle_length = seen.len() - idx;
            // calculate the index modulo cycle_length, to avoid dying of Big Number
            let final_idx = idx + (1_000_000_000 - idx) % cycle_length;

            break seen[final_idx].weight();
        }
        seen.push(telescope.clone());
    }
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &str = "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 136);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 64);
    }
}
