use std::ops::Index;

type Forest = Vec2D<u8>;

pub struct Vec2D<T> {
    storage: Vec<T>,
    y_max: usize,
    x_max: usize,
}

impl<T> Index<(usize, usize)> for Vec2D<T> {
    type Output = T;

    fn index(&self, (y, x): (usize, usize)) -> &Self::Output {
        &self.storage[y * self.x_max + x]
    }
}

impl<T> From<Vec<Vec<T>>> for Vec2D<T> {
    fn from(v: Vec<Vec<T>>) -> Self {
        let y_max = v.len();
        let x_max = v[0].len();
        let storage = v.into_iter().flatten().collect();
        Vec2D {
            storage,
            y_max,
            x_max,
        }
    }
}

pub fn parse(inpt: &str) -> Forest {
    inpt.lines()
        .map(|l| l.chars().map(|x| x.to_digit(10).unwrap() as u8).collect())
        .collect::<Vec<_>>()
        .into()
}

fn is_visible(x: usize, y: usize, forest: &Forest) -> bool {
    let height = forest[(y, x)];

    (0..x).rev().all(|i| forest[(y, i)] < height)
        || (x + 1..forest.x_max).all(|i| forest[(y, i)] < height)
        || (0..y).rev().all(|i| forest[(i, x)] < height)
        || (y + 1..forest.y_max).all(|i| forest[(i, x)] < height)
}

#[allow(clippy::needless_range_loop)]
fn scenic_score(x: usize, y: usize, forest: &Forest) -> usize {
    let height = forest[(y, x)];

    // Iterator::take_until 🥺

    let left = {
        let mut count = 0;
        for i in (0..x).rev() {
            count += 1;
            if forest[(y, i)] >= height {
                break;
            }
        }
        count
    };

    let right = {
        let mut count = 0;
        for i in x + 1..forest.x_max {
            count += 1;
            if forest[(y, i)] >= height {
                break;
            }
        }
        count
    };

    let up = {
        let mut count = 0;
        for i in (0..y).rev() {
            count += 1;
            if forest[(i, x)] >= height {
                break;
            }
        }
        count
    };

    let down = {
        let mut count = 0;
        for i in y + 1..forest.y_max {
            count += 1;
            if forest[(i, x)] >= height {
                break;
            }
        }
        count
    };

    left * right * up * down
}

pub fn part1(forest: &Forest) -> usize {
    (0..forest.y_max)
        .map(|j| {
            (0..forest.x_max)
                .filter(|i| is_visible(*i, j, forest))
                .count()
        })
        .sum()
}

pub fn part2(forest: &Forest) -> usize {
    (0..forest.y_max)
        .flat_map(|j| (0..forest.x_max).map(move |i| scenic_score(i, j, forest)))
        .max()
        .unwrap()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    const INPUT: &str = "30373
25512
65332
33549
35390";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 21);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 8);
    }
}
