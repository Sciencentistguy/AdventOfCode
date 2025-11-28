use std::cmp::Ordering;

pub struct Coordinates(Vec<(usize, isize)>);

impl ToOwned for Coordinates {
    type Owned = Self;

    fn to_owned(&self) -> Self::Owned {
        Coordinates(self.0.clone())
    }
}

impl Coordinates {
    fn mix(&mut self) {
        for i in 0..self.0.len() {
            let (current_idx, &val) = self
                .0
                .iter()
                .enumerate()
                .find(|(_, (j, _))| j == &i)
                .unwrap();
            let next_idx =
                (current_idx as isize + val.1).rem_euclid(self.0.len() as isize - 1) as usize;
            match current_idx.cmp(&next_idx) {
                Ordering::Less => self
                    .0
                    .copy_within(current_idx + 1..next_idx + 1, current_idx),
                Ordering::Equal => {}
                Ordering::Greater => self.0.copy_within(next_idx..current_idx, next_idx + 1),
            }
            self.0[next_idx] = val;
        }
    }

    fn grove_coordinates(&self) -> isize {
        let (zero_idx, _) = self
            .0
            .iter()
            .enumerate()
            .find(|(_, (_, x))| *x == 0)
            .unwrap();

        [1000, 2000, 3000]
            .iter()
            .map(|x| self.0[(zero_idx + x) % self.0.len()].1)
            .sum()
    }
}

pub fn parse(inpt: &str) -> Coordinates {
    Coordinates(
        inpt.lines()
            .map(|line| line.parse().unwrap())
            .enumerate()
            .collect(),
    )
}

pub fn part1(parsed: &Coordinates) -> isize {
    let mut coordinates = parsed.to_owned();
    coordinates.mix();
    coordinates.grove_coordinates()
}

pub fn part2(parsed: &Coordinates) -> isize {
    const DECRYPTION_KEY: isize = 811589153;

    let mut coordinates = parsed.to_owned();

    for entry in &mut coordinates.0 {
        entry.1 *= DECRYPTION_KEY;
    }

    for _ in 0..10 {
        coordinates.mix();
    }

    coordinates.grove_coordinates()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "1
2
-3
3
-2
0
4";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 3);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 1623178306);
    }
}
