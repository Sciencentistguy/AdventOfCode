use crate::common::ArraySplit;

#[derive(Default, Debug)]
pub struct Race {
    time: usize,
    distance: usize,
}

pub fn parse(inpt: &str) -> (Vec<Race>, Race) {
    let [time, distance] = inpt.array_lines();

    let part1_interpretation = time[5..]
        .split_whitespace()
        .map(|x| x.parse().unwrap())
        .zip(distance[9..].split_whitespace().map(|x| x.parse().unwrap()))
        .map(|(time, distance)| Race { time, distance })
        .collect();
    let part2_interpretation = {
        let [time, dist] = inpt.array_lines();
        let time = time[5..]
            .chars()
            .filter_map(|c| c.to_digit(10))
            .fold(0u64, |curr, digit| curr * 10 + digit as u64);
        let distance = dist[9..]
            .chars()
            .filter_map(|c| c.to_digit(10))
            .fold(0u64, |curr, digit| curr * 10 + digit as u64);

        Race {
            time: time as usize,
            distance: distance as usize,
        }
    };
    (part1_interpretation, part2_interpretation)
}

fn total_wins(race: &Race) -> u32 {
    let b = race.time as f64;
    let c = race.distance as f64;

    let disc = (b * b - 4.0 * c).sqrt();

    // We need the floor / ceil to move in the directions we want. Usually this is no problem, but
    // if we have a value of disc that is an integer, then just dividing by 2 leaves us on an
    // integer boundary, which will cause errors. A small offset is added in the correct direction
    // to solve this.

    let upper = dbg!(((b + disc) / 2.0 - 0.0001).floor() as u32);
    let lower = dbg!(((b - disc) / 2.0 + 0.0001).ceil() as u32);

    upper - lower + 1
}

pub fn part1(races: &[Race]) -> u32 {
    races.iter().map(total_wins).product()
}

pub fn part2(race: &Race) -> u32 {
    total_wins(race)
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed.0));
    println!("Part 2: {}", part2(&parsed.1));
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &str = "Time:      7  15   30
Distance:  9  40  200
";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT).0), 288);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT).1), 71503);
    }
}
