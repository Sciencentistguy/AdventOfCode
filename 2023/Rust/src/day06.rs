#[derive(Default, Debug)]
pub struct Race {
    time: usize,
    distance: usize,
}

pub fn parse(inpt: &str) -> Vec<Race> {
    let mut it = inpt.lines();
    let time = it.next().unwrap();
    let time = time[5..].split_whitespace().map(|x| x.parse().unwrap());
    let distance = it.next().unwrap();
    let distance = distance[9..].split_whitespace().map(|x| x.parse().unwrap());
    time.zip(distance)
        .map(|(time, distance)| Race { time, distance })
        .collect()
}

fn total_wins(race: &Race) -> usize {
    (0..race.time)
        .filter(|wait| {
            let speed = wait;
            let time_remaining = race.time - wait;
            let distance_travelled = speed * time_remaining;
            distance_travelled > race.distance
        })
        .count()
}

pub fn part1(races: &[Race]) -> usize {
    races.iter().map(total_wins).product()
}

pub fn part2(input: &str) -> usize {
    let mut it = input.lines();
    let time = it.next().unwrap();
    let distance = it.next().unwrap();

    let time = time
        .chars()
        .filter(|x| x.is_ascii_digit())
        .fold(0, |acc, c| (acc * 10) + c.to_digit(10).unwrap()) as usize;
    let distance = distance
        .chars()
        .filter(|x| x.is_ascii_digit())
        .fold(0, |acc, c| (acc * 10) + c.to_digit(10).unwrap()) as usize;

    let race = Race { time, distance };

    total_wins(&race)
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(input));
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &str = "Time:      7  15   30
Distance:  9  40  200
";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 288);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(INPUT), 71503);
    }
}
