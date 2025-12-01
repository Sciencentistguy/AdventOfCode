type Parsed = Vec<i64>;
type Solution = u64;

pub fn parse(input: &str) -> Parsed {
    input
        .lines()
        .map(|x| {
            let x = x.trim();
            let sign = match x.as_bytes()[0] {
                b'L' => 1,
                b'R' => -1,
                _ => unreachable!("wrong sign"),
            };
            sign * x[1..].parse::<i64>().unwrap()
        })
        .collect()
}

pub fn part1(parsed: &Parsed) -> Solution {
    let mut count = 0;
    let mut dial: i64 = 50;
    for rotation in parsed {
        dial += rotation;
        dial %= 100;
        if dial == 0 {
            count += 1;
        }
    }
    count
}

pub fn part2(parsed: &Parsed) -> Solution {
    let mut count = 0;
    let mut dial: i64 = 50;
    for &rotation in parsed {
        let start = dial;
        dial += rotation;
        let end = dial;

        if rotation > 0 {
            count += (end.div_euclid(100) - start.div_euclid(100)) as u64;
        } else if rotation < 0 {
            count += ((start - 1).div_euclid(100) - (end - 1).div_euclid(100)) as u64;
        }
    }
    count
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82
";

    const P1_SOLUTION: Solution = 3;
    const P2_SOLUTION: Solution = 6;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
