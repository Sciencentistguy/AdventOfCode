type Parsed = Vec<(u64, u64)>;
type Solution = u64;

pub fn parse(input: &str) -> Parsed {
    input
        .trim()
        .split(',')
        .map(|pair| pair.split_once('-').unwrap())
        .map(|(a, b)| (a.parse().unwrap(), b.parse().unwrap()))
        .collect()
}

pub fn part1(parsed: &Parsed) -> Solution {
    parsed
        .iter()
        .flat_map(|(a, b)| *a..=*b)
        .filter(|id| is_repeated_sequence_2(*id))
        .sum::<u64>()
}

fn is_repeated_sequence_2(n: u64) -> bool {
    if n == 0 {
        return false; // breaks for 0
    }

    let digits = n.ilog10() + 1;
    if digits % 2 != 0 {
        return false; // skip odd digit numbers
    }

    let half_digits = digits / 2;
    let divisor = 10u64.pow(half_digits);

    let upper = n / divisor;
    let lower = n % divisor;

    upper == lower
}

fn is_repeated_sequence_n(n: u64) -> bool {
    if n == 0 {
        return false;
    }

    let digits = n.ilog10() + 1;

    for period in 1..=digits / 2 {
        if digits % period != 0 {
            continue; // if not divisible, can't be repeated
        }

        let divisor = 10u64.pow(period);
        let pattern = n % divisor;
        let mut temp = n;
        let mut matches = true;

        while temp > 0 {
            if temp % divisor != pattern {
                matches = false;
                break;
            }
            temp /= divisor;
        }

        if matches {
            return true;
        }
    }

    false
}

pub fn part2(parsed: &Parsed) -> Solution {
    parsed
        .iter()
        .flat_map(|(a, b)| *a..=*b)
        .filter(|id| is_repeated_sequence_n(*id))
        .sum::<u64>()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124";

    const P1_SOLUTION: Solution = 1227775554;
    const P2_SOLUTION: Solution = 4174379265;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
