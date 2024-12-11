use fxhash::FxHashMap as HashMap;

type Parsed = HashMap<u128, u64>;
type Solution = u64;

const PART1_MAX: usize = 25;
const PART2_MAX: usize = 75;

pub fn parse(input: &str) -> Parsed {
    let mut map = HashMap::default();
    for num in input.trim().split_ascii_whitespace() {
        *map.entry(num.parse().unwrap()).or_default() += 1;
    }
    map
}

fn split_middle(num: u128) -> Option<(u128, u128)> {
    // Count number of digits
    let mut temp = num;
    let mut digit_count = 0;
    while temp > 0 {
        digit_count += 1;
        temp /= 10;
    }
    
    if digit_count % 2 != 0 {
        return None;
    }
    
    let divisor = 10_u128.pow(digit_count / 2 );

    let second = num % divisor;
    let first = num / divisor;
    
    Some((first, second))
}

fn blink(last: &mut Parsed, next: &mut Parsed){
    for (key, v) in last.drain() {
        if key == 0 {
            // rule 1
            *next.entry(1).or_default() += v;
        } else if let Some((first, second)) = split_middle(key) {
            // rule 2
            *next.entry(first).or_default() += v;
            *next.entry(second).or_default() += v;
        } else {
            // rule 3
            let key = key * 2024;
            *next.entry(key).or_default() += v;
        }
    }
}

pub fn part1(stones: &Parsed) -> Solution {
    let mut stones = stones.clone();
    let mut next = HashMap::default();
    for _ in 0..PART1_MAX {
        blink(&mut stones, &mut next);
        std::mem::swap(&mut stones, &mut next);
    }

    stones.values().sum()
}

pub fn part2(stones: &Parsed) -> Solution {
    let mut stones = stones.clone();
    let mut next = HashMap::default();
    for _ in 0..PART2_MAX {
        blink(&mut stones, &mut next);
        std::mem::swap(&mut stones, &mut next);
    }

    stones.values().sum()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "125 17";

    const P1_SOLUTION: Solution = 55312;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    } 

    // There is no p2 test :(

    #[test]
    fn test_split_middle() {
        assert_eq!(split_middle(1234), Some((12, 34)));
        assert_eq!(split_middle(12345), None);
        assert_eq!(split_middle(1000), Some((10, 0)));
    }
}
