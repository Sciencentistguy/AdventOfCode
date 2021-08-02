const TARGET: u64 = 2020;

pub fn parse_input(input: &str) -> Vec<u64> {
    let mut v = input
        .lines()
        .map(str::parse::<u64>)
        .collect::<Result<Vec<_>, _>>()
        .expect("Input contains invalid line");
    v.sort_unstable();
    v
}

pub fn solve_part1_bruteforce(input: &[u64]) -> u64 {
    for (c, i) in input.iter().enumerate() {
        for j in input[..c].iter() {
            if i + j == TARGET {
                return i * j;
            }
        }
    }
    unreachable!()
}

pub fn solve_part2_bruteforce(input: &[u64]) -> u64 {
    for i in input {
        for j in input {
            for k in input {
                if i + j + k == TARGET {
                    return i * j * k;
                }
            }
        }
    }
    unreachable!()
}

pub fn run(input: String) {
    let parsed_input = parse_input(&input);
    println!("Day 01, part 1: {}", solve_part1_bruteforce(&parsed_input));
    println!("Day 01, part 2: {}", solve_part2_bruteforce(&parsed_input));
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &str = "1721
979
366
299
675
1456";

    #[test]
    fn day01_part1() {
        let parsed = parse_input(INPUT);
        assert_eq!(solve_part1_bruteforce(&parsed), 514579);
    }
    #[test]
    fn day01_part2() {
        let parsed = parse_input(INPUT);
        assert_eq!(solve_part2_bruteforce(&parsed), 241861950);
    }
}
