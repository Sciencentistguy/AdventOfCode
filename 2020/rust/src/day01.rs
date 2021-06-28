use aoc_runner_derive::*;

const TARGET: u64 = 2020;

#[aoc_generator(day01)]
pub fn input_generator(input: &str) -> Vec<u64> {
    input
        .lines()
        .map(str::parse::<u64>)
        .collect::<Result<Vec<_>, _>>()
        .expect("Input contains invalid line")
}

#[aoc(day01, part1, bruteforce)]
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

#[aoc(day01, part2, bruteforce)]
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn day01_part1_example() {
        let input = "1721
979
366
299
675
1456
";
        println!("{}", input);
        let parsed = input_generator(input);
        println!("{:?}", parsed);
        let p1 = solve_part1_bruteforce(&parsed);
        println!("{:?}", p1);
        assert_eq!(p1, 514579);
    }
}
