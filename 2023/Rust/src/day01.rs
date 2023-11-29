pub fn parse(inpt: &str) -> Vec<u64> {
    todo!()
}

pub fn part1(parsed: &[u64]) -> u64 {
    todo!()
}

pub fn part2(parsed: &[u64]) -> u64 {
    todo!()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), todo!());
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), todo!());
    }
}
