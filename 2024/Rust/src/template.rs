type Parsed = ();
type Solution = u64;

pub fn parse(input: &str) -> Parsed {
    todo!()
}

pub fn part1((left, right): &Parsed) -> Solution {
    todo!()
}

pub fn part2((left, right): &Parsed) -> Solution {
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

    const INPUT: &str = todo!();

    const P1_SOLUTION: Solution = todo!();
    const P2_SOLUTION: Solution = todo!();

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}