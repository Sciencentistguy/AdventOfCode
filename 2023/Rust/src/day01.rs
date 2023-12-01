use aho_corasick::AhoCorasick;

pub fn parse(inpt: &str) -> Vec<&[u8]> {
    inpt.trim()
        .split('\n')
        .map(|x| x.trim().as_bytes())
        .collect()
}

pub fn part1(parsed: &[&[u8]]) -> u32 {
    let mut ret = 0;
    let mut digits = Vec::with_capacity(8);
    for line in parsed {
        digits.clear();
        digits.extend(line.iter().filter(|c| c.is_ascii_digit()).map(|x| x - b'0'));
        ret += 10 * digits[0] as u32;
        ret += *digits.last().unwrap() as u32;
    }
    ret
}

pub fn part2(input_: &[u8]) -> u32 {
    let mut input = input_.to_owned();

    const PATTERNS: [&str; 9] = [
        "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    ];

    let ac = AhoCorasick::new(PATTERNS).unwrap();

    for mat in ac.find_overlapping_iter(input_) {
        /* crimes :D */
        input[mat.range().start + 1] = mat.pattern().as_u32() as u8 + 1 + b'0';
    }

    let transformed = unsafe { std::str::from_utf8_unchecked(&input) };

    part1(&parse(transformed))
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(input.as_bytes()));
}

#[cfg(test)]
mod tests {
    use super::*;
    const P1_INPUT: &str = "1abc2
    pqr3stu8vwx
    a1b2c3d4e5f
    treb7uchet";
    const P2_INPUT: &str = "two1nine
    eightwothree
    abcone2threexyz
    xtwone3four
    4nineeightseven2
    zoneight234
    7pqrstsixteen";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(P1_INPUT)), 142);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(P2_INPUT.as_bytes()), 281);
    }
}
