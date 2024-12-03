use atoi::FromRadix10;
use tap::Tap;

type Parsed = Vec<Instruction>;
type Solution = u64;

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    Mul(u64, u64),
    Do,
    Dont,
}

pub fn parse(input: &str) -> Parsed {
    let input = input.as_bytes();
    let mut instructions = Vec::new();

    let all_potential_muls = memchr::memmem::find_iter(input, b"mul(");
    let all_dos = memchr::memmem::find_iter(input, b"do()");
    let all_donts = memchr::memmem::find_iter(input, b"don't()");

    let all_finds = all_potential_muls
        .chain(all_dos)
        .chain(all_donts)
        .collect::<Vec<_>>()
        .tap_mut(|v| v.sort_unstable());

    for find in all_finds {
        if input[find..].starts_with(b"mul(") {
            let input = &input[find + 4..];
            let (x, idx) = u64::from_radix_10(input);
            if idx == 0 {
                continue;
            }
            let input = &input[idx..];
            if input[0] != b',' {
                continue;
            }
            let input = &input[1..];
            let (y, idx) = u64::from_radix_10(input);
            if idx == 0 {
                continue;
            }
            let input = &input[idx..];
            if input[0] != b')' {
                continue;
            }
            instructions.push(Instruction::Mul(x, y));
        } else if input[find..].starts_with(b"do()") {
            instructions.push(Instruction::Do);
        } else if input[find..].starts_with(b"don't()") {
            instructions.push(Instruction::Dont);
        }
    }

    instructions
}

pub fn part1(parsed: &Parsed) -> Solution {
    parsed.iter().fold(0, |acc, instr| match instr {
        Instruction::Mul(x, y) => acc + x * y,
        _ => acc,
    })
}

pub fn part2(parsed: &Parsed) -> Solution {
    let mut ret = 0;
    let mut enabled = true;
    for instr in parsed {
        match instr {
            Instruction::Mul(x, y) => {
                if enabled {
                    ret += x * y;
                }
            }
            Instruction::Do => {
                enabled = true;
            }
            Instruction::Dont => {
                enabled = false;
            }
        }
    }
    ret
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const P1_INPUT: &str =
        "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))";
    const P2_INPUT: &str =
        "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))";

    const P1_SOLUTION: Solution = 161;
    const P2_SOLUTION: Solution = 48;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(P1_INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(P2_INPUT)), P2_SOLUTION);
    }
}
