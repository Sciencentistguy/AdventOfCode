use std::{
    convert::Infallible,
    ops::{Deref, DerefMut},
    str::FromStr,
};

pub enum Instruction {
    Addx(i64),
    Noop,
}

impl FromStr for Instruction {
    type Err = Infallible;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match &s[..4] {
            "noop" => Ok(Self::Noop),
            "addx" => Ok(Self::Addx(s[5..].parse().unwrap())),
            _ => panic!("Unknown instruction: {}", s),
        }
    }
}

pub fn parse(inpt: &str) -> Vec<Instruction> {
    inpt.lines().map(|l| l.parse().unwrap()).collect()
}

pub fn part1(instructions: &[Instruction]) -> i64 {
    fn check_critical_point(ret: &mut i64, acc: i64, pc: i64) {
        if pc % 40 == 20 {
            *ret += acc * pc;
        }
    }

    let mut acc = 1;
    let mut pc = 0;

    let mut ret = 0;

    for instruction in instructions {
        match instruction {
            Instruction::Noop => {
                pc += 1;

                check_critical_point(&mut ret, acc, pc);
            }
            Instruction::Addx(val) => {
                for _ in 0..2 {
                    pc += 1;

                    check_critical_point(&mut ret, acc, pc);
                }
                acc += val;
            }
        }
    }

    ret
}

#[repr(transparent)]
struct Crt([char; 240]);

impl Default for Crt {
    fn default() -> Self {
        Self([' '; 240])
    }
}

impl Deref for Crt {
    type Target = [char; 240];

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Crt {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl Crt {
    fn render(self) -> String {
        let mut out = String::with_capacity(250); // a little more than 240 + newlines
        for line in self.array_chunks::<40>() {
            for char in line {
                out.push(*char);
            }
            out.push('\n');
        }
        out
    }

    fn write_pixel(&mut self, pc: usize, acc: i64) {
        self[pc] = if acc.abs_diff((pc % 40) as i64) <= 1 {
            '█'
        } else {
            '░'
        };
    }
}

pub fn part2(instructions: &[Instruction]) -> String {
    let mut crt = Crt::default();
    let mut pc = 0;
    let mut acc: i64 = 1;

    for instruction in instructions {
        match instruction {
            Instruction::Noop => {
                crt.write_pixel(pc, acc);
                pc += 1;
            }
            Instruction::Addx(val) => {
                for _ in 0..2 {
                    crt.write_pixel(pc, acc);
                    pc += 1;
                }
                acc += *val;
            }
        }
    }

    crt.render()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2:\n{}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 13140);
    }
    #[test]
    fn test_part2() {
        const EXPECTED: &str = "██░░██░░██░░██░░██░░██░░██░░██░░██░░██░░
███░░░███░░░███░░░███░░░███░░░███░░░███░
████░░░░████░░░░████░░░░████░░░░████░░░░
█████░░░░░█████░░░░░█████░░░░░█████░░░░░
██████░░░░░░██████░░░░░░██████░░░░░░████
███████░░░░░░░███████░░░░░░░███████░░░░░
";

        assert_eq!(part2(&parse(INPUT)), EXPECTED);
    }
}
