use std::hint::unreachable_unchecked;

use common::ArraySplit;
use compact_str::CompactString;

#[repr(C)]
struct Computer<'a> {
    instructions: &'a [u8],
    a: u64,
    b: u64,
    c: u64,
    pc: usize,
    output: Vec<u8>,
}

pub struct InitialState {
    instructions: Vec<u8>,
    a: u64,
    b: u64,
    c: u64,
}

type Parsed = InitialState;

pub fn parse(input: &str) -> Parsed {
    fn parse_register_init(s: &str) -> u64 {
        const REGISTER_OFFSET: usize = "Register X: ".len();
        s[REGISTER_OFFSET..].parse().unwrap()
    }

    let (init, instructions) = input.split_once("\n\n").unwrap();
    let [a, b, c] = init.array_lines().map(parse_register_init);

    const PROGRAM_OFFSET: usize = "Program: ".len();
    let instructions: Vec<u8> = instructions[PROGRAM_OFFSET..]
        .trim()
        .split(',')
        .map(|x| x.as_bytes()[0] - b'0')
        .collect();

    debug_assert!(!instructions.iter().any(|&x| x > 7));

    InitialState {
        instructions,
        a,
        b,
        c,
    }
}

impl<'a> Computer<'a> {
    fn combo(&self, combo: u8) -> u64 {
        static IMMEDIATE: [u64; 3] = [1, 2, 3];

        let lookup_2 = [
            std::ptr::null(),
            &raw const IMMEDIATE[0],
            &raw const IMMEDIATE[1],
            &raw const IMMEDIATE[2],
            &raw const self.a,
            &raw const self.b,
            &raw const self.c,
        ];

        debug_assert!((1..=6).contains(&combo));
        unsafe { **lookup_2.get_unchecked(combo as usize) }
    }

    fn from_initial_state(parsed: &'a InitialState) -> Self {
        Self {
            instructions: &parsed.instructions,
            a: parsed.a,
            b: parsed.b,
            c: parsed.c,
            pc: 0,
            output: Vec::with_capacity(parsed.instructions.len()),
        }
    }

    fn run_until_completion(&mut self) {
        while self.pc < self.instructions.len() {
            self.run_instruction();
        }
    }

    fn run_instruction(&mut self) {
        let opcode = self.instructions[self.pc];
        let operand = self.instructions[self.pc + 1];
        match opcode {
            0 => {
                let operand = self.combo(operand);
                self.a >>= operand;
            }
            1 => self.b ^= operand as u64,
            2 => {
                let operand = self.combo(operand);
                self.b = operand % 8;
            }
            3 => {
                if self.a != 0 {
                    self.pc = operand as usize;
                    return;
                }
            }
            4 => self.b ^= self.c,
            5 => {
                let operand = self.combo(operand);
                self.output.push((operand % 8) as u8);
            }
            6 => {
                let operand = self.combo(operand);
                self.b = self.a >> operand;
            }
            7 => {
                let operand = self.combo(operand);
                self.c = self.a >> operand;
            }
            _ => unsafe { unreachable_unchecked() },
        }
        self.pc += 2;
    }

    fn reset(&mut self, a: u64) {
        self.a = a;
        self.b = 0;
        self.c = 0;
        self.pc = 0;
        self.output.clear();
    }
}

pub fn part1(parsed: &Parsed) -> CompactString {
    let mut computer = Computer::from_initial_state(parsed);
    computer.run_until_completion();
    let mut out = CompactString::default();
    for c in computer.output {
        out.push((c + b'0') as char);
        out.push(',');
    }
    out.pop();
    out
}

pub fn part2(parsed: &Parsed) -> u64 {
    let mut i = parsed.instructions.len() - 1;
    let mut a = 0;
    let mut computer = Computer::from_initial_state(parsed);
    loop {
        computer.reset(a);
        computer.run_until_completion();
        if computer.output.get(i) == parsed.instructions.get(i) {
            if i == 0 {
                break;
            } else {
                i -= 1;
            }
        } else {
            a += 1 << (3 * i)
        }
    }
    a
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const P1_INPUT: &str = "Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0";
    const P2_INPUT: &str = "Register A: 2024
Register B: 0
Register C: 0

Program: 0,3,5,4,3,0
";

    const P1_SOLUTION: &str = "4,6,3,5,6,3,5,2,1,0";
    const P2_SOLUTION: u64 = 117440;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(P1_INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(P2_INPUT)), P2_SOLUTION);
    }
}
