use std::ops::{Index, IndexMut};

use crate::Result;

#[derive(Debug, Clone, Copy)]
#[repr(usize)]
pub enum Register {
    A = 0,
    B = 1,
}

impl From<&str> for Register {
    fn from(value: &str) -> Self {
        match value {
            "a" => Self::A,
            "b" => Self::B,
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    Hlf(Register),
    Tpl(Register),
    Inc(Register),
    Jmp(isize),
    Jie(Register, isize),
    Jio(Register, isize),
}

pub fn parse(inpt: &str) -> Vec<Instruction> {
    fn p_offset(off: &str) -> isize {
        off[1..].parse::<isize>().unwrap()
            * match off.as_bytes()[0] {
                b'+' => 1,
                b'-' => -1,
                _ => panic!(),
            }
    }
    inpt.lines()
        .map(|line| match &line[..3] {
            "hlf" => Instruction::Hlf(line[4..5].into()),
            "tpl" => Instruction::Tpl(line[4..5].into()),
            "inc" => Instruction::Inc(line[4..5].into()),
            "jmp" => Instruction::Jmp(p_offset(&line[4..])),
            "jie" => Instruction::Jie(line[4..5].into(), p_offset(&line[7..])),
            "jio" => Instruction::Jio(line[4..5].into(), p_offset(&line[7..])),
            _ => panic!(),
        })
        .collect()
}

impl Index<Register> for [usize; 2] {
    type Output = usize;

    fn index(&self, index: Register) -> &Self::Output {
        &self[index as usize]
    }
}

impl IndexMut<Register> for [usize; 2] {
    fn index_mut(&mut self, index: Register) -> &mut Self::Output {
        &mut self[index as usize]
    }
}

struct VM {
    a: usize,
    b: usize,
    pc: usize,
}
impl Index<Register> for VM {
    type Output = usize;

    fn index(&self, index: Register) -> &Self::Output {
        match index {
            Register::A => &self.a,
            Register::B => &self.b,
        }
    }
}

impl IndexMut<Register> for VM {
    fn index_mut(&mut self, index: Register) -> &mut Self::Output {
        match index {
            Register::A => &mut self.a,
            Register::B => &mut self.b,
        }
    }
}

fn run_vm(instructions: &[Instruction], mut state: VM) -> VM {
    while let Some(&instruction) = instructions.get(state.pc) {
        match instruction {
            Instruction::Hlf(r) => {
                state[r] /= 2;
                state.pc += 1;
            }
            Instruction::Tpl(r) => {
                state[r] *= 3;
                state.pc += 1;
            }
            Instruction::Inc(r) => {
                state[r] += 1;
                state.pc += 1;
            }
            Instruction::Jmp(off) => state.pc = state.pc.wrapping_add_signed(off),
            Instruction::Jie(r, off) => {
                state.pc = state
                    .pc
                    .wrapping_add_signed(if state[r] % 2 == 0 { off } else { 1 })
            }
            Instruction::Jio(r, off) => {
                state.pc = state
                    .pc
                    .wrapping_add_signed(if state[r] == 1 { off } else { 1 })
            }
        }
    }

    state
}

pub fn part1(parsed: &[Instruction]) -> usize {
    run_vm(parsed, VM { a: 0, b: 0, pc: 0 }).b
}

pub fn part2(parsed: &[Instruction]) -> usize {
    run_vm(parsed, VM { a: 1, b: 0, pc: 0 }).b
}

pub fn run(input: &str) -> Result<()> {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));

    Ok(())
}
