#![allow(dead_code)]
use std::time::{Duration, Instant};

use nohash_hasher::IntSet;

#[derive(Debug, Clone, Copy)]
enum Opcode {
    Acc(i64),
    Jmp(i64),
    Nop(i64),
}

impl Opcode {
    fn flip(&self) -> Self {
        match self {
            Self::Jmp(x) => Self::Nop(*x),
            Self::Nop(x) => Self::Jmp(*x),
            Self::Acc(x) => Self::Acc(*x),
        }
    }
}

fn parse_input(input: &str) -> (Vec<Opcode>, Duration) {
    let start = Instant::now();
    let instructions = input
        .lines()
        .map(|line| {
            let instr = &line[..3];
            let sign = line.as_bytes()[4] as char;
            let num: i64 = line[5..].parse().unwrap();
            let num = match sign {
                '-' => -num,
                _ => num,
            };
            match instr {
                "acc" => Opcode::Acc(num),
                "jmp" => Opcode::Jmp(num),
                "nop" => Opcode::Nop(num),
                _ => unreachable!("invalid input"),
            }
        })
        .collect();
    let end = Instant::now();
    (instructions, end - start)
}

fn solve_part1(input: &[Opcode]) -> (i64, Duration) {
    let start = Instant::now();
    let mut counted = vec![false; input.len()];
    let mut acc: i64 = 0;
    let mut pc: i64 = 0;
    while let Some(instruction) = input.get(pc as usize) {
        let used_before = &mut counted[pc as usize];
        if *used_before {
            let end = Instant::now();
            return (acc, end - start);
        }
        *used_before = true;
        match *instruction {
            Opcode::Jmp(x) => {
                pc += x;
            }
            Opcode::Acc(x) => {
                acc += x;
                pc += 1;
            }
            Opcode::Nop(_) => {
                pc += 1;
            }
        };
    }
    unreachable!()
}

fn solve_part2(input: &[Opcode]) -> (i64, Duration) {
    let start = Instant::now();
    let mut instruction_to_flip = 0;
    let mut pcs = IntSet::default();
    pcs.reserve(input.len());
    'outer: loop {
        pcs.clear();
        let mut acc: i64 = 0;
        let mut pc: i64 = 0;
        loop {
            let instruction = match input.get(pc as usize) {
                Some(x) if pc == instruction_to_flip => x.flip(),
                Some(x) => *x,
                None => break,
            };
            if !pcs.insert(pc) {
                instruction_to_flip += 1;
                continue 'outer;
            }
            match instruction {
                Opcode::Jmp(x) => {
                    pc += x;
                }
                Opcode::Acc(x) => {
                    acc += x;
                    pc += 1;
                }
                Opcode::Nop(_) => {
                    pc += 1;
                }
            };
        }
        let end = Instant::now();
        return (acc, end - start);
    }
}

pub fn run(input: String) {
    let (parsed_input, time) = parse_input(&input);
    println!("Day 06, parsing input took {}ns", time.as_nanos());
    let (p1, time) = solve_part1(&parsed_input);
    println!("Day 06, part 1: {}. Took {}ns", p1, time.as_nanos());
    let (p2, time) = solve_part2(&parsed_input);
    println!("Day 06, part 2: {}. Took {}ns", p2, time.as_nanos());
}
