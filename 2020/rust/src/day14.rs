use std::{
    collections::HashMap,
    time::{Duration, Instant},
};

#[derive(Debug)]
enum Instruction {
    Mem { location: usize, value: usize },
    Mask(Mask_),
}

#[derive(Debug, Default, Clone)]
struct Mask_ {
    ones: usize,
    zeros: usize,
    xs: Vec<usize>,
}

fn parse_input(input: &str) -> (Vec<Instruction>, Duration) {
    let start = Instant::now();
    let out = input
        .lines()
        .map(|line| {
            if line.starts_with("mask =") {
                let mut ones = 0;
                let mut zeros = 0;
                let mut xs = Vec::new();
                let s = line[7..].as_bytes();
                for (i, c) in s.iter().rev().enumerate() {
                    match c {
                        b'0' => zeros |= 1 << i,
                        b'1' => ones |= 1 << i,
                        b'X' => xs.push(i),
                        _ => unreachable!("invalid input"),
                    }
                }
                Instruction::Mask(Mask_ { ones, zeros, xs })
            } else if line.starts_with("mem[") {
                let location = line[4..line.find(']').unwrap()].parse().unwrap();
                let value = line[line.find('=').unwrap() + 2..].parse().unwrap();
                Instruction::Mem { location, value }
            } else {
                unreachable!("invalid input");
            }
        })
        .collect();
    /*
     *let mut out = Vec::with_capacity(input.lines().count());
     *for line in input.lines() {
     *    if line.starts_with("mask =") {
     *        let mut ones = 0;
     *        let mut zeros = 0;
     *        let mut xs = Vec::new();
     *        let s = line[7..].as_bytes();
     *        for (i, c) in s.iter().rev().enumerate() {
     *            match c {
     *                b'0' => zeros |= 1 << i,
     *                b'1' => ones |= 1 << i,
     *                b'X' => xs.push(i),
     *                _ => unreachable!("invalid input"),
     *            }
     *        }
     *        out.push(Instruction::Mask(Mask_ { ones, zeros, xs }));
     *    } else if line.starts_with("mem[") {
     *        let location = line[4..line.find(']').unwrap()].parse().unwrap();
     *        let value = line[line.find('=').unwrap() + 2..].parse().unwrap();
     *        out.push(Instruction::Mem { location, value });
     *    } else {
     *        unreachable!("invalid input");
     *    }
     *}
     */
    let end = Instant::now();
    (out, end - start)
}

fn solve_part1(input: &[Instruction]) -> (usize, Duration) {
    let start = Instant::now();
    let mut memory = HashMap::new();
    let mut mask_zeros = 0;
    let mut mask_ones = 0;

    for instruction in input {
        match instruction {
            &Instruction::Mem { location, value } => {
                let val = value & (!mask_zeros);
                let val = val | mask_ones;
                memory.insert(location, val);
            }
            Instruction::Mask(mask) => {
                mask_ones = mask.ones;
                mask_zeros = mask.zeros;
            }
        };
    }
    let count = memory.values().sum();
    let end = Instant::now();
    (count, end - start)
}

fn solve_part2(input: &[Instruction]) -> (usize, Duration) {
    let start = Instant::now();
    let mut memory = HashMap::new();
    let mut floating = Vec::new();
    let mut whitelist = 0;

    for instruction in input {
        match instruction {
            &Instruction::Mem { location, value } => {
                let addr = location & whitelist;
                for float_addr in &floating {
                    memory.insert(addr | float_addr, value);
                }
            }
            Instruction::Mask(mask) => {
                whitelist = mask.zeros;
                floating.clear();
                for template in 0..2usize.pow(mask.xs.len() as u32) {
                    let addr = mask.xs.iter().enumerate().fold(mask.ones, |addr, (i, fb)| {
                        addr | (template & 1 << i) << (fb - i)
                    });
                    floating.push(addr);
                }
            }
        };
    }

    let count = memory.values().sum();
    let end = Instant::now();
    (count, end - start)
}

pub fn run(input: String) {
    let (parsed_input, time) = parse_input(&input);
    println!("Day 14, parsing input took {}ns", time.as_nanos());
    let (p1, time) = solve_part1(&parsed_input);
    println!("Day 14, part 1: {}. Took {}ns", p1, time.as_nanos());
    let (p2, time) = solve_part2(&parsed_input);
    println!("Day 14, part 2: {}. Took {}ns", p2, time.as_nanos());
}
