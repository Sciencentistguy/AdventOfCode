use std::io::Write;

use itertools::Itertools;

#[derive(Debug)]
pub struct Instruction {
    number: usize,
    from: usize,
    to: usize,
}

#[inline]
fn double_borrow_mut<T>(slice: &mut [T], idx1: usize, idx2: usize) -> (&mut T, &mut T) {
    debug_assert!(idx1 < slice.len());
    debug_assert!(idx2 < slice.len());
    debug_assert_ne!(idx1, idx2);
    // Safety: Given the asserts above, we know that idx1 and idx2 are valid indices and are
    // disjoint
    unsafe {
        let ptr = slice.as_mut_ptr();
        let ptr1 = ptr.add(idx1);
        let ptr2 = ptr.add(idx2);
        (&mut *ptr1, &mut *ptr2)
    }
}

pub fn parse(input: &str) -> (Vec<Vec<u8>>, Vec<Instruction>) {
    debug_assert!(input.is_ascii());
    let (crates, instructions) = input.split("\n\n").collect_tuple().unwrap();

    // move {number} from {from} to {to}
    let instructions = instructions
        .lines()
        .map(|line| {
            let line = line.trim().as_bytes();

            let number_start = memchr::memchr(b' ', line).unwrap() + 1;
            let number_end = memchr::memchr(b' ', &line[number_start..]).unwrap() + number_start;
            let from_start = memchr::memchr(b'm', &line[number_end..]).unwrap() + number_end + 2;
            let to_start = memchr::memrchr(b' ', line).unwrap() + 1;

            // Safety: this byte slice is from a str + its ascii so i can slice where i want
            let number = unsafe { std::str::from_utf8_unchecked(&line[number_start..number_end]) }
                .parse::<usize>()
                .unwrap();

            let from = (line[from_start] - b'0') as usize;

            let to = (line[to_start] - b'0') as usize;

            Instruction { number, from, to }
        })
        .collect::<Vec<_>>();

    let crates: Vec<_> = crates.lines().map(|x| x.as_bytes()).collect();

    let num_stacks = crates
        .last()
        .unwrap()
        .iter()
        .filter(|&&x| x != b' ')
        .count();

    let mut stacks = vec![Vec::new(); num_stacks];

    for line in crates[..crates.len() - 1].iter() {
        // char we want for stack n is 4n + 1
        for (n, stack) in stacks.iter_mut().enumerate() {
            let c = line[(4 * n) + 1];
            if c != b' ' {
                stack.push(c);
            }
        }
    }

    for stack in &mut stacks {
        stack.reverse();
    }

    (stacks, instructions)
}

// Was used for debugging, I'll leave it in
#[allow(dead_code)]
fn dump_stacks(stacks: &[Vec<char>]) {
    let max_stack = stacks.iter().map(|x| x.len()).max().unwrap();
    for i in (0..=max_stack).rev() {
        for stack in stacks {
            if let Some(crate_) = stack.get(i) {
                print!(" {crate_} ");
            } else {
                print!("   ");
            }
        }
        println!();
    }
    for i in 0..=max_stack {
        print!(" {} ", i + 1);
    }
    println!();

    std::io::stdout().lock().flush().unwrap();
}

pub fn part1(parsed: &(Vec<Vec<u8>>, Vec<Instruction>)) -> String {
    let mut stacks = parsed.0.clone();
    for instruction in &parsed.1 {
        let (from, to) = double_borrow_mut(&mut stacks, instruction.from - 1, instruction.to - 1);
        let start_pos = from.len() - instruction.number;
        let move_from_slc = &mut from[start_pos..];
        move_from_slc.reverse();
        to.extend_from_slice(move_from_slc);
        from.truncate(start_pos);
    }
    String::from_utf8(
        stacks
            .iter()
            .map(|x| *x.last().unwrap())
            .collect::<Vec<u8>>(),
    )
    .unwrap()
}

pub fn part2(parsed: &(Vec<Vec<u8>>, Vec<Instruction>)) -> String {
    let mut stacks = parsed.0.clone();
    for instruction in &parsed.1 {
        let (from, to) = double_borrow_mut(&mut stacks, instruction.from - 1, instruction.to - 1);
        let start_pos = from.len() - instruction.number;
        let move_from_slc = &from[start_pos..];
        to.extend_from_slice(move_from_slc);
        from.truncate(start_pos);
    }
    String::from_utf8(
        stacks
            .iter()
            .map(|x| *x.last().unwrap())
            .collect::<Vec<u8>>(),
    )
    .unwrap()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
";

    #[test]
    fn test_part1() {
        let mut input = INPUT.to_owned();
        let parsed = parse(&mut input);
        assert_eq!("CMZ", part1(&parsed));
    }

    #[test]
    fn test_part2() {
        let mut input = INPUT.to_owned();
        let parsed = parse(&mut input);
        assert_eq!("MCD", part2(&parsed));
    }
}
