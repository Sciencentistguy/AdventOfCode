use bstr::ByteSlice;
use common::ByteSplit;

type Parsed = Vec<Present>;
type Solution = u64;

pub struct Present {
    l: u64,
    w: u64,
    h: u64,
}

impl Present {
    fn surface_area(&self) -> u64 {
        2 * (self.l * self.w + self.w * self.h + self.h * self.l)
    }

    fn smallest_side_area(&self) -> u64 {
        let side1 = self.l * self.w;
        let side2 = self.w * self.h;
        let side3 = self.h * self.l;
        *[side1, side2, side3].iter().min().unwrap()
    }

    fn smallest_perimeter(&self) -> u64 {
        let mut sides = [self.l, self.w, self.h];
        sides.sort_unstable();
        2 * (sides[0] + sides[1])
    }
     
    fn volume(&self) -> u64 {
        self.l * self.w * self.h
    }
}

pub fn parse(input: &str) -> Parsed {
    input
        .trim()
        .as_bytes()
        .byte_lines()
        .map(|line| {
            let (l, rest) = line.byte_split_once(b'x').unwrap();
            let (w, h) = rest.byte_split_once(b'x').unwrap();
            unsafe {
                Present {
                    l: l.to_str_unchecked().parse().unwrap(),
                    w: w.to_str_unchecked().parse().unwrap(),
                    h: h.to_str_unchecked().parse().unwrap(),
                }
            }
        })
        .collect()
}

pub fn part1(parsed: &Parsed) -> Solution {
    parsed
        .iter()
        .map(|present| present.surface_area() + present.smallest_side_area())
        .sum()
}

pub fn part2(parsed: &Parsed) -> Solution {
    parsed
        .iter()
        .map(|present| present.smallest_perimeter() + present.volume())
        .sum()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}