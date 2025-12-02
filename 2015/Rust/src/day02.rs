use std::simd::prelude::*;

use atoi::FromRadix10;
use common::ByteSplit;
use itertools::izip;

type Parsed = Presents;
type Solution = u32;

pub struct Presents {
    ls: Vec<u32>,
    ws: Vec<u32>,
    hs: Vec<u32>,
}

pub fn parse(input: &str) -> Parsed {
    let input = input.trim().as_bytes();
    let lines = input.byte_lines().collect::<Vec<_>>();
    let mut ls = Vec::with_capacity(lines.len());
    let mut ws = Vec::with_capacity(lines.len());
    let mut hs = Vec::with_capacity(lines.len());
    for line in lines {
        let (l, i) = u32::from_radix_10(line);
        let line = &line[i + 1..];
        let (w, i) = u32::from_radix_10(line);
        let line = &line[i + 1..];
        let (h, _i) = u32::from_radix_10(line);

        ls.push(l);
        ws.push(w);
        hs.push(h);
    }
    Presents { ls, ws, hs }
}

pub fn solve(parsed: &Parsed) -> (Solution, Solution) {
    const LANES: usize = 64;
    type Sv = Simd<u32, LANES>;

    let (ls_chunks, ls_rest) = parsed.ls.as_chunks::<LANES>();
    let (ws_chunks, ws_rest) = parsed.ws.as_chunks::<LANES>();
    let (hs_chunks, hs_rest) = parsed.hs.as_chunks::<LANES>();

    let mut part1_sum: u32 = 0;
    let mut part2_sum: u32 = 0;

    for (ls, ws, hs) in izip!(ls_chunks, ws_chunks, hs_chunks) {
        let l_vec = Sv::from_array(*ls);
        let w_vec = Sv::from_array(*ws);
        let h_vec = Sv::from_array(*hs);

        let side1 = l_vec * w_vec;
        let side2 = w_vec * h_vec;
        let side3 = h_vec * l_vec;

        let surface_area = Sv::splat(2) * (side1 + side2 + side3);
        let smallest_side = side1.simd_min(side2).simd_min(side3);

        let p1_total = surface_area + smallest_side;

        let perim1 = Sv::splat(2) * (l_vec + w_vec);
        let perim2 = Sv::splat(2) * (w_vec + h_vec);
        let perim3 = Sv::splat(2) * (h_vec + l_vec);

        let smallest_perim = perim1.simd_min(perim2).simd_min(perim3);
        let volume = l_vec * w_vec * h_vec;

        let p2_total = smallest_perim + volume;

        part1_sum += p1_total.reduce_sum();
        part2_sum += p2_total.reduce_sum();
    }

    for (l, w, h) in izip!(ls_rest, ws_rest, hs_rest) {
        let side1 = l * w;
        let side2 = w * h;
        let side3 = h * l;

        let surface_area = 2 * (side1 + side2 + side3);
        let smallest_side = side1.min(side2).min(side3);

        let p1_total = surface_area + smallest_side;
        let perim1 = 2 * (l + w);
        let perim2 = 2 * (w + h);
        let perim3 = 2 * (h + l);
        let smallest_perim = perim1.min(perim2).min(perim3);
        let volume = l * w * h;
        let p2_total = smallest_perim + volume;

        part1_sum += p1_total;
        part2_sum += p2_total;
    }

    (part1_sum, part2_sum)
}

pub fn run(input: &str) {
    let parsed = parse(input);
    let (part1, part2) = solve(&parsed);
    println!("Part 1: {}", part1);
    println!("Part 2: {}", part2);
}
