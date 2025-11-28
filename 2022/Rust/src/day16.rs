use std::collections::HashMap;

use memchr::memmem;
use ndarray::Array3;
use tap::Tap;

type Valves<'a> = Vec<(&'a str, u16, Vec<&'a str>)>;

pub fn parse(inpt: &str) -> Valves<'_> {
    assert!(inpt.is_ascii());
    inpt.lines()
        .map(|line| {
            let valve_start = 6;
            let valve_end = 8;
            let valve = &line[valve_start..valve_end];

            let flow_start = memchr::memchr(b'=', line.as_bytes()).unwrap() + 1;
            let flow_end = memchr::memchr(b';', line.as_bytes()).unwrap();
            let flow: u16 = line[flow_start..flow_end].parse().unwrap();

            let tunnels_start = memmem::find(line.as_bytes(), b"valve").unwrap() + 6;
            let tunnels_start = if line.as_bytes()[tunnels_start] == b's' {
                tunnels_start + 1
            } else {
                tunnels_start
            };
            let tunnels = line[tunnels_start..]
                .split(", ")
                .map(|x| x.trim())
                .collect();

            (valve, flow, tunnels)
        })
        .collect::<Vec<_>>()
        .tap_mut(|v| v.sort_unstable_by(|a, b| b.1.cmp(&a.1)))
}

pub fn part1(valves: &Valves) -> (u16, usize, Array3<u16>, usize) {
    let lab2idx = valves
        .iter()
        .enumerate()
        .map(|(i, v)| (v.0, i))
        .collect::<HashMap<_, _>>();

    let index_of = |label: &str| -> usize { lab2idx[label] };
    // the number of "useful" valves (with positive flow)
    // valves is sorted, so valves[..num_useful] all have positive flow
    let num_useful = valves.iter().filter(|v| v.1 > 0).count();
    let num_valves = valves.len();
    let mut connected_valves = vec![Vec::with_capacity(8); num_valves];
    let mut flow = vec![0u16; num_valves];
    for v in valves.iter() {
        let i = index_of(v.0);
        flow[i] = v.1;
        for w in v.2.iter() {
            connected_valves[i].push(index_of(w));
        }
    }

    let aa = index_of("AA");

    let mm = 1 << num_useful;
    // [time left, current node, bitset of available valves]
    let mut solution_space = Array3::<u16>::zeros([30, num_valves, mm]);
    // dynamic programming is a hell of a drug:
    for t in 1..30 {
        for i in 0..num_valves {
            let ii = 1 << i;
            for x in 0..mm {
                let mut val = solution_space[(t, i, x)];
                if ii & x != 0 {
                    val = val.max(solution_space[(t - 1, i, x - ii)] + flow[i] * t as u16);
                }
                for &j in connected_valves[i].iter() {
                    val = val.max(solution_space[(t - 1, j, x)]);
                }
                solution_space[(t, i, x)] = val;
            }
        }
    }

    let res = solution_space[(29, aa, mm - 1)];
    (res, mm, solution_space, aa)
}

pub fn part2(mm: usize, solution_space: &Array3<u16>, aa: usize) -> u16 {
    let mut best = 0;
    for x in 0..mm / 2 {
        let y = mm - 1 - x;
        best = best.max(solution_space[(25, aa, x)] + solution_space[(25, aa, y)]);
    }
    best
}

pub fn run(input: &str) {
    let parsed = parse(input);
    let (p1, mm, solution_space, aa) = part1(&parsed);
    println!("Part 1: {p1}");
    println!("Part 2: {}", part2(mm, &solution_space, aa));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)).0, 1651);
    }
    #[test]
    fn test_part2() {
        let (_, mm, opt, aa) = part1(&parse(INPUT));
        assert_eq!(part2(mm, &opt, aa), 1707);
    }
}
