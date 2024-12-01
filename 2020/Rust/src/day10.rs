use std::{
    convert::TryInto,
    iter,
    time::{Duration, Instant},
};

struct Interval {
    begin: usize,
    end: usize,
    width: usize,
}

fn parse_input(input: &str) -> (Vec<usize>, Duration) {
    let start = Instant::now();
    let mut out: Vec<_> = iter::once(0)
        .chain(input.lines().map(|x| x.parse().expect("Invalid input")))
        .collect();
    out.sort_unstable();
    let end = Instant::now();
    (out, end - start)
}

fn solve_part1(input: &[usize]) -> (usize, Duration) {
    let start = Instant::now();
    let diffs: Vec<_> = (0..input.len() - 1)
        .map(|i| input[i + 1] - input[i])
        .collect();
    let out =
        diffs.iter().filter(|&&d| d == 1).count() * diffs.iter().filter(|&&d| d == 3).count() + 1;
    let end = Instant::now();
    (out, end - start)
}

fn solve_part2(input: &[usize]) -> (usize, Duration) {
    let start = Instant::now();

    let mut input = input.to_owned();
    input.push(input.last().unwrap() + 3);

    let mut i = 0;
    let mut j = 0;
    let mut intervals: Vec<Interval> = Vec::with_capacity(input.len());

    while i < input.len() && j < input.len() {
        j = i + 1;
        while j < input.len() && input[j] - input[i] <= 3 {
            j += 1;
        }
        if j - i > 2 {
            let width = j - i - 2;
            if let Some(interval) = intervals.last_mut() {
                if input[i] < interval.end {
                    *interval = Interval {
                        begin: interval.begin,
                        end: input[j - 1],
                        width: interval.width + width - 1,
                    };
                } else {
                    intervals.push(Interval {
                        begin: input[i],
                        end: input[j - 1],
                        width,
                    })
                }
            } else {
                intervals.push(Interval {
                    begin: input[i],
                    end: input[j - 1],
                    width,
                });
            }
        }
        i += 1;
    }
    let mut count = 1;
    for interval in intervals {
        count *= 2usize.pow(interval.width.try_into().unwrap())
            - ((interval.end - interval.begin > 3) as usize);
    }
    let end = Instant::now();
    (count, end - start)
}

pub fn run(input: String) {
    let (parsed_input, time) = parse_input(&input);
    println!("Day 10, parsing input took {}ns", time.as_nanos());
    let (p1, time) = solve_part1(&parsed_input);
    println!("Day 10, part 1: {}. Took {}ns", p1, time.as_nanos());
    let (p2, time) = solve_part2(&parsed_input);
    println!("Day 10, part 2: {}. Took {}ns", p2, time.as_nanos());
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3";

    #[test]
    fn day10_part1() {
        let parsed = parse_input(INPUT).0;
        assert_eq!(solve_part1(&parsed).0, 220);
    }
    #[test]
    fn day11_part1() {
        let parsed = parse_input(INPUT).0;
        assert_eq!(solve_part2(&parsed).0, 19208);
    }
}
