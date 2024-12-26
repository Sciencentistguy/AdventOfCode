use criterion::{Criterion, criterion_group, criterion_main};

macro_rules! bench_day {
    ($c: ident, $aoc: ident, $day:ident) => {{
        use aoc_2024::$day;
        let input = $aoc
            .read_or_fetch(stringify!($day)[3..].parse().unwrap())
            .unwrap();
        let parsed = $day::parse(&input);

        $c.bench_function(concat!(stringify!($day), "::parse"), |b| {
            b.iter(|| $day::parse(&input))
        });
        $c.bench_function(concat!(stringify!($day), "::part1"), |b| {
            b.iter(|| $day::part1(&parsed))
        });
        $c.bench_function(concat!(stringify!($day), "::part2"), |b| {
            b.iter(|| $day::part2(&parsed))
        });
    }};
}

pub fn criterion_benchmark(c: &mut Criterion) {
    let aoc = emergence::AoC::new(2024).unwrap();

    bench_day!(c, aoc, day01);
    bench_day!(c, aoc, day02);
    bench_day!(c, aoc, day03);
    bench_day!(c, aoc, day04);
    bench_day!(c, aoc, day05);
    // bench_day!(c, aoc, day06);
    // day 6 is special
    {
        use aoc_2024::day06;
        let input = aoc.read_or_fetch(6).unwrap();
        let parsed = day06::parse(&input);
        let (_, visited) = day06::part1(&parsed);

        c.bench_function("day06::parse", |b| b.iter(|| day06::parse(&input)));
        c.bench_function("day06::part1", |b| b.iter(|| day06::part1(&parsed)));
        c.bench_function("day06::part2", |b| {
            b.iter(|| day06::part2(&parsed, &visited))
        });
    }
    bench_day!(c, aoc, day07);
    bench_day!(c, aoc, day08);
    bench_day!(c, aoc, day09);
    bench_day!(c, aoc, day10);
    bench_day!(c, aoc, day11);
    bench_day!(c, aoc, day12);
    bench_day!(c, aoc, day13);
    bench_day!(c, aoc, day14);
    // bench_day!(c, aoc, day15);
    // day 15 is special
    {
        use aoc_2024::day15;
        let input = aoc.read_or_fetch(15).unwrap();

        c.bench_function("day15::part1", |b| {
            b.iter(|| day15::part1(input.as_bytes()))
        });
        c.bench_function("day15::part2", |b| {
            b.iter(|| day15::part2(input.as_bytes()))
        });
    }
    bench_day!(c, aoc, day16);
    bench_day!(c, aoc, day17);
    bench_day!(c, aoc, day18);
    bench_day!(c, aoc, day19);
    bench_day!(c, aoc, day23);
    bench_day!(c, aoc, day24);
    bench_day!(c, aoc, day25);
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
