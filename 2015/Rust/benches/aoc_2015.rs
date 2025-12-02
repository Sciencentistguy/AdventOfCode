use criterion::{Criterion, criterion_group, criterion_main};

macro_rules! bench_day {
    ($c: ident, $aoc: ident, $day:ident) => {{
        use aoc_2015::$day;
        let input = $aoc
            .read_or_fetch(stringify!($day)[3..].parse().unwrap())
            .unwrap();
        let parsed = $day::parse(&input);

        $c.bench_function(concat!("2015::", stringify!($day), "::parse"), |b| {
            b.iter(|| $day::parse(&input))
        });
        $c.bench_function(concat!("2015::", stringify!($day), "::part1"), |b| {
            b.iter(|| $day::part1(&parsed))
        });
        $c.bench_function(concat!("2015::", stringify!($day), "::part2"), |b| {
            b.iter(|| $day::part2(&parsed))
        });
    }};
}

pub fn criterion_benchmark(c: &mut Criterion) {
    let aoc = emergence::AoC::new(2015).unwrap();

    bench_day!(c, aoc, day01);
    {
        use aoc_2015::day02;
        let input = aoc
            .read_or_fetch(stringify!(day02)[3..].parse().unwrap())
            .unwrap();
        let parsed = day02::parse(&input);
        c.bench_function(concat!(stringify!(day02), "::parse"), |b| {
            b.iter(|| day02::parse(&input))
        });
        c.bench_function(concat!(stringify!(day02), "::solve"), |b| {
            b.iter(|| day02::solve(&parsed))
        });
    };
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
