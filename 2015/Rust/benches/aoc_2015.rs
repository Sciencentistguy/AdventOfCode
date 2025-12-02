use criterion::{Criterion, criterion_group, criterion_main};

macro_rules! bench_day {
    ($c: ident, $aoc: ident, $day:ident) => {{
        use aoc_2015::$day;
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
    let aoc = emergence::AoC::new(2015).unwrap();

    bench_day!(c, aoc, day01);
    bench_day!(c, aoc, day02);
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
