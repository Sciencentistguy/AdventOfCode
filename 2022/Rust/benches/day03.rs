use aoc_2022::day03;
use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let aoc = emergence::AoC::new(2022).unwrap();

    let mut input = aoc.read_or_fetch(3).unwrap();

    c.bench_function("day03::parse", move |b| {
        b.iter(|| {
            let _ = day03::parse(&mut input);
        })
    });

    let mut input = aoc.read_or_fetch(3).unwrap();

    let parsed = day03::parse(&mut input).to_owned();

    c.bench_function("day03::part1", |b| b.iter(|| day03::part1(&parsed)));
    c.bench_function("day03::part2", |b| b.iter(|| day03::part2(&parsed)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
