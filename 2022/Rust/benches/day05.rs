use aoc_2022::day05;
use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let aoc = emergence::AoC::new(2022).unwrap();

    let input = aoc.read_or_fetch(5).unwrap();
    let parsed = day05::parse(&input);

    c.bench_function("day05::parse", |b| b.iter(|| day05::parse(&input)));
    c.bench_function("day05::part1", |b| b.iter(|| day05::part1(&parsed)));
    c.bench_function("day05::part2", |b| b.iter(|| day05::part2(&parsed)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
