use aoc_2024::day08;
use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let aoc = emergence::AoC::new(2024).unwrap();

    let input = aoc.read_or_fetch(8).unwrap();
    let parsed = day08::parse(&input);

    c.bench_function("day08::parse", |b| b.iter(|| day08::parse(&input)));
    c.bench_function("day08::part1", |b| b.iter(|| day08::part1(&parsed)));
    c.bench_function("day08::part2", |b| b.iter(|| day08::part2(&parsed)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
