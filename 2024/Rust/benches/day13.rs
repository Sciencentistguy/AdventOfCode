use aoc_2024::day13;
use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let aoc = emergence::AoC::new(2024).unwrap();

    let input = aoc.read_or_fetch(13).unwrap();
    let parsed = day13::parse(&input);

    c.bench_function("day13::parse", |b| b.iter(|| day13::parse(&input)));
    c.bench_function("day13::part1", |b| b.iter(|| day13::part1(&parsed)));
    c.bench_function("day13::part2", |b| b.iter(|| day13::part2(&parsed)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
