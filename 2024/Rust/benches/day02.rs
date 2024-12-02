use aoc_2024::day02;
use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let aoc = emergence::AoC::new(2024).unwrap();

    let input = aoc.read_or_fetch(1).unwrap();
    let parsed = day02::parse(&input);

    c.bench_function("day02::parse", |b| b.iter(|| day02::parse(&input)));
    c.bench_function("day02::part1", |b| b.iter(|| day02::part1(&parsed)));
    c.bench_function("day02::part2", |b| b.iter(|| day02::part2(&parsed)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
