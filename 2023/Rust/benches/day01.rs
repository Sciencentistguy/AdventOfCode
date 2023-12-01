use aoc_2023::day01;
use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let aoc = emergence::AoC::new(2023).unwrap();

    let input = aoc.read_or_fetch(1).unwrap();
    let parsed = day01::parse(&input);

    c.bench_function("day01::parse", |b| b.iter(|| day01::parse(&input)));
    c.bench_function("day01::part1", |b| b.iter(|| day01::part1(&parsed)));
    c.bench_function("day01::part2", |b| b.iter(|| day01::part2(input.as_bytes())));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
