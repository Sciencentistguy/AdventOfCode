use aoc_2024::day15;
use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let aoc = emergence::AoC::new(2024).unwrap();

    let input = aoc.read_or_fetch(15).unwrap();

    c.bench_function("day15::part1", |b| b.iter(|| day15::part1(input.as_bytes())));
    c.bench_function("day15::part2", |b| b.iter(|| day15::part2(input.as_bytes())));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
