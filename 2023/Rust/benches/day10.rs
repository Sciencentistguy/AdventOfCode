use aoc_2023::day10;
use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let aoc = emergence::AoC::new(2023).unwrap();

    let input = aoc.read_or_fetch(10).unwrap();
    let parsed = day10::parse(&input);

    c.bench_function("day10::parse", |b| b.iter(|| day10::parse(&input)));
    c.bench_function("day10::part1", |b| b.iter(|| day10::part1(&parsed)));
    c.bench_function("day10::part2", |b| b.iter(|| day10::part2(&parsed)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
