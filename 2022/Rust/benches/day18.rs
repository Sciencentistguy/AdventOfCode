use aoc_2022::day18;
use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let aoc = emergence::AoC::new(2022).unwrap();

    let input = aoc.read_or_fetch(18).unwrap();
    let parsed = day18::parse(&input);

    c.bench_function("day18::parse", |b| b.iter(|| day18::parse(&input)));
    c.bench_function("day18::part1", |b| b.iter(|| day18::part1(&parsed)));
    c.bench_function("day18::part2", |b| b.iter(|| day18::part2(&parsed)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
