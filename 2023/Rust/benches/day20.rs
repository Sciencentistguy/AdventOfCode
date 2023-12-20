use aoc_2023::day20;
use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let aoc = emergence::AoC::new(2023).unwrap();

    let input = aoc.read_or_fetch(20).unwrap();
    let parsed = day20::parse(&input);

    c.bench_function("day20::parse", |b| b.iter(|| day20::parse(&input)));
    c.bench_function("day20::solve", |b| b.iter(|| day20::solve(&parsed)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
