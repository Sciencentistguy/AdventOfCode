use aoc_2023::day07;
use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let aoc = emergence::AoC::new(2023).unwrap();

    let input = aoc.read_or_fetch(7).unwrap();
    let parsed = day07::parse(&input);

    c.bench_function("day07::parse", |b| b.iter(|| day07::parse(&input)));
    c.bench_function("day07::part1", |b| b.iter(|| day07::part1(&parsed.0)));
    c.bench_function("day07::part2", |b| b.iter(|| day07::part2(&parsed.1)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
