use aoc_2023::day23;
use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let aoc = emergence::AoC::new(2023).unwrap();

    let input = aoc.read_or_fetch(23).unwrap();
    let parsed = day23::parse(&input);

    c.bench_function("day23::parse", |b| b.iter(|| day23::parse(&input)));
    c.bench_function("day23::part1", |b| b.iter(|| day23::part1(&parsed)));
    c.bench_function("day23::part2", |b| b.iter(|| day23::part2(&parsed)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
