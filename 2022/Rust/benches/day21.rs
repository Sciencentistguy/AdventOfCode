use aoc_2022::day21;
use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let aoc = emergence::AoC::new(2022).unwrap();

    let input = aoc.read_or_fetch(21).unwrap();
    let parsed = day21::parse(&input);

    c.bench_function("day21::parse", |b| b.iter(|| day21::parse(&input)));
    c.bench_function("day21::part1", |b| b.iter(|| day21::part1(&parsed)));
    c.bench_function("day21::part2", |b| b.iter(|| day21::part2(&parsed)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
