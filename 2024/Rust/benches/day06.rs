use aoc_2024::day06;
use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let aoc = emergence::AoC::new(2024).unwrap();

    let input = aoc.read_or_fetch(6).unwrap();
    let parsed = day06::parse(&input);
    let visited = day06::part1(&parsed).1;

    c.bench_function("day06::parse", |b| b.iter(|| day06::parse(&input)));
    c.bench_function("day06::part1", |b| b.iter(|| day06::part1(&parsed)));
    c.bench_function("day06::part2", |b| b.iter(|| day06::part2(&parsed, &visited)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
