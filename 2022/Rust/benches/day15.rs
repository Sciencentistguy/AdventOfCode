use aoc_2022::day15;
use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let aoc = emergence::AoC::new(2022).unwrap();

    let input = aoc.read_or_fetch(15).unwrap();
    let parsed = day15::parse(&input);

    c.bench_function("day15::parse", |b| b.iter(|| day15::parse(&input)));
    c.bench_function("day15::part1", |b| {
        b.iter(|| day15::part1(&parsed, 2_000_000))
    });
    c.bench_function("day15::part2", |b| {
        b.iter(|| day15::part2(&parsed, 4_000_000))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
