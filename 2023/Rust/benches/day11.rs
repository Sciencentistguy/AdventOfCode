use aoc_2023::day11;
use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let aoc = emergence::AoC::new(2023).unwrap();

    let input = aoc.read_or_fetch(11).unwrap();
    let parsed = day11::parse(&input);

    c.bench_function("day11::parse", |b| b.iter(|| day11::parse(&input)));
    c.bench_function("day11::part1", |b| b.iter(|| day11::part1(&parsed.0)));
    c.bench_function("day11::part2", |b| b.iter(|| day11::part2(&parsed.1)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
