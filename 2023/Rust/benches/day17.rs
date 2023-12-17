use aoc_2023::day17;
use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let aoc = emergence::AoC::new(2023).unwrap();

    let input = aoc.read_or_fetch(17).unwrap();
    let parsed = day17::parse(&input);

    c.bench_function("day17::parse", |b| b.iter(|| day17::parse(&input)));
    c.bench_function("day17::part1", |b| b.iter(|| day17::part1(&parsed)));
    c.bench_function("day17::part2", |b| b.iter(|| day17::part2(&parsed)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
