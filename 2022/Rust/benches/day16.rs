use aoc_2022::day16;
use criterion::{criterion_group, criterion_main, Criterion};

pub fn criterion_benchmark(c: &mut Criterion) {
    let aoc = emergence::AoC::new(2022).unwrap();

    let input = aoc.read_or_fetch(16).unwrap();
    let parsed = day16::parse(&input);

    let (_, mm, opt, aa) = day16::part1(&parsed);

    c.bench_function("day16::parse", |b| b.iter(|| day16::parse(&input)));
    c.bench_function("day16::part1", |b| b.iter(|| day16::part1(&parsed)));
    c.bench_function("day16::part2", |b| b.iter(|| day16::part2(mm, &opt, aa)));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
