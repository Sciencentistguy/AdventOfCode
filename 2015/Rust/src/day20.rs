use crate::Result;

pub fn parse(inpt: &str) -> Result<usize> {
    Ok(inpt.trim().parse()?)
}

fn divisors(n: usize, storage: &mut Vec<usize>) {
    for d in 1..n.isqrt() {
        if n % d == 0 {
            storage.push(d);
            if d != n / d {
                storage.push(n / d);
            }
        }
    }
}

pub fn part1(parsed: usize) -> usize {
    let mut space = Vec::new();
    let size = parsed.isqrt();
    space.reserve(size);
    (1..)
        .map(|n| {
            space.clear();
            divisors(n, &mut space);
            (n, space.iter().fold(0, |acc, i| acc + (10 * i)))
        })
        .find(|&(_, n)| n >= parsed)
        .unwrap()
        .0
}

pub fn part2(parsed: usize) -> usize {
    let mut space = Vec::new();
    let size = parsed.isqrt();
    space.reserve(size);
    (1..)
        .map(|n| {
            space.clear();
            divisors(n, &mut space);
            (
                n,
                space
                    .iter()
                    .fold(0, |acc, i| acc + if n / i <= 50 { 11 * i } else { 0 }),
            )
        })
        .find(|&(_, n)| n >= parsed)
        .unwrap()
        .0
}

pub fn run(input: &str) -> Result<()> {
    let parsed = parse(input)?;
    println!("Part 1: {}", part1(parsed));
    println!("Part 2: {}", part2(parsed));

    Ok(())
}
