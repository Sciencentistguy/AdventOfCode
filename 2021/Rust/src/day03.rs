use std::time::Instant;

use eyre::Result;

type BinaryNumber = Vec<bool>;

fn parse(input: &str) -> Vec<BinaryNumber> {
    input
        .lines()
        .map(|l| {
            l.chars()
                .map(|c| match c {
                    '0' => false,
                    '1' => true,
                    _ => unreachable!(),
                })
                .collect()
        })
        .collect()
}

fn bitvec_to_int(vec: BinaryNumber) -> u64 {
    let mut ret = 0;
    for b in vec {
        ret *= 2;
        if b {
            ret += 1;
        }
    }
    ret
}

fn part1(input: &[BinaryNumber]) -> Result<u64> {
    let len = input[0].len();
    let mut gamma = Vec::with_capacity(len);
    let mut epsilon = Vec::with_capacity(len);

    for i in 0..len {
        let mut counts = [0; 2];
        for num in input {
            counts[num[i] as usize] += 1;
        }
        let x = counts[0] > counts[1];
        gamma.push(!x);
        epsilon.push(x);
    }

    Ok(bitvec_to_int(gamma) * bitvec_to_int(epsilon))
}

fn part2(input: &[BinaryNumber]) -> u64 {
    let mut o2 = input.to_owned();
    let mut co2 = input.to_owned();

    for i in 0..input[0].len() {
        let o2_len = o2.len();
        let co2_len = co2.len();
        if o2_len == 1 && co2_len == 1 {
            break;
        }
        if o2_len > 1 {
            let mut counts = [0; 2];
            for num in &o2 {
                counts[num[i] as usize] += 1;
            }
            o2.retain(|n| n[i] != (counts[0] > counts[1]));
        }
        if co2_len > 1 {
            let mut counts = [0; 2];
            for num in &co2 {
                counts[num[i] as usize] += 1;
            }
            co2.retain(|n| n[i] != (counts[0] <= counts[1]));
        }
    }

    let o2 = o2.swap_remove(0);
    let co2 = co2.swap_remove(0);

    bitvec_to_int(o2) * bitvec_to_int(co2)
}

pub fn run(input: String) -> Result<()> {
    let parsed = parse(input.as_str());
    let start = Instant::now();
    let part1 = part1(&parsed)?;
    let p1_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 03 (part 1) is {}. Took {:?}",
        part1, p1_time
    );
    let start = Instant::now();
    let part2 = part2(&parsed);
    let p2_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 03 (part 2) is {}. Took {:?}",
        part2, p2_time
    );

    Ok(())
}
