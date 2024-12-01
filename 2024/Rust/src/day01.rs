type Parsed = (Vec<u64>, Vec<u64>);
type Solution = u64;

pub fn parse(input: &str) -> Parsed {
    let lines = input.lines().collect::<Vec<_>>();
    let length = lines.len();

    let mut left = Vec::with_capacity(length);
    let mut right = Vec::with_capacity(length);

    for line in lines {
        let (a, b) = line.split_at(line.find(' ').unwrap());
        let a = a.parse::<u64>().unwrap();
        let b = b.trim().parse::<u64>().unwrap();

        left.push(a);
        right.push(b);
    }

    left.sort();
    right.sort();

    (left, right)
}

pub fn part1((left, right): &Parsed) -> Solution {
    left.iter()
        .zip(right.iter())
        .map(|(a, &b)| a.abs_diff(b))
        .sum()
}

fn count_in(v: &[u64], n: u64) -> u64 {
    let mut count = 0;
    for &num in v {
        if num == n {
            count += 1;
        }
    }
    count
}

pub fn part2((left, right): &Parsed) -> Solution {
    let mut counter = 0;
    for &num in left {
        counter += num * count_in(right, num);
    }
    counter
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}
