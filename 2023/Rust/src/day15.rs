use linked_hash_map::LinkedHashMap;

pub fn parse(input: &str) -> Vec<&str> {
    input.trim().split(',').collect()
}

fn hash(s: &str) -> usize {
    s.as_bytes()
        .iter()
        .fold(0, |acc, &n| ((acc + n as usize) * 17) % 256)
}

pub fn part1(init_sequence: &[&str]) -> usize {
    init_sequence.iter().map(|x| hash(x)).sum()
}

pub fn part2(init_sequence: &[&str]) -> usize {
    let mut boxes = vec![LinkedHashMap::<&str, u8>::new(); 256];

    for item in init_sequence {
        let (label, focal_length) = item.split_once(|c| c == '=' || c == '-').unwrap();

        let box_num = hash(label);

        if focal_length.is_empty() {
            boxes[box_num].remove(label);
        } else {
            *boxes[box_num].entry(label).or_insert(0) = focal_length.parse().unwrap();
        }
    }

    let mut ret = 0;

    for (b, r#box) in boxes.iter().enumerate() {
        for l in 0..r#box.len() {
            let label = r#box.keys().nth(l).unwrap();
            ret += (b + 1) * (l + 1) * boxes[b][label] as usize;
        }
    }
    ret
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &str = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7";

    #[test]
    fn test_hash() {
        assert_eq!(hash("HASH"), 52);
    }

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 1320);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 145);
    }
}
