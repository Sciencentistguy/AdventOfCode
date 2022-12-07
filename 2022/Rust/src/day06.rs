pub fn parse(input: &str) -> &[u8] {
    input.as_bytes()
}

#[rustfmt::skip]
pub fn part1(parsed: &[u8]) -> usize {
    for (pos, [a, b, c, d]) in parsed.array_windows().enumerate() {
        if     a != b && a != c && a != d
            && b != c && b != d
            && c != d {
            return pos + 4;
        }
    }
    unreachable!()
}

#[rustfmt::skip]
pub fn part2(parsed: &[u8]) -> usize {
    for (pos, [a, b, c, d, e, f, g, h, i, j, k, l, m, n]) in parsed.array_windows().enumerate() {
        if     a != b && a != c && a != d && a != e && a != f && a != g && a != h && a != i && a != j && a != k && a != l && a != m && a != n
            && b != c && b != d && b != e && b != f && b != g && b != h && b != i && b != j && b != k && b != l && b != m && b != n
            && c != d && c != e && c != f && c != g && c != h && c != i && c != j && c != k && c != l && c != m && c != n
            && d != e && d != f && d != g && d != h && d != i && d != j && d != k && d != l && d != m && d != n
            && e != f && e != g && e != h && e != i && e != j && e != k && e != l && e != m && e != n
            && f != g && f != h && f != i && f != j && f != k && f != l && f != m && f != n
            && g != h && g != i && g != j && g != k && g != l && g != m && g != n
            && h != i && h != j && h != k && h != l && h != m && h != n
            && i != j && i != k && i != l && i != m && i != n
            && j != k && j != l && j != m && j != n
            && k != l && k != m && k != n
            && l != m && l != n
            && m != n {
            return pos + 14;
        }
    }
    unreachable!()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(parsed));
    println!("Part 2: {}", part2(parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "mjqjpqmgbljsphdztnvjfqwrcgsmlb";

    #[test]
    fn test_part1() {
        let parsed = parse(INPUT);
        assert_eq!(7, part1(&parsed));
    }

    #[test]
    fn test_part2() {
        let parsed = parse(INPUT);
        assert_eq!(19, part2(&parsed));
    }
}
