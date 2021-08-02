use std::time::{Duration, Instant};

fn parse_input(input: &str) -> (Vec<&str>, Duration) {
    let start = Instant::now();
    let res = input.lines().collect();
    let end = Instant::now();
    (res, end - start)
}

fn is_tree(map: &[&str], x: usize, y: usize) -> bool {
    let width = map[0].len();
    map[y].as_bytes()[x % width] == b'#'
}

fn get_trees_slope(input: &[&str], x_step: usize, y_step: usize) -> usize {
    let mut x = 0;
    let mut y = 0;
    let mut count = 0;
    let y_limit = input.len();
    while y < y_limit {
        if is_tree(&input, x, y) {
            count += 1;
        }
        x += x_step;
        y += y_step;
    }
    count
}

fn solve_part1(input: &[&str]) -> (usize, Duration) {
    let start = Instant::now();
    let res = get_trees_slope(input, 3, 1);
    let end = Instant::now();
    (res, end - start)
}

fn solve_part2(input: &[&str]) -> (usize, Duration) {
    let start = Instant::now();
    let res = [
        get_trees_slope(input, 1, 1),
        get_trees_slope(input, 3, 1),
        get_trees_slope(input, 5, 1),
        get_trees_slope(input, 7, 1),
        get_trees_slope(input, 1, 2),
    ]
    .iter()
    .product();
    let end = Instant::now();
    (res, end - start)
}

pub fn run(input: String) {
    let (parsed_input, time) = parse_input(&input);
    println!("Day 03, parsing input took {}ns", time.as_nanos());
    let (p1, time) = solve_part1(&parsed_input);
    println!("Day 03, part 1: {}. Took {}ns", p1, time.as_nanos());
    let (p2, time) = solve_part2(&parsed_input);
    println!("Day 03, part 2: {}. Took {}ns", p2, time.as_nanos());
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &str = "..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#";

    #[test]
    fn day01_part1_example() {
        let parsed = parse_input(INPUT).0;
        assert_eq!(solve_part1(&parsed).0, 7);
    }
    #[test]
    fn day01_part2_example() {
        let parsed = parse_input(INPUT).0;
        assert_eq!(solve_part2(&parsed).0, 336);
    }
}
