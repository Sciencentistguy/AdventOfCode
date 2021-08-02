fn parse_input(input: &str) -> Vec<&str> {
    input.lines().collect()
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

fn solve_part1(input: &[&str]) -> usize {
    get_trees_slope(input, 3, 1)
}

fn solve_part2(input: &[&str]) -> usize {
    [
        get_trees_slope(input, 1, 1),
        get_trees_slope(input, 3, 1),
        get_trees_slope(input, 5, 1),
        get_trees_slope(input, 7, 1),
        get_trees_slope(input, 1, 2),
    ]
    .iter()
    .product()
}

pub fn run(input: String) {
    let parsed_input = parse_input(&input);
    println!("Day 03, part 1: {}", solve_part1(&parsed_input));
    println!("Day 03, part 2: {}", solve_part2(&parsed_input));
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
        let parsed = parse_input(INPUT);
        assert_eq!(solve_part1(&parsed), 7);
    }
    #[test]
    fn day01_part2_example() {
        let parsed = parse_input(INPUT);
        assert_eq!(solve_part2(&parsed), 336);
    }
}
