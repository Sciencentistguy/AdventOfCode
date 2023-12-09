use smallvec::SmallVec;

type Storage<T> = SmallVec<T, 25>;
type State = Storage<Storage<i32>>;

#[derive(Clone)]
struct Row(Storage<i32>);

pub struct Report(Vec<Row>);

impl Row {
    fn extrapolate_into(&self, state: &mut State) {
        state.clear();
        state.push(self.0.clone());
        while !state.last().unwrap().iter().all(|&x| x == 0) {
            state.push(
                state
                    .last()
                    .unwrap()
                    .array_windows()
                    .map(|[a, b]| b - a)
                    .collect(),
            );
        }
    }
}

pub fn parse(inpt: &str) -> Report {
    Report(
        inpt.lines()
            .map(|x| {
                Row(
                    // iter::once(0)
                    // .chain(
                    x.split_whitespace()
                        .map(|x| x.parse().unwrap())
                        // )
                        .collect(),
                )
            })
            .collect(),
    )
}

pub fn part1(report: &Report) -> i32 {
    let mut ret = 0;
    let mut state = State::new();
    for row in &report.0 {
        row.extrapolate_into(&mut state);
        ret += state.iter().flat_map(|x| x.last()).sum::<i32>();
    }
    ret
}

pub fn part2(report: &Report) -> i32 {
    let mut ret = 0;
    let mut state = State::new();
    for row in &report.0 {
        let mut row = row.to_owned();
        row.0.reverse();
        row.extrapolate_into(&mut state);
        ret += state.iter().flat_map(|x| x.last()).sum::<i32>();
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
    const INPUT: &str = "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45
";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 114);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 2);
    }
}
