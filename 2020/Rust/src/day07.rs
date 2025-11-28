use std::collections::HashMap;
use std::collections::VecDeque;
use std::time::Duration;
use std::time::Instant;

#[derive(Debug, Clone)]
struct Rule<'a> {
    colour: &'a str,
    number: usize,
}

fn parse_input(input: &str) -> ((HashMap<&str, Vec<Rule<'_>>>, Vec<&str>), Duration) {
    let start = Instant::now();
    let mut bags: Vec<&str> = Vec::new();
    let mut rules: HashMap<&str, Vec<Rule>> = HashMap::new();
    for line in input.lines() {
        let split = line.split(' ').collect::<Vec<_>>();
        let colour = unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                line.as_ptr(),
                split[2].as_ptr().sub(1) as usize - line.as_ptr() as usize,
            ))
        };
        let mut i = 4;
        loop {
            if i >= split.len() {
                break;
            }
            let v = rules.entry(colour).or_default();
            if let Ok(number) = split[i].parse() {
                v.push(Rule {
                    colour: unsafe {
                        std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                            split[i + 1].as_ptr(),
                            split[i + 2].as_ptr().add(split[i + 2].len() - 2) as usize
                                - split[i].as_ptr() as usize,
                        ))
                    },
                    number,
                })
            }
            i += 4;
        }
        bags.push(colour)
    }
    let end = Instant::now();
    ((rules, bags), end - start)
}

fn solve_part1(rules: &HashMap<&str, Vec<Rule>>, bags: &[&str]) -> (usize, Duration) {
    let start = Instant::now();
    let mut count = 0;
    for &colour in bags {
        if colour == "shiny gold" {
            continue;
        }
        let mut candidates = VecDeque::with_capacity(rules.len());
        candidates.push_back(colour);
        while let Some(&bag) = candidates.front() {
            if bag == "shiny gold" {
                count += 1;
                break;
            }
            for subbag in &rules[bag] {
                candidates.push_back(subbag.colour);
            }
            candidates.pop_front();
        }
    }
    let end = Instant::now();
    (count, end - start)
}

fn solve_part2(rules: &HashMap<&str, Vec<Rule>>) -> (usize, Duration) {
    let start = Instant::now();
    let mut count = 0;
    let mut candidates: VecDeque<Rule> = VecDeque::new();
    candidates.push_back(Rule {
        colour: "shiny gold",
        number: 1,
    });
    while let Some(rule) = candidates.front().cloned() {
        count += rule.number;

        for subbag in &rules[rule.colour] {
            candidates.push_back(Rule {
                colour: subbag.colour,
                number: subbag.number * rule.number,
            });
        }
        candidates.pop_front();
    }
    count -= 1;
    let end = Instant::now();
    (count, end - start)
}

pub fn run(input: String) {
    let ((rules, bags), time) = parse_input(&input);
    println!("Day 07, parsing input took {}ns", time.as_nanos());
    let (p1, time) = solve_part1(&rules, &bags);
    println!("Day 07, part 1: {}. Took {}ns", p1, time.as_nanos());
    let (p2, time) = solve_part2(&rules);
    println!("Day 07, part 2: {}. Took {}ns", p2, time.as_nanos());
}
