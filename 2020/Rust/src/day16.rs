use std::{
    collections::HashMap,
    time::{Duration, Instant},
};

use regex::Regex;

type Ticket = Vec<usize>;

#[derive(Debug)]
struct Range {
    low_1: usize,
    high_1: usize,
    low_2: usize,
    high_2: usize,
}

#[derive(Debug)]
struct Parsed<'a> {
    fields: HashMap<&'a str, Range>,
    your_ticket: Vec<usize>,
    nearby_tickets: Vec<Ticket>,
}

impl Range {
    fn contains(&self, i: usize) -> bool {
        (i >= self.low_1 && i <= self.high_1) || (i >= self.low_2 && i <= self.high_2)
    }
}

fn parse_input(input: &str) -> (Parsed, Duration) {
    let start = Instant::now();

    let group_0_regex = Regex::new(r"^([\w ]+): (\d+)-(\d+) or (\d+)-(\d+)").unwrap();
    let lines = input.lines().collect::<Vec<_>>();
    let groups = lines.split(|x| x.is_empty()).collect::<Vec<_>>();

    let fields = groups[0]
        .iter()
        .map(|&line| {
            let capts = group_0_regex.captures(line).unwrap();
            let name = capts.get(1).unwrap().as_str();
            let rng = Range {
                low_1: capts[2].parse().unwrap(),
                high_1: capts[3].parse().unwrap(),
                low_2: capts[4].parse().unwrap(),
                high_2: capts[5].parse().unwrap(),
            };
            (name, rng)
        })
        .collect();

    let your_ticket = groups[1][1]
        .split(',')
        .map(|x| x.parse().unwrap())
        .collect();

    let nearby_tickets = groups[2]
        .iter()
        .skip(1)
        .map(|ticket| ticket.split(',').map(|num| num.parse().unwrap()).collect())
        .collect();
    //let mut nearby_tickets = Vec::new();

    //for i in 1..groups[2].len() {
    //nearby_tickets.push(
    //groups[2][i]
    //.split(',')
    //.map(|num| num.parse().unwrap())
    //.collect(),
    //);
    //}

    let out = Parsed {
        fields,
        your_ticket,
        nearby_tickets,
    };

    let end = Instant::now();
    (out, end - start)
}

fn is_valid_possible_value(input: &Parsed, value: usize) -> bool {
    !input.fields.iter().all(|(_, range)| !range.contains(value))
}

fn is_valid_ticket(input: &Parsed, ticket: &[usize]) -> bool {
    ticket.iter().all(|&x| is_valid_possible_value(input, x))
}

fn solve_part1(input: &Parsed) -> (usize, Duration) {
    let start = Instant::now();
    let mut count = 0;
    for ticket in input.nearby_tickets.iter() {
        for &x in ticket {
            if !is_valid_possible_value(input, x) {
                count += x;
            }
        }
    }
    let end = Instant::now();
    (count, end - start)
}

fn solve_part2(input: &Parsed) -> (usize, Duration) {
    let start = Instant::now();

    let valid_tickets = input
        .nearby_tickets
        .iter()
        .filter(|t| is_valid_ticket(input, t))
        .collect::<Vec<_>>();

    let mut candidates: Vec<(Vec<usize>, &str)> = input
        .fields
        .iter()
        .map(|(&name, range)| {
            (
                (0..input.your_ticket.len())
                    .filter(|i| {
                        valid_tickets
                            .iter()
                            .all(|ticket| range.contains(ticket[*i]))
                    })
                    .collect(),
                name,
            )
        })
        .collect();

    candidates.sort_unstable_by(|(a, _), (b, _)| a.len().cmp(&b.len()));

    let mut field_and_index = HashMap::new();
    let mut taken = vec![false; input.your_ticket.len()];

    for (possible, name) in candidates {
        for index in possible {
            if !taken[index] {
                field_and_index.insert(name, index);
                taken[index] = true;
                break;
            }
        }
    }

    let out = field_and_index
        .iter()
        .filter(|(name, _)| name.starts_with("departure"))
        .map(|(_, idx)| input.your_ticket[*idx])
        .product();

    let end = Instant::now();
    (out, end - start)
}

pub fn run(input: String) {
    let (parsed_input, time) = parse_input(&input);
    println!("Day 16, parsing input took {}ns", time.as_nanos());
    let (p1, time) = solve_part1(&parsed_input);
    println!("Day 16, part 1: {}. Took {}ns", p1, time.as_nanos());
    let (p2, time) = solve_part2(&parsed_input);
    println!("Day 16, part 2: {}. Took {}ns", p2, time.as_nanos());
}
