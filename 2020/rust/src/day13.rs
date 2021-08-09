use std::time::{Duration, Instant};

struct Timetable {
    depart_time: i64,
    buses: Vec<Option<i64>>,
}

fn parse_input(input: &str) -> (Timetable, Duration) {
    let start = Instant::now();
    let mut lines = input.lines();
    let depart_time = lines.next().and_then(|x| x.parse().ok()).unwrap();
    let valid_buses = lines
        .next()
        .unwrap()
        .split(',')
        .map(|x| x.parse().ok())
        .collect();
    let out = Timetable {
        depart_time,
        buses: valid_buses,
    };
    let end = Instant::now();
    (out, end - start)
}

fn solve_part1(input: &Timetable) -> (i64, Duration) {
    let start = Instant::now();

    let mut bus = 0;
    let mut actual_time = 0;

    'outer: for time in input.depart_time.. {
        for bus_id in input.buses.iter().flatten() {
            if time % bus_id == 0 {
                bus = *bus_id;
                actual_time = time;
                break 'outer;
            }
        }
    }

    let out = bus * (actual_time - input.depart_time);

    let end = Instant::now();
    (out, end - start)
}

fn solve_part2(input: &Timetable) -> (i64, Duration) {
    let start = Instant::now();
    let mut wait_times = Vec::with_capacity(input.buses.len());
    let mut minute: i64 = -1;
    let mut multiplier = 0;

    for bus in input.buses.iter() {
        minute += 1;
        if let Some(bus) = bus {
            if minute == 0 {
                multiplier = *bus;
            } else {
                wait_times.push((bus, bus - (minute % bus)));
            }
        }
    }

    let mut timestamp = 0;
    for (first, second) in wait_times {
        while (timestamp % first) != second {
            timestamp += multiplier;
        }
        multiplier *= first;
    }

    let end = Instant::now();
    (timestamp, end - start)
}

pub fn run(input: String) {
    let (parsed_input, time) = parse_input(&input);
    println!("Day 13, parsing input took {}ns", time.as_nanos());
    let (p1, time) = solve_part1(&parsed_input);
    println!("Day 13, part 1: {}. Took {}ns", p1, time.as_nanos());
    let (p2, time) = solve_part2(&parsed_input);
    println!("Day 13, part 2: {}. Took {}ns", p2, time.as_nanos());
}
