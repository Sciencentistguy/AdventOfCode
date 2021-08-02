use std::collections::HashMap;
use std::time::Duration;
use std::time::Instant;

fn parse_input(input: &str) -> (Vec<HashMap<&str, &str>>, Duration) {
    let start = Instant::now();
    let grouped_entries = input
        .lines()
        .collect::<Vec<_>>()
        .split(|x| x.is_empty())
        .map(|x| x.to_vec())
        .collect::<Vec<_>>();
    let mut parsed_gi = Vec::with_capacity(grouped_entries.len());
    for passport in &grouped_entries {
        let mut hm = HashMap::with_capacity(8); // complete passport has 8 entries
        for &line in passport {
            for entry in line.split(' ') {
                if let Some(idx) = entry.find(':') {
                    let (name, val) = entry.split_at(idx);
                    let val = &val[1..]; // remove ':'
                    hm.insert(name, val);
                }
            }
        }
        parsed_gi.push(hm)
    }
    let end = Instant::now();
    (parsed_gi, end - start)
}

fn is_complete_passport(passport: &HashMap<&str, &str>) -> bool {
    const REQUIERD_FIELDS: &[&str] = &["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];
    REQUIERD_FIELDS
        .iter()
        .all(|&field| passport.keys().any(|&x| x == field))
}

fn solve_part1(input: &[HashMap<&str, &str>]) -> (usize, Duration) {
    let start = Instant::now();
    let res = input
        .iter()
        .filter(|passport| is_complete_passport(&passport))
        .count();
    let end = Instant::now();
    (res, end - start)
}

fn solve_part2(input: &[HashMap<&str, &str>]) -> (usize, Duration) {
    let start = Instant::now();
    let complete_passports = input
        .iter()
        .filter(|passport| is_complete_passport(&passport));
    let mut count = 0;
    for passport in complete_passports {
        let mut valid = true;
        for (&field, &value) in passport {
            valid = match field {
                "byr" => {
                    if let Ok(value) = value.parse::<u64>() {
                        (1920..=2002).contains(&value)
                    } else {
                        false
                    }
                }
                "iyr" => {
                    if let Ok(value) = value.parse::<u64>() {
                        (2010..=2020).contains(&value)
                    } else {
                        false
                    }
                }
                "eyr" => {
                    if let Ok(value) = value.parse::<u64>() {
                        (2020..=2030).contains(&value)
                    } else {
                        false
                    }
                }
                "hgt" => {
                    if let Some(idx) = value.rfind(|c: char| c.is_digit(10)) {
                        let (num, unit) = value.split_at(idx + 1);
                        if let Ok(num) = num.parse::<u64>() {
                            match unit {
                                "cm" => (150..=193).contains(&num),
                                "in" => (59..=76).contains(&num),
                                _ => false,
                            }
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                }
                "hcl" => {
                    let mut chars = value.chars();
                    chars.next() == Some('#') && chars.all(|c| c.is_digit(16))
                }
                "ecl" => {
                    matches!(value, "amb" | "blu" | "brn" | "gry" | "grn" | "hzl" | "oth")
                }
                "pid" => value.len() == 9 && value.chars().all(|c| c.is_digit(10)),
                "cid" => true,

                _ => unreachable!(),
            };
            if !valid {
                break;
            }
        }
        if valid {
            count += 1;
        }
    }
    let end = Instant::now();
    (count, end - start)
}

pub fn run(input: String) {
    let (parsed_input, time) = parse_input(&input);
    println!("Day 04, parsing input took {}ns", time.as_nanos());
    let (p1, time) = solve_part1(&parsed_input);
    println!("Day 04, part 1: {}. Took {}ns", p1, time.as_nanos());
    let (p2, time) = solve_part2(&parsed_input);
    println!("Day 04, part 2: {}. Took {}ns", p2, time.as_nanos());
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &str = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm

iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929

hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm

hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in
";
    #[test]
    fn day04_part1() {
        let parsed = parse_input(INPUT).0;
        assert_eq!(solve_part1(&parsed).0, 2);
    }
}
