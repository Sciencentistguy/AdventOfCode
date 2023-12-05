use std::ops::Range;

#[derive(Debug)]
struct Mapping {
    range: Range<usize>,
    offset: i64,
}

impl Mapping {
    fn map_range(&self, range: Range<usize>) -> MappedRange {
        // opt_range returns Some(range) if range is not empty, else None. this allows us to filter
        // out empty ranges earlier
        let opt_range = |start, end| Some(start..end).filter(|r| !r.is_empty());
        let before = opt_range(range.start, self.range.start.min(range.end));
        let after = opt_range(self.range.end.max(range.start), range.end);
        let overlap = opt_range(
            range.start.max(self.range.start),
            range.end.min(self.range.end),
        )
        .map(|r| ((r.start as i64 + self.offset) as usize)..(r.end as i64 + self.offset) as usize);
        MappedRange {
            before,
            overlap,
            after,
        }
    }
}

#[derive(Debug, Clone)]
struct MappedRange {
    before: Option<Range<usize>>,
    overlap: Option<Range<usize>>,
    after: Option<Range<usize>>,
}

#[derive(Debug)]
struct Mappings(pub Vec<Mapping>);

impl Mappings {
    fn translate(&self, x: usize) -> usize {
        for mapping in &self.0 {
            if mapping.range.contains(&x) {
                return (x as i64 + mapping.offset) as usize;
            }
        }
        x
    }

    fn translate_range(&self, input_range: Range<usize>) -> impl Iterator<Item = Range<usize>> {
        let mut overlaps = vec![];
        let mut leftovers = vec![input_range];
        let mut leftovers_queue = vec![];
        for mapping in &self.0 {
            for input_range in leftovers.drain(..) {
                let mapped = mapping.map_range(input_range);
                overlaps.extend(mapped.overlap);
                leftovers_queue.extend(mapped.before);
                leftovers_queue.extend(mapped.after);
            }
            std::mem::swap(&mut leftovers, &mut leftovers_queue);
        }
        overlaps.into_iter().chain(leftovers)
    }
}

#[derive(Debug)]
pub struct Alamanac {
    seeds: Vec<usize>,
    mappings: [Mappings; 7],
}

fn parse_map(inpt: &str) -> Mappings {
    Mappings(
        inpt.lines()
            .skip(1)
            .map(|x| {
                let parts: Vec<usize> = x
                    .split_ascii_whitespace()
                    .map(|x| x.parse().unwrap())
                    .collect();
                let range = parts[1]..parts[1] + parts[2];
                Mapping {
                    range,
                    offset: parts[0] as i64 - parts[1] as i64,
                }
            })
            .collect(),
    )
}

pub fn parse(inpt: &str) -> Alamanac {
    let sections = inpt.trim().split("\n\n").collect::<Vec<_>>();
    let unparsed_mappings: [&str; 7] = sections[1..].try_into().unwrap();

    Alamanac {
        seeds: sections[0][7..]
            .split_ascii_whitespace()
            .map(|x| x.parse().unwrap())
            .collect(),
        mappings: unparsed_mappings.map(parse_map),
    }
}

pub fn part1(almanac: &Alamanac) -> usize {
    almanac
        .seeds
        .iter()
        .map(|&seed| {
            almanac
                .mappings
                .iter()
                .fold(seed, |acc, mapping| mapping.translate(acc))
        })
        .min()
        .unwrap()
}

pub fn part2(almanac: &Alamanac) -> usize {
    let seed_ranges = almanac.seeds.array_chunks().map(|&[a, b]| a..a + b);

    seed_ranges
        .flat_map(|seed_range| {
            almanac
                .mappings
                .iter()
                .fold(vec![seed_range], |acc, mappings| {
                    acc.into_iter()
                        .flat_map(|input_range| mappings.translate_range(input_range))
                        .collect()
                })
        })
        .map(|locations| locations.start)
        .min()
        .unwrap()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;
    const INPUT: &str = "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 35);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 46);
    }
}
