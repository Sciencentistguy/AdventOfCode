use std::{collections::HashSet, time::Instant};

use eyre::Result;

#[derive(Hash, PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
struct Point {
    x: i64,
    y: i64,
}

#[derive(Debug, Clone, Copy)]
enum Fold {
    X(i64),
    Y(i64),
}
type Graph = HashSet<Point>;
type Input = (Graph, Vec<Fold>);

fn parse(input: &str) -> Input {
    let lines: Vec<_> = input.lines().collect();
    let mut graph = HashSet::new();
    let mut splitted = lines.split(|x| x.is_empty());

    for &line in splitted.next().unwrap() {
        let (x, y) = line.split_at(line.find(',').unwrap());
        let y = &y[1..];
        graph.insert(Point {
            x: x.parse().unwrap(),
            y: y.parse().unwrap(),
        });
    }

    let mut folds = Vec::new();
    for &line in splitted.next().unwrap() {
        let line = &line[11..];
        let num = line[2..].parse().unwrap();
        folds.push(match line.chars().next().unwrap() {
            'x' => Fold::X(num),
            'y' => Fold::Y(num),
            x => unreachable!("invalid coordinate `{}`", x),
        })
    }

    (graph, folds)
}

fn render_graph(graph: &Graph) -> String {
    let max_x = graph.iter().max_by_key(|x| x.x).unwrap().x;
    let max_y = graph.iter().max_by_key(|x| x.y).unwrap().y;

    let mut out = String::with_capacity((max_y * max_x).abs() as usize);

    for y in 0..=max_y {
        for x in 0..=max_x {
            out.push(if graph.contains(&Point { x, y }) {
                '#'
            } else {
                ' '
            });
        }
        out.push('\n');
    }
    out
}

fn part1((graph, folds): &Input) -> usize {
    let fold = folds[0];
    let mut new_graph: Graph = HashSet::with_capacity(graph.len());
    for p in graph {
        let p = p.clone();
        match fold {
            Fold::X(coord) => {
                if p.x > coord {
                    new_graph.insert(Point {
                        x: coord - (p.x - coord),
                        y: p.y,
                    });
                } else {
                    new_graph.insert(p);
                }
            }
            Fold::Y(coord) => {
                if p.y > coord {
                    new_graph.insert(Point {
                        x: p.x,
                        y: coord - (p.y - coord),
                    });
                } else {
                    new_graph.insert(p);
                }
            }
        }
    }
    new_graph.len()
}

fn part2((graph, folds): &Input) -> String {
    let mut graph = graph.to_owned();
    let mut new_graph = HashSet::with_capacity(graph.len());
    for &fold in folds {
        for p in &graph {
            let p = p.clone();
            match fold {
                Fold::X(coord) => {
                    if p.x > coord {
                        new_graph.insert(Point {
                            x: coord - (p.x - coord),
                            y: p.y,
                        });
                    } else {
                        new_graph.insert(p);
                    }
                }
                Fold::Y(coord) => {
                    if p.y > coord {
                        new_graph.insert(Point {
                            x: p.x,
                            y: coord - (p.y - coord),
                        });
                    } else {
                        new_graph.insert(p);
                    }
                }
            }
        }
        graph.clear();
        graph.extend(new_graph.drain());
    }

    render_graph(&graph)
}

pub fn run(input: String) -> Result<()> {
    let parsed = parse(input.as_str());
    let start = Instant::now();
    let part1 = part1(&parsed);
    let p1_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 13 (part 1) is {}. Took {:?}",
        part1, p1_time
    );
    let start = Instant::now();
    let part2 = part2(&parsed);
    let p2_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 13 (part 2) is:\n{}Took {:?}",
        part2, p2_time
    );

    Ok(())
}
