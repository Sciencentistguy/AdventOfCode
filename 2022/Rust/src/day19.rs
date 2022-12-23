type Recipe = [u16; 4];

pub struct Blueprint([Recipe; 4]);

impl Blueprint {
    fn simulate(&self, max_time: u16) -> usize {
        struct State {
            ores: [u16; 4],
            robots: [u16; 4],
            time: u16,
        }

        // Nicking a pattern from haskell here
        fn go(
            blueprint: &Blueprint,
            state: State,
            max_time: u16,
            max_robots: &[u16; 4],
            max_geodes: &mut u16,
        ) {
            let mut has_recursed = false;
            for i in 0..4 {
                if state.robots[i] == max_robots[i] {
                    continue;
                }
                let recipe = &blueprint.0[i];
                // Find the limiting ore for the recipe.
                let wait_time = (0..3)
                    .filter_map(|ore_type| {
                        if recipe[ore_type] == 0 {
                            None
                        } else if recipe[ore_type] <= state.ores[ore_type] {
                            Some(0)
                        } else if state.robots[ore_type] == 0 {
                            // No robot yet, we can't build it (it takes more than max_time to build it).
                            Some(max_time + 1)
                        } else {
                            Some(
                                (recipe[ore_type] - state.ores[ore_type] + state.robots[ore_type]
                                    - 1)
                                    / state.robots[ore_type],
                            )
                        }
                    })
                    .max()
                    .unwrap();
                let time_finished = state.time + wait_time + 1;
                if time_finished >= max_time {
                    continue;
                }
                let mut new_ores = [0; 4];
                let mut new_robots = [0; 4];
                for o in 0..4 {
                    new_ores[o] = state.ores[o] + state.robots[o] * (wait_time + 1) - recipe[o];
                    new_robots[o] = state.robots[o] + if o == i { 1 } else { 0 };
                }
                let remaining_time = max_time - time_finished;
                // If we were to build only geode robots every turn, could we beat the current max?
                if ((remaining_time - 1) * remaining_time) / 2
                    + new_ores[3]
                    + remaining_time * new_robots[3]
                    < *max_geodes
                {
                    continue;
                }
                has_recursed = true;
                go(
                    blueprint,
                    State {
                        ores: new_ores,
                        robots: new_robots,
                        time: time_finished,
                    },
                    max_time,
                    max_robots,
                    max_geodes,
                );
            }
            if !has_recursed {
                // We couldn't make new robots, so this is the best this branch can do.
                *max_geodes = std::cmp::max(
                    *max_geodes,
                    state.ores[3] + state.robots[3] * (max_time - state.time),
                );
            }
        }

        // The actual call:

        let mut max_robots = [u16::max_value(); 4];
        for i in 0..3 {
            max_robots[i] = self.0.iter().map(|r| r[i]).max().unwrap();
        }
        let mut max_geodes = 0;
        go(
            self,
            State {
                ores: [0; 4],
                robots: [1, 0, 0, 0],
                time: 0,
            },
            max_time,
            &max_robots,
            &mut max_geodes,
        );
        max_geodes as _
    }
}

pub fn parse(inpt: &str) -> Vec<Blueprint> {
    inpt.lines()
        .map(|line| {
            let mut iter = line.split_ascii_whitespace();
            let ore = [iter.nth(6).unwrap().parse().unwrap(), 0, 0, 0];
            let clay = [iter.nth(5).unwrap().parse().unwrap(), 0, 0, 0];
            let obsidian = [
                iter.nth(5).unwrap().parse().unwrap(),
                iter.nth(2).unwrap().parse().unwrap(),
                0,
                0,
            ];
            let geode = [
                iter.nth(5).unwrap().parse().unwrap(),
                0,
                iter.nth(2).unwrap().parse().unwrap(),
                0,
            ];
            Blueprint([ore, clay, obsidian, geode])
        })
        .collect()
}

pub fn part1(blueprints: &[Blueprint]) -> usize {
    blueprints
        .iter()
        .enumerate()
        .map(|(i, b)| b.simulate(24) * (i + 1))
        .sum()
}

pub fn part2(blueprints: &[Blueprint]) -> usize {
    blueprints
        .iter()
        .take(3)
        .map(|b| b.simulate(32))
        .product::<usize>()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 24000);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 45000);
    }
}
