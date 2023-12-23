use std::collections::{HashMap, VecDeque};

use tap::Tap;

#[derive(Debug, Clone, Copy)]
pub enum State {
    On,
    Off,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Pulse {
    Low,
    High,
}

impl Pulse {
    /// Returns `true` if the pulse is [`Low`].
    ///
    /// [`Low`]: Pulse::Low
    #[must_use]
    pub fn is_low(&self) -> bool {
        matches!(self, Self::Low)
    }

    /// Returns `true` if the pulse is [`High`].
    ///
    /// [`High`]: Pulse::High
    #[must_use]
    pub fn is_high(&self) -> bool {
        matches!(self, Self::High)
    }
}

#[derive(Debug, Clone)]
pub enum Module<'a> {
    Broadcaster {
        connected_to: Vec<&'a str>,
    },
    FlipFlop {
        name: &'a str,
        state: State,
        connected_to: Vec<&'a str>,
    },
    Conjunction {
        name: &'a str,
        memory: HashMap<&'a str, Pulse>,
        connected_to: Vec<&'a str>,
    },
}

impl<'a> Module<'a> {
    fn connected_to(&self) -> &[&'a str] {
        match self {
            Module::Broadcaster { connected_to } => connected_to,
            Module::FlipFlop { connected_to, .. } => connected_to,
            Module::Conjunction { connected_to, .. } => connected_to,
        }
    }

    fn memory_mut(&mut self) -> Option<&mut HashMap<&'a str, Pulse>> {
        match self {
            Module::Conjunction { memory, .. } => Some(memory),
            _ => None,
        }
    }

    /// Returns `true` if the module is [`Conjunction`].
    ///
    /// [`Conjunction`]: Module::Conjunction
    #[must_use]
    fn is_conjunction(&self) -> bool {
        matches!(self, Self::Conjunction { .. })
    }
}

#[derive(Debug)]
struct Message<'a> {
    from: &'a str,
    to: &'a str,
    pulse: Pulse,
}

pub fn parse(input: &str) -> HashMap<&str, Module> {
    input
        .lines()
        .map(|line| {
            let (name, connected_to) = line.split_once(" -> ").unwrap();
            let connected_to = connected_to.split(',').map(|x| x.trim()).collect();

            if name == "broadcaster" {
                (name, Module::Broadcaster { connected_to })
            } else if let Some(name) = name.strip_prefix('%') {
                (
                    name,
                    Module::FlipFlop {
                        name,
                        state: State::Off,
                        connected_to,
                    },
                )
            } else if let Some(name) = name.strip_prefix('&') {
                (
                    name,
                    Module::Conjunction {
                        name,
                        memory: HashMap::new(),
                        connected_to,
                    },
                )
            } else {
                unreachable!("invalid line: {line}");
            }
        })
        .collect::<HashMap<_, _>>()
        .tap_mut(|hm| {
            let conjucntions = hm
                .iter()
                .filter_map(|(k, v)| v.is_conjunction().then_some(*k))
                .collect::<Vec<_>>();
            for conj in conjucntions {
                let mut connections_from = Vec::new();
                for (k, v) in hm.iter() {
                    if v.connected_to().contains(&conj) {
                        connections_from.push(*k);
                    }
                }
                for name in connections_from {
                    hm.get_mut(conj)
                        .and_then(|x| x.memory_mut())
                        .unwrap()
                        .insert(name, Pulse::Low);
                }
            }
        })
}

pub fn solve(modules: &HashMap<&str, Module>) -> (Option<u64>, Option<u64>) {
    // The test input does not have an "rx", and there is no sample result for part 2. We have to
    // disable this part of the code otherwise the unit tests wil fail.
    #[cfg(test)]
    let mut targets: HashMap<&str, u64> = HashMap::new();
    #[cfg(not(test))]
    let points_to_rx = modules
        .iter()
        .filter(|(_, v)| v.connected_to().contains(&"rx"))
        .map(|(k, _)| k)
        .next()
        .unwrap();
    #[cfg(not(test))]
    let mut targets: HashMap<_, _> = modules
        .iter()
        .filter(|(_, v)| v.connected_to().contains(points_to_rx))
        .map(|(k, _)| (k, 0))
        .collect();

    let mut modules = modules.to_owned();
    let mut lows = 0;
    let mut highs = 0;

    let mut p1 = None;
    let mut p2 = None;

    let mut vd = VecDeque::new();
    let mut to_enqueue = Vec::new();

    'outer: for button_presses in 1.. {
        vd.push_back(Message {
            from: "button",
            to: "broadcaster",
            pulse: Pulse::Low,
        });
        lows += 1;

        while let Some(message) = vd.pop_front() {
            if message.to == "output" {
                continue;
            }
            let Some(current) = modules.get_mut(message.to) else {
                continue;
            };

            match current {
                Module::Broadcaster { connected_to } => {
                    for dest in connected_to {
                        to_enqueue.push(Message {
                            from: "broadcaster",
                            to: dest,
                            pulse: message.pulse,
                        });
                    }
                }
                Module::FlipFlop {
                    name,
                    state,
                    connected_to,
                } => {
                    if message.pulse.is_high() {
                        // Flip-Flop modules ignore high pulses
                        continue;
                    }

                    match state {
                        State::On => {
                            *state = State::Off;
                            for dest in connected_to {
                                to_enqueue.push(Message {
                                    from: name,
                                    to: dest,
                                    pulse: Pulse::Low,
                                });
                            }
                        }
                        State::Off => {
                            *state = State::On;
                            for dest in connected_to {
                                to_enqueue.push(Message {
                                    from: name,
                                    to: dest,
                                    pulse: Pulse::High,
                                });
                            }
                        }
                    }
                }
                Module::Conjunction {
                    name,
                    memory,
                    connected_to,
                } => {
                    // Store the current pulse in the memory
                    *memory.get_mut(message.from).unwrap() = message.pulse;

                    if memory.values().all(|x| x.is_high()) {
                        // send lows
                        for dest in connected_to {
                            to_enqueue.push(Message {
                                from: name,
                                to: dest,
                                pulse: Pulse::Low,
                            });
                        }
                    } else {
                        // send highs
                        for dest in connected_to {
                            to_enqueue.push(Message {
                                from: name,
                                to: dest,
                                pulse: Pulse::High,
                            });
                        }
                    }
                }
            }

            for message in to_enqueue.drain(..) {
                if let Some(ptr) = targets.get_mut(&message.to)
                    && message.pulse.is_low()
                {
                    *ptr = button_presses;
                }
                let possible_ret = targets.values().product();
                if possible_ret != 0 {
                    p2 = Some(possible_ret);
                    if p1.is_some() {
                        break 'outer;
                    }
                }
                match message.pulse {
                    Pulse::Low => lows += 1,
                    Pulse::High => highs += 1,
                }

                vd.push_back(message);
            }
        }
        if button_presses == 1000 {
            p1 = Some(lows * highs);
            if p2.is_some() || cfg!(test) {
                break 'outer;
            }
        }
    }

    (p1, p2)
}

pub fn run(input: &str) {
    let parsed = parse(input);
    let (part1, part2) = solve(&parsed);
    println!("Part 1: {}", part1.unwrap());
    println!("Part 2: {}", part2.unwrap());
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT_1: &str = "broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a
";
    const INPUT_2: &str = "broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output
";

    #[test]
    fn test_part1() {
        assert_eq!(solve(&parse(INPUT_1)).0.unwrap(), 32000000);
        assert_eq!(solve(&parse(INPUT_2)).0.unwrap(), 11687500);
    }
}
