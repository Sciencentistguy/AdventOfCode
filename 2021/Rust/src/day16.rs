use std::{collections::VecDeque, mem, time::Instant};

use eyre::Result;

type Input<'a> = Packet;

struct Packet {
    version: usize,
    typeid: TypeID,
    contents: Contents,
}

enum Contents {
    Data(usize),
    Packets(Vec<Packet>),
}

impl Contents {
    fn data(&self) -> &usize {
        match self {
            Contents::Data(x) => x,
            Contents::Packets(_) => panic!("not data"),
        }
    }

    fn packets(&self) -> &[Packet] {
        match self {
            Contents::Data(_) => panic!("not packets"),
            Contents::Packets(x) => x,
        }
    }
}

fn consume_n(queue: &mut VecDeque<char>, n: usize) -> String {
    (0..n).map(|_| queue.pop_front().unwrap()).collect()
}

// These variants are inhabited, see `impl From<usize> for TypeID`
#[allow(dead_code)]
#[derive(PartialEq, Eq, Debug)]
enum TypeID {
    Sum,
    Product,
    Minimum,
    Maximum,
    Literal,
    Greater,
    Less,
    Equal,
}

impl From<usize> for TypeID {
    fn from(x: usize) -> Self {
        match x {
            0..=7 => unsafe { mem::transmute(x as u8) },
            _ => panic!("invalid typeID"),
        }
    }
}

mod typeid {}

impl Packet {
    fn new(input: &mut VecDeque<char>) -> Packet {
        let version = usize::from_str_radix(&consume_n(input, 3), 2).unwrap();
        match TypeID::from(usize::from_str_radix(&consume_n(input, 3), 2).unwrap()) {
            typeid @ TypeID::Literal => {
                // A literal packet. Extract the data and store it.
                let mut data = String::new();

                while {
                    // If the MSB of the next 4-bit section is `1`, it is not the last packet.
                    let should_continue = input.pop_front().unwrap().to_digit(10).unwrap() == 1;

                    data += &consume_n(input, 4);
                    should_continue
                } {}

                Packet {
                    version,
                    typeid,
                    contents: Contents::Data(usize::from_str_radix(&data, 2).unwrap()),
                }
            }

            typeid => {
                // A recursive packet. Parse inner packets and store them.
                match input.pop_front().unwrap().to_digit(10).unwrap() {
                    0 => {
                        // Consume the next 15 bits to get `len`. `len` is the length in bits of 
                        // all the sub-packets. We are done consuming packets when `len` bits have
                        // been consumed from input.
                        let len = usize::from_str_radix(&consume_n(input, 15), 2).unwrap();
                        let mut sub_packets = Vec::new();
                        let target_len = input.len() - len;

                        while input.len() > target_len {
                            sub_packets.push(Packet::new(input));
                        }

                        Packet {
                            version,
                            typeid,
                            contents: Contents::Packets(sub_packets),
                        }
                    }
                    1 => {
                        // Consume the next 11 bits to get `len`. `len` is the number of
                        // sub-packets.
                        let len = usize::from_str_radix(&consume_n(input, 11), 2).unwrap();

                        Packet {
                            version,
                            typeid,
                            contents: Contents::Packets(
                                (0..len).map(|_| Packet::new(input)).collect(),
                            ),
                        }
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

    fn sum_version(&self) -> usize {
        let mut ans = self.version;
        if let Contents::Packets(ref sub) = self.contents {
            for p in sub {
                ans += p.sum_version();
            }
        }
        ans
    }

    fn evaluate(&self) -> usize {
        match self.typeid {
            TypeID::Sum => self.contents.packets().iter().map(|p| p.evaluate()).sum(),

            TypeID::Product => self
                .contents
                .packets()
                .iter()
                .map(|p| p.evaluate())
                .product(),

            TypeID::Minimum => self
                .contents
                .packets()
                .iter()
                .map(|p| p.evaluate())
                .min()
                .unwrap(),

            TypeID::Maximum => self
                .contents
                .packets()
                .iter()
                .map(|p| p.evaluate())
                .max()
                .unwrap(),

            TypeID::Literal => *self.contents.data(),

            TypeID::Less => {
                let packets = self.contents.packets();

                if packets[0].evaluate() > packets[1].evaluate() {
                    1
                } else {
                    0
                }
            }

            TypeID::Greater => {
                let packets = self.contents.packets();

                if packets[0].evaluate() < packets[1].evaluate() {
                    1
                } else {
                    0
                }
            }

            TypeID::Equal => {
                let packets = self.contents.packets();

                if packets[0].evaluate() == packets[1].evaluate() {
                    1
                } else {
                    0
                }
            }
        }
    }
}
fn parse(input: &str) -> Input<'_> {
    let bin: String = input
        .trim()
        .chars()
        .map(|c| format!("{:0>4b}", c.to_digit(16).unwrap()))
        .collect();
    Packet::new(&mut bin.chars().collect())
}

fn part1(input: &Packet) -> usize {
    input.sum_version()
}

fn part2(input: &Packet) -> usize {
    input.evaluate()
}

pub fn run(input: String) -> Result<()> {
    let parsed = parse(input.as_str());
    let start = Instant::now();
    let part1 = part1(&parsed);
    let p1_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 16 (part 1) is {}. Took {:?}",
        part1, p1_time
    );
    let start = Instant::now();
    let part2 = part2(&parsed);
    let p2_time = Instant::now() - start;
    println!(
        "The solution to 2021 day 16 (part 2) is {}. Took {:?}",
        part2, p2_time
    );

    Ok(())
}
