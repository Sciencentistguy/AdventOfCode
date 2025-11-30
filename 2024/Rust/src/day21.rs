use rayon::prelude::*;

type Parsed = [[ButtonPress; 4]; 5];
type Solution = u64;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ButtonPress {
    Num(u8),
    A,
    Up,
    Down,
    Left,
    Right,
}

trait NumericValue {
    fn numeric_component(&self) -> u64;
}

impl NumericValue for [ButtonPress] {
    fn numeric_component(&self) -> u64 {
        let mut value = 0u64;
        for &button in self {
            match button {
                ButtonPress::Num(digit) => {
                    value = value * 10 + digit as u64;
                }
                _ => {}
            }
        }
        value
    }
}

enum KeypadLayout {
    /// +---+---+---+
    /// | 7 | 8 | 9 |
    /// +---+---+---+
    /// | 4 | 5 | 6 |
    /// +---+---+---+
    /// | 1 | 2 | 3 |
    /// +---+---+---+
    ///     | 0 | A |
    ///     +---+---+
    Numeric,
    ///     +---+---+
    ///     | ^ | A |
    /// +---+---+---+
    /// | < | v | > |
    /// +---+---+---+
    Directional,
}
struct Keypad {
    layout: KeypadLayout,
    position: (usize, usize),
}

impl Keypad {
    fn get_path_to(&mut self, target: ButtonPress) -> Vec<ButtonPress> {
        let target_pos = match self.layout {
            KeypadLayout::Numeric => match target {
                ButtonPress::Num(7) => (0, 0),
                ButtonPress::Num(8) => (0, 1),
                ButtonPress::Num(9) => (0, 2),
                ButtonPress::Num(4) => (1, 0),
                ButtonPress::Num(5) => (1, 1),
                ButtonPress::Num(6) => (1, 2),
                ButtonPress::Num(1) => (2, 0),
                ButtonPress::Num(2) => (2, 1),
                ButtonPress::Num(3) => (2, 2),
                ButtonPress::Num(0) => (3, 1),
                ButtonPress::A => (3, 2),
                _ => panic!("Invalid button for Numeric keypad"),
            },
            KeypadLayout::Directional => match target {
                ButtonPress::Up => (0, 1),
                ButtonPress::A => (0, 2),
                ButtonPress::Left => (1, 0),
                ButtonPress::Down => (1, 1),
                ButtonPress::Right => (1, 2),
                _ => panic!("Invalid button for Directional keypad"),
            },
        };

        let (curr_r, curr_c) = self.position;
        let (target_r, target_c) = target_pos;
        let mut moves = Vec::new();

        let dr = target_r as isize - curr_r as isize;
        let dc = target_c as isize - curr_c as isize;

        // Determine gap position to avoid
        let gap = match self.layout {
            KeypadLayout::Numeric => (3, 0),
            KeypadLayout::Directional => (0, 0),
        };

        // Logic to prioritize moves to avoid the gap
        // If moving Left would hit the gap, move Up/Down first.
        // If moving Down would hit the gap, move Left/Right first.
        let move_vert = if dr < 0 {
            vec![ButtonPress::Up; dr.abs() as usize]
        } else {
            vec![ButtonPress::Down; dr.abs() as usize]
        };

        let move_horiz = if dc < 0 {
            vec![ButtonPress::Left; dc.abs() as usize]
        } else {
            vec![ButtonPress::Right; dc.abs() as usize]
        };

        // Check if horizontal move first hits gap
        let horiz_first_hits_gap = (curr_r, target_c) == gap;
        // Check if vertical move first hits gap
        let vert_first_hits_gap = (target_r, curr_c) == gap;

        if horiz_first_hits_gap {
            moves.extend(move_vert);
            moves.extend(move_horiz);
        } else if vert_first_hits_gap {
            moves.extend(move_horiz);
            moves.extend(move_vert);
        } else {
            // Heuristic: usually < is most expensive to reach on next robot, so do it last?
            // Or < is furthest from A.
            // For now, simple preference: < before v/u before >
            if dc < 0 {
                // Left
                moves.extend(move_horiz);
                moves.extend(move_vert);
            } else {
                moves.extend(move_vert);
                moves.extend(move_horiz);
            }
        }

        moves.push(ButtonPress::A);
        self.position = target_pos;
        moves
    }
}

pub fn parse(input: &str) -> Parsed {
    let mut ret = [[ButtonPress::A; 4]; 5]; // uninit?
    for (i, line) in input.lines().enumerate() {
        for (j, ch) in line.chars().enumerate().take(4) {
            ret[i][j] = match ch {
                'A' => ButtonPress::A,
                '0'..='9' => ButtonPress::Num(ch as u8 - b'0'),
                _ => panic!("Invalid character in input"),
            };
        }
    }
    ret
}

fn solve(parsed: &Parsed, intermedite_numpads: u64) -> Solution {
    parsed
        .par_iter()
        .map(|sequence| {
            let mut final_keypad = Keypad {
                layout: KeypadLayout::Numeric,
                position: (3, 2),
            };
            let mut path = Vec::new();
            for &button in sequence {
                let moves = final_keypad.get_path_to(button);
                path.extend(moves);
            }
            for _ in 0..intermedite_numpads {
                let mut keypad = Keypad {
                    layout: KeypadLayout::Directional,
                    position: (0, 2),
                };
                let mut path2 = Vec::new();
                for &button in &path {
                    let moves = keypad.get_path_to(button);
                    path2.extend(moves);
                }
                path = path2;
            }
            path.len() as u64 * sequence.numeric_component()
        })
        .sum()
}

pub fn part1(parsed: &Parsed) -> Solution {
    solve(parsed, 2)
}

pub fn part2(parsed: &Parsed) -> Solution {
    solve(parsed, 25)
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "029A
980A
179A
456A
379A
";

    const P1_SOLUTION: Solution = 126384;
    // const P2_SOLUTION: Solution = todo!();

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        // assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
