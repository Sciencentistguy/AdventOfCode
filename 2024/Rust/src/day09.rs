type Parsed = Vec<Option<usize>>;
type Solution = usize;

pub fn parse(input: &str) -> Parsed {
    let len = input.chars().filter_map(|x| x.to_digit(10)).sum::<u32>();
    let mut ret = Vec::with_capacity(len as usize);
    let mut file_id= 0;
    let mut is_file = true;
    for b in input.trim().as_bytes() {
        let b = b - b'0';
        if is_file {
            for _ in 0..b {
                ret.push(Some(file_id as usize));
            }
            file_id += 1;
        } else {
            for _ in 0..b {
                ret.push(None);
            }
        }
        is_file = !is_file;
    }
    ret
}

fn calculate_checksum(disk: &[Option<usize>]) -> usize {
    disk.iter()
        .enumerate()
        .filter_map(|(i, &block)| block.map(|id| i * id))
        .sum()
}

fn get_twice_mut<T>(slice: &mut [T], i: usize, j: usize) -> (&mut T, &mut T) {
    assert_ne!(i, j);
    let (a, b) = slice.split_at_mut(std::cmp::max(i, j));
    if i < j {
        (&mut a[i], &mut b[0])
    } else {
        (&mut b[0], &mut a[j])
    }
}

pub fn part1(parsed: &Parsed) -> Solution {
    let mut disk = parsed.clone();
    let mut files = Vec::with_capacity(disk.iter().filter_map(|&x| x).count());

    for i in 0..disk.len() {
        if let Some(file_id) = disk[i] {
            files.push((file_id, i));
        }
    }

    files.sort_by_key(|&(file_id, _,)| std::cmp::Reverse(file_id));

    for (_, src) in files {
        if let Some(dest) = find_free_space(&disk[..src], 1) {
            debug_assert_ne!(src, dest);
            let (src, dest) = get_twice_mut(&mut disk, src, dest);
            std::mem::swap(src, dest);
        }
    }

    calculate_checksum(&disk)

}

fn find_free_space(disk: &[Option<usize>], length: usize) -> Option<usize> {
    if length == 1 {
        return disk.iter().position(|&block| block.is_none());
    }

    let mut free_length = 0;
    for (i, &block) in disk.iter().enumerate() {
        if block.is_none() {
            free_length += 1;
            if free_length == length {
                return Some(i + 1 - length);
            }
        } else {
            free_length = 0;
        }
    }
    None
}

fn move_file(disk: &mut [Option<usize>], start: usize, length: usize, free_start: usize) {
    for i in 0..length {
        disk[free_start + i] = disk[start + i];
        disk[start + i] = None;
    }
}

pub fn part2(parsed: &Parsed) -> Solution {
    let mut disk = parsed.clone();
    let mut files = Vec::new();
    let mut i = 0;

    while i < disk.len() {
        if let Some(file_id) = disk[i] {
            let mut length = 0;
            while i + length < disk.len() && disk[i + length] == Some(file_id) {
                length += 1;
            }
            files.push((file_id, i, length));
            i += length;
        } else {
            i += 1;
        }
    }

    files.sort_by_key(|&(file_id, _, _)| std::cmp::Reverse(file_id));

    for (_, start, length) in files {
        if let Some(free_start) = find_free_space(&disk[..start], length) {
            move_file(&mut disk, start, length, free_start);
        }
    }

    calculate_checksum(&disk)
}

pub fn run(input: &str) {
    let parsed = parse(input);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "2333133121414131402";

    const P1_SOLUTION: Solution = 1928;
    const P2_SOLUTION: Solution = 2858;

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), P1_SOLUTION);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), P2_SOLUTION);
    }
}
