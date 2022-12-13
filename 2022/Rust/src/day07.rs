use std::num::NonZeroU64;

use id_tree::{Node, Tree, TreeBuilder};

// The only information we need about a node is its size (None if its a directory)
#[derive(Debug)]
pub struct FsNode(Option<NonZeroU64>);

pub fn parse(inpt: &str) -> id_tree::Tree<FsNode> {
    assert!(inpt.is_ascii());
    let mut filesystem: Tree<FsNode> = TreeBuilder::new()
        .with_root(Node::new(FsNode(None)))
        .with_node_capacity(inpt.len())
        .build();

    let mut current = filesystem.root_node_id().unwrap().clone();

    'l: for line in inpt.lines().skip(1) {
        let line = line.as_bytes();
        if line[0] == b'$' {
            // Its a command
            if line[2] == b'c' {
                // Its a `cd`: "$ cd <path>"
                match &line[5..] {
                    b".." => {
                        current = filesystem.get(&current).unwrap().parent().unwrap().clone();
                    }
                    _ => {
                        current = filesystem
                            .insert(
                                Node::new(FsNode(None)),
                                id_tree::InsertBehavior::UnderNode(&current),
                            )
                            .unwrap();
                    }
                }
            }
        } else {
            let space = memchr::memchr(b' ', line).unwrap();
            let size = match &line[..space] {
                b"dir" => {
                    continue 'l;
                }

                // Safety: x is a slice of an ascii string, so it is valid utf8
                x => unsafe { std::str::from_utf8_unchecked(x) }.parse().unwrap(),
            };

            filesystem
                .insert(
                    Node::new(FsNode(Some(size))),
                    id_tree::InsertBehavior::UnderNode(&current),
                )
                .unwrap();
        }
    }

    filesystem
}

/// Recursion :)
fn total_size(tree: &Tree<FsNode>, node: &Node<FsNode>) -> u64 {
    let mut total = node.data().0.map(NonZeroU64::get).unwrap_or(0);
    for child in node.children() {
        total += total_size(tree, tree.get(child).unwrap());
    }
    total
}

pub fn part1(tree: &Tree<FsNode>) -> u64 {
    tree.traverse_pre_order(tree.root_node_id().unwrap())
        .unwrap()
        .filter(|n| n.data().0.is_none())
        .map(|n| total_size(tree, n))
        .filter(|&s| s <= 100_000)
        .sum()
}

pub fn part2(tree: &Tree<FsNode>) -> u64 {
    const TOTAL_SIZE: u64 = 70_000_000;
    const UNUSED_REQUIRED: u64 = 30_000_000;

    let used_space = total_size(tree, tree.get(tree.root_node_id().unwrap()).unwrap());
    let free_space = TOTAL_SIZE - used_space;

    let space_to_free = UNUSED_REQUIRED - free_space;

    tree.traverse_pre_order(tree.root_node_id().unwrap())
        .unwrap()
        .filter(|n| n.data().0.is_none())
        .map(|n| total_size(tree, n))
        .filter(|&s| s >= space_to_free)
        .min()
        .unwrap()
}

pub fn run(input: &str) {
    let parsed = parse(input);
    // dbg!(&parsed);
    println!("Part 1: {}", part1(&parsed));
    println!("Part 2: {}", part2(&parsed));
}

#[cfg(test)]
mod tests {
    use super::*;

    const INPUT: &str = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k";

    #[test]
    fn test_part1() {
        assert_eq!(part1(&parse(INPUT)), 95437);
    }
    #[test]
    fn test_part2() {
        assert_eq!(part2(&parse(INPUT)), 24933642);
    }
}
