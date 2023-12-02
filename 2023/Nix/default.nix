with import ./common; rec {
  day01 = import ./day01;
  all = let
    sentences = lib.lists.imap1 (i: a: "Day ${toString i}: Part 1: ${toString a.part1}; Part 2: ${toString a.part2}") [day01];
  in
    lib.strings.intersperse "\n" sentences;
}
