with import ./common; rec {
  days = {
    day01 = import ./day01;
    day02 = import ./day02;
  };
  all = let
    sentences = lib.lists.imap1 (i: a: "Day ${toString i}: Part 1: ${toString a.part1}; Part 2: ${toString a.part2}") (builtins.attrValues days);
  in
    sentences;
}
